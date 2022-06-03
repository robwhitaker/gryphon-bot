{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Habitica.Request
  ( -- * Habitica authentication
    HabiticaAuthHeaders,
    XClient,
    habiticaHeaders,
    readApiKeyFromEnv,
    xClient,

    -- * Making an API request
    habiticaRequest,
    retry,

    -- * Ignoring responses
    IgnoreData,

    -- * Handling an API response
    HabiticaResponse,
    responseBody,
    responseCookieJar,
    responseHeader,
    responseRateLimit,
    responseRateLimitError,
    responseStatusCode,
    responseStatusMessage,

    -- * Handling errors from the API
    HabiticaApiError,
    HabiticaRateLimitError,
    HabiticaRequestError (..),
    prettyPrintError,
    urlFromError,

    -- * HabiticaApi effect interpreter
    HabiticaApi,
    runHabiticaApi,
  )
where

import Calamity.Types.LogEff (LogEff)
import qualified Control.Concurrent as Concurrent
import Data.Aeson (FromJSON, (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Fixed as Fixed
import Data.Time (NominalDiffTime, UTCTime)
import qualified Data.Time as Time
import qualified Data.Time.Format as TimeFmt
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified DiPolysemy as DiP
import qualified Network.HTTP.Client as HttpClient
import Network.HTTP.Req
  ( AllowsBody,
    HttpBody,
    HttpBodyAllowed,
    HttpConfig,
    HttpException (VanillaHttpException),
    HttpMethod,
    JsonResponse,
    Option,
    ProvidesBody,
    Scheme (..),
    Url,
    httpConfigCheckResponse,
    (/:),
  )
import qualified Network.HTTP.Req as Req
import Optics ((^.))
import qualified Optics
import Polysemy (Member, Members, Sem)
import qualified Polysemy as P
import Polysemy.Embed (Embed)
import Polysemy.Error (Error)
import qualified Polysemy.Error as P
import qualified Polysemy.Internal as P (send)
import qualified System.Environment as Env
import Prelude hiding (Option)

newtype HabiticaAuthHeaders = HabiticaAuthHeaders (Option 'Https)

newtype XClient = XClient (UUID, Text)

newtype ApiKey = ApiKey UUID

readApiKeyFromEnv :: String -> IO (Either Text ApiKey)
readApiKeyFromEnv envVar = do
  mbKey <- Env.lookupEnv envVar
  case mbKey of
    Nothing ->
      pure $ Left $ "Could not find " <> fromString envVar <> " in environment"
    Just keyStr -> case UUID.fromString keyStr of
      Nothing -> pure $ Left "Unable to parse Habitica API key"
      Just key -> pure $ Right $ ApiKey key

xClient :: UUID -> Text -> XClient
xClient maintainerId appName = XClient (maintainerId, appName)

toClientString :: XClient -> ByteString
toClientString (XClient (maintainerId, appName)) =
  UUID.toASCIIBytes maintainerId <> "-" <> encodeUtf8 appName

habiticaHeaders :: UUID -> ApiKey -> XClient -> HabiticaAuthHeaders
habiticaHeaders userId (ApiKey apiKey) xClient' =
  HabiticaAuthHeaders $
    mconcat
      [ Req.header "x-api-user" (UUID.toASCIIBytes userId),
        Req.header "x-api-key" (UUID.toASCIIBytes apiKey),
        Req.header "x-client" (toClientString xClient')
      ]

data HabiticaApiError = HabiticaApiError
  { error :: Text,
    message :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

Optics.makeFieldLabelsWith Optics.noPrefixFieldLabels ''HabiticaApiError

newtype RawHabiticaResBody a = RawHabiticaResBody
  { unRawHabiticaResBody :: Either HabiticaApiError a
  }
  deriving stock (Show, Eq)

instance FromJSON a => FromJSON (RawHabiticaResBody a) where
  parseJSON = Aeson.withObject "RawHabiticaResBody" $ \o -> do
    success <- o .: "success"
    RawHabiticaResBody
      <$> if success
        then Right <$> o .: "data"
        else Left <$> Aeson.parseJSON (Aeson.Object o)

type RawHabiticaResponse a = JsonResponse (RawHabiticaResBody a)

data HabiticaResponse a = HabiticaResponse
  { rawResponse :: RawHabiticaResponse a,
    bodyOfResponse :: a
  }

data IgnoreData = IgnoreData
  deriving stock (Show)

instance FromJSON IgnoreData where
  parseJSON = const (pure IgnoreData)

data HabiticaRateLimit = HabiticaRateLimit
  { limit :: Int,
    remaining :: Int,
    reset :: UTCTime,
    retryAfter :: Maybe NominalDiffTime
  }
  deriving stock (Show, Eq)

Optics.makeFieldLabelsWith Optics.noPrefixFieldLabels ''HabiticaRateLimit

data HabiticaRateLimitError = HabiticaRateLimitError
  { limit :: Int,
    remaining :: Int,
    reset :: UTCTime,
    retryAfter :: NominalDiffTime
  }
  deriving stock (Show, Eq)

Optics.makeFieldLabelsWith Optics.noPrefixFieldLabels ''HabiticaRateLimitError

data HabiticaRequestError
  = HttpError (Url 'Https) HttpException
  | RateLimitError (Url 'Https) HabiticaRateLimitError
  | ApiError (Url 'Https) HabiticaApiError
  deriving stock (Show)

urlFromError :: HabiticaRequestError -> Url 'Https
urlFromError = \case
  HttpError url _ -> url
  RateLimitError url _ -> url
  ApiError url _ -> url

prettyPrintError :: IsString a => HabiticaRequestError -> a
prettyPrintError = \case
  HttpError _ err ->
    fromString $ "Habitica API request failed with HTTP error: " <> show err
  RateLimitError _ err ->
    fromString $ "Habitica API request failed due to rate limiting: " <> show err
  ApiError _ err ->
    fromString $
      "Habitica API request failed with HabiticaError: "
        <> show (err ^. #error <> ": " <> err ^. #message)

responseBody :: HabiticaResponse a -> a
responseBody = bodyOfResponse

responseStatusCode :: FromJSON a => HabiticaResponse a -> Int
responseStatusCode = Req.responseStatusCode . rawResponse

responseStatusMessage :: FromJSON a => HabiticaResponse a -> ByteString
responseStatusMessage = Req.responseStatusMessage . rawResponse

responseHeader :: FromJSON a => HabiticaResponse a -> ByteString -> Maybe ByteString
responseHeader = Req.responseHeader . rawResponse

responseCookieJar :: FromJSON a => HabiticaResponse a -> HttpClient.CookieJar
responseCookieJar = Req.responseCookieJar . rawResponse

responseRateLimit' :: FromJSON a => RawHabiticaResponse a -> Maybe HabiticaRateLimit
responseRateLimit' res = do
  limit <- Req.responseHeader res "X-RateLimit-Limit" >>= readBS
  remaining <- Req.responseHeader res "X-RateLimit-Remaining" >>= readBS
  reset <-
    Req.responseHeader res "X-RateLimit-Reset"
      >>=
      -- Habitica sends this in the format output by JavaScript's
      -- `new Date()`, so it has to be parsed manually
      TimeFmt.parseTimeM True TimeFmt.defaultTimeLocale "%a %b %e %Y %H:%M:%S %Z%z"
        . takeWhile (/= '(')
        . toString @Text
        . decodeUtf8
  let retryAfter =
        fmap Time.secondsToNominalDiffTime $
          Req.responseHeader res "Retry-After" >>= readBS
  return
    HabiticaRateLimit
      { ..
      }
  where
    readBS :: Read a => ByteString -> Maybe a
    readBS = readMaybe . toString @Text . decodeUtf8

responseRateLimit :: FromJSON a => HabiticaResponse a -> Maybe HabiticaRateLimit
responseRateLimit = responseRateLimit' . rawResponse

responseRateLimitError' ::
  FromJSON a => RawHabiticaResponse a -> Maybe HabiticaRateLimitError
responseRateLimitError' res = case responseRateLimit' res of
  Nothing -> Nothing
  Just rateLimit ->
    HabiticaRateLimitError
      (rateLimit ^. #limit)
      (rateLimit ^. #remaining)
      (rateLimit ^. #reset)
      <$> (rateLimit ^. #retryAfter)

responseRateLimitError ::
  FromJSON a => HabiticaResponse a -> Maybe HabiticaRateLimitError
responseRateLimitError = responseRateLimitError' . rawResponse

type ReqConstraints a method body =
  ( FromJSON a,
    HttpMethod method,
    HttpBody body,
    HttpBodyAllowed (AllowsBody method) (ProvidesBody body)
  )

data HabiticaApi m a where
  HabiticaRequest ::
    ReqConstraints a method body =>
    method ->
    Url 'Https ->
    body ->
    Option 'Https ->
    HabiticaApi m (HabiticaResponse a)

habiticaRequest ::
  (Member HabiticaApi r, ReqConstraints a method body) =>
  method ->
  [Text] ->
  body ->
  Option 'Https ->
  Sem r (HabiticaResponse a)
habiticaRequest method endpoint body opts =
  P.send $ HabiticaRequest method url body opts
  where
    apiBaseUrl = Req.https "habitica.com" /: "api" /: "v3"

    url = foldl' (/:) apiBaseUrl endpoint

retry ::
  forall a r.
  ( FromJSON a,
    Members '[LogEff, HabiticaApi, Error HabiticaRequestError, Embed IO] r
  ) =>
  Int ->
  Sem r (HabiticaResponse a) ->
  Sem r (HabiticaResponse a)
retry
  retriesLeft
  act
    | retriesLeft <= 0 = act
    | otherwise = do
        act `P.catch` \err ->
          DiP.attr "route" (Req.renderUrl $ urlFromError err) $ do
            case err of
              HttpError _ _ -> handleError (seconds 5) err
              RateLimitError _ rateLimit ->
                let delay =
                      diffTimeToMicroSeconds (rateLimit ^. #retryAfter)
                        + seconds 5
                 in handleError delay err
              ApiError _ _ -> P.throw err
    where
      seconds :: Int -> Int
      seconds s = s * 1000000

      diffTimeToMicroSeconds :: NominalDiffTime -> Int
      diffTimeToMicroSeconds diffTime = seconds $ fromInteger (Fixed.resolution picos)
        where
          picos = Time.nominalDiffTimeToSeconds diffTime

      handleError :: Int -> HabiticaRequestError -> Sem r (HabiticaResponse a)
      handleError delay err = do
        DiP.info_ $
          "An error occurred while running a Habitica request. Retries left: "
            <> show retriesLeft
            <> ". Error was: "
            <> show err
        when (delay > 0) $ P.embed $ Concurrent.threadDelay delay
        retry (retriesLeft - 1) act

runHabiticaApi ::
  Members '[LogEff, Error HabiticaRequestError, Embed IO] r =>
  HabiticaAuthHeaders ->
  Sem (HabiticaApi ': r) a ->
  Sem r a
runHabiticaApi (HabiticaAuthHeaders headers) = P.interpret $ \case
  HabiticaRequest method url body opts -> DiP.attr "route" (Req.renderUrl url) $ do
    let request = Req.req method url body Req.jsonResponse (headers <> opts)
    res <-
      ( Req.runReq httpConfig request
          & P.fromExceptionVia (HttpError url . hideAPIKeyInExceptions)
        )
        `P.catch` \err -> do
          DiP.debug_ $ prettyPrintError err
          P.throw err

    case responseRateLimitError' res of
      Just rateLimited -> do
        let err = RateLimitError url rateLimited
        DiP.debug_ $ prettyPrintError err
        P.throw err
      Nothing -> case unRawHabiticaResBody (Req.responseBody res) of
        Left apiError -> do
          let err = ApiError url apiError
          DiP.debug_ $ prettyPrintError err
          P.throw err
        Right resBody -> do
          DiP.debug_ "Success"
          pure $ HabiticaResponse res resBody
  where
    httpConfig :: HttpConfig
    httpConfig =
      Req.defaultHttpConfig
        { httpConfigCheckResponse = \_ _ _ -> Nothing
        }

    -- TODO: Might still be possible to see the headers in the error field
    --       of HttpExceptionRequest: InvalidHeader, InvalidRequestHeader
    hideAPIKeyInExceptions :: HttpException -> HttpException
    hideAPIKeyInExceptions = \case
      VanillaHttpException (HttpClient.HttpExceptionRequest request err) ->
        let requestApiMasked =
              request
                { HttpClient.requestHeaders =
                    fmap
                      ( \header@(headerName, _) ->
                          if headerName == "x-api-key"
                            then (headerName, "(hidden)")
                            else header
                      )
                      (HttpClient.requestHeaders request)
                }
         in VanillaHttpException
              (HttpClient.HttpExceptionRequest requestApiMasked err)
      otherException -> otherException
