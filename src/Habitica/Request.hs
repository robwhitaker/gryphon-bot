{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Habitica.Request
  (
     -- * Habitica authentication
    HabiticaAuthHeaders
  , XClient
  , habiticaHeaders
  , readApiKeyFromEnv
  , xClient
    -- * Making an API request
  , habiticaRequest
    -- * Ignoring responses
  , IgnoreData
    -- * Handling an API response
  , HabiticaError(..)
  , HabiticaResponse
  , responseBody
  , responseCookieJar
  , responseHeader
  , responseRateLimit
  , responseStatusCode
  , responseStatusMessage
    -- * HabiticaApi effect interpreter
  , HabiticaApi
  , runHabiticaApi ) where

import qualified System.Environment  as Env

import           Data.Aeson          ( (.:), FromJSON )
import qualified Data.Aeson          as Aeson
import           Data.Time           ( NominalDiffTime, UTCTime )
import qualified Data.Time           as Time
import qualified Data.Time.Format    as TimeFmt
import           Data.UUID           ( UUID )
import qualified Data.UUID           as UUID

import qualified Network.HTTP.Client as HttpClient
import           Network.HTTP.Req    ( (/:), AllowsBody, HttpBody, HttpBodyAllowed
                                     , HttpConfig, HttpException(VanillaHttpException)
                                     , HttpMethod, JsonResponse, Option, ProvidesBody
                                     , Scheme(..), Url, httpConfigCheckResponse )
import qualified Network.HTTP.Req    as Req

import qualified Optics

import           Polysemy            ( Member, Members, Sem )
import qualified Polysemy            as P
import           Polysemy.Embed      ( Embed )
import           Polysemy.Error      ( Error )
import qualified Polysemy.Error      as P
import qualified Polysemy.Internal   as P ( send )

import           Prelude             hiding ( Option )

newtype HabiticaAuthHeaders =
    HabiticaAuthHeaders (Option 'Https)

newtype XClient = XClient (UUID, Text)

newtype ApiKey = ApiKey UUID

readApiKeyFromEnv :: String -> IO (Maybe ApiKey)
readApiKeyFromEnv envVar = do
    mbKey <- Env.lookupEnv envVar
    return $ mbKey >>= fmap ApiKey . UUID.fromString

xClient :: UUID -> Text -> XClient
xClient maintainerId appName = XClient (maintainerId, appName)

toClientString :: XClient -> ByteString
toClientString (XClient (maintainerId,appName)) =
    UUID.toASCIIBytes maintainerId <> "-" <> encodeUtf8 appName

habiticaHeaders :: UUID -> ApiKey -> XClient -> HabiticaAuthHeaders
habiticaHeaders userId (ApiKey apiKey) xClient' =
    HabiticaAuthHeaders
    $ mconcat
        [ Req.header "x-api-user" (UUID.toASCIIBytes userId)
        , Req.header "x-api-key" (UUID.toASCIIBytes apiKey)
        , Req.header "x-client" (toClientString xClient')
        ]

data HabiticaError = HabiticaError
    { error   :: Text
    , message :: Text
    }
  deriving stock ( Show, Eq, Generic )
  deriving anyclass ( FromJSON )

Optics.makeFieldLabelsWith Optics.noPrefixFieldLabels ''HabiticaError

newtype HabiticaResBody a = HabiticaResBody
    { unHabiticaResBody :: Either HabiticaError a
    }
  deriving stock ( Show, Eq )

instance FromJSON a => FromJSON (HabiticaResBody a) where
    parseJSON = Aeson.withObject "HabiticaResBody" $ \o -> do
        success <- o .: "success"
        HabiticaResBody
            <$> if success
                then Right <$> o .: "data"
                else Left <$> Aeson.parseJSON (Aeson.Object o)

type HabiticaResponse a = JsonResponse (HabiticaResBody a)

data IgnoreData = IgnoreData
  deriving stock ( Show )

instance FromJSON IgnoreData where
    parseJSON = const (pure IgnoreData)

data HabiticaRateLimit = HabiticaRateLimit
    { limit      :: Int
    , remaining  :: Int
    , reset      :: UTCTime
    , retryAfter :: Maybe NominalDiffTime
    }
  deriving stock ( Show, Eq )

Optics.makeFieldLabelsWith Optics.noPrefixFieldLabels ''HabiticaRateLimit

responseBody :: FromJSON a => HabiticaResponse a -> Either HabiticaError a
responseBody = unHabiticaResBody . Req.responseBody

responseStatusCode :: FromJSON a => HabiticaResponse a -> Int
responseStatusCode = Req.responseStatusCode

responseStatusMessage :: FromJSON a => HabiticaResponse a -> ByteString
responseStatusMessage = Req.responseStatusMessage

responseHeader :: FromJSON a => HabiticaResponse a -> ByteString -> Maybe ByteString
responseHeader = Req.responseHeader

responseCookieJar :: FromJSON a => HabiticaResponse a -> HttpClient.CookieJar
responseCookieJar = Req.responseCookieJar

responseRateLimit :: FromJSON a => HabiticaResponse a -> Maybe HabiticaRateLimit
responseRateLimit res = do
    limit <- responseHeader res "X-RateLimit-Limit" >>= readBS
    remaining <- responseHeader res "X-RateLimit-Remaining" >>= readBS
    reset <- responseHeader res "X-RateLimit-Reset"
        >>=
        -- Habitica sends this in the format output by JavaScript's
        -- `new Date()`, so it has to be parsed manually
        TimeFmt.parseTimeM True TimeFmt.defaultTimeLocale "%a %b %e %Y %H:%M:%S %Z%z"
        . takeWhile (/= '(')
        . toString @Text
        . decodeUtf8
    let retryAfter =
            fmap Time.secondsToNominalDiffTime
            $ responseHeader res "Retry-After" >>= readBS
    return
        HabiticaRateLimit
        { .. }
  where
    readBS :: Read a => ByteString -> Maybe a
    readBS = readMaybe . toString @Text . decodeUtf8

type ReqConstraints a method body =
    ( FromJSON a
    , HttpMethod method
    , HttpBody body
    , HttpBodyAllowed (AllowsBody method) (ProvidesBody body))

data HabiticaApi m a where
    HabiticaRequest :: ReqConstraints a method body
        => method
        -> Url 'Https
        -> body
        -> Option 'Https
        -> HabiticaApi m (HabiticaResponse a)

habiticaRequest
    :: (Member HabiticaApi r, ReqConstraints a method body)
    => method
    -> [Text]
    -> body
    -> Option 'Https
    -> Sem r (HabiticaResponse a)
habiticaRequest method endpoint body opts =
    P.send $ HabiticaRequest method url body opts
  where
    apiBaseUrl = Req.https "habitica.com" /: "api" /: "v3"

    url = foldl' (/:) apiBaseUrl endpoint

runHabiticaApi
    :: Members '[Error HttpException, Embed IO] r
    => HabiticaAuthHeaders
    -> Sem (HabiticaApi ': r) a
    -> Sem r a
runHabiticaApi (HabiticaAuthHeaders headers) = P.interpret $ \case
    HabiticaRequest method url body opts -> P.mapError hideAPIKeyInExceptions
        $ P.fromException
        $ Req.runReq httpConfig
        $ Req.req method url body Req.jsonResponse (headers <> opts)
  where
    httpConfig :: HttpConfig
    httpConfig =
        Req.defaultHttpConfig
        { httpConfigCheckResponse = \_ _ _ -> Nothing }

    -- TODO: Might still be possible to see the headers in the error field
    --       of HttpExceptionRequest: InvalidHeader, InvalidRequestHeader
    hideAPIKeyInExceptions :: HttpException -> HttpException
    hideAPIKeyInExceptions = \case
        VanillaHttpException (HttpClient.HttpExceptionRequest request err) ->
            let requestApiMasked =
                    request
                    { HttpClient.requestHeaders =
                          fmap (\header@(headerName,_) ->
                                if headerName == "x-api-key"
                                then (headerName, "(hidden)")
                                else header) (HttpClient.requestHeaders request) }
            in VanillaHttpException
                   (HttpClient.HttpExceptionRequest requestApiMasked err)

        otherException -> otherException

