{ mkDerivation, array, base, happy, pretty, stdenv, syb }:
mkDerivation {
  pname = "haskell-src";
  version = "1.0.3.1";
  sha256 = "869cc710004c2161470d8a788dab96d2cff054fa106c301be6689109f57e5132";
  revision = "1";
  editedCabalFile = "1li6czcs54wnij6qnvpx6f66iiw023pggb3zl3jvp74qqflcf5sg";
  libraryHaskellDepends = [ array base pretty syb ];
  libraryToolDepends = [ happy ];
  jailbreak = true;
  description = "Support for manipulating Haskell source code";
  license = stdenv.lib.licenses.bsd3;
}
