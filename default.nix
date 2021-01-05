{ mkDerivation, aeson, base, calamity, di, di-polysemy, optics
, polysemy, relude, req, servant, stdenv, unagi-chan
}:
mkDerivation {
  pname = "gryphon-bot";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base calamity di di-polysemy optics polysemy relude req
    servant unagi-chan
  ];
  description = "Discord bot for the Habitican Evolution party on Habitica";
  license = stdenv.lib.licenses.bsd3;
}
