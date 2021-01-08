{ mkDerivation, aeson, base, calamity, containers, di, di-polysemy
, optics, polysemy, relude, req, servant-server, stdenv, time
, unagi-chan, uuid
}:
mkDerivation {
  pname = "gryphon-bot";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base calamity containers di di-polysemy optics polysemy
    relude req servant-server time unagi-chan uuid
  ];
  description = "Discord bot for the Habitican Evolution party on Habitica";
  license = stdenv.lib.licenses.bsd3;
}
