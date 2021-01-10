{ mkDerivation, aeson, base, calamity, containers, di, di-core
, di-polysemy, http-client, optics, polysemy, polysemy-plugin
, relude, req, servant-server, stdenv, time, unagi-chan, uuid, warp
}:
mkDerivation {
  pname = "gryphon-bot";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base calamity containers di di-core di-polysemy http-client
    optics polysemy polysemy-plugin relude req servant-server time
    unagi-chan uuid warp
  ];
  description = "Discord bot for the Habitican Evolution party on Habitica";
  license = stdenv.lib.licenses.bsd3;
}
