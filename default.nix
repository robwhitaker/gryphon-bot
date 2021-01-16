{ mkDerivation, aeson, async, base, calamity, containers, di
, di-core, di-polysemy, http-client, optics, polysemy
, polysemy-plugin, relude, req, servant-server, stdenv, text, time
, unagi-chan, uuid, warp
}:
mkDerivation {
  pname = "gryphon-bot";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async base calamity containers di di-core di-polysemy
    http-client optics polysemy polysemy-plugin relude req
    servant-server text time unagi-chan uuid warp
  ];
  description = "Discord bot for the Habitican Evolution party on Habitica";
  license = stdenv.lib.licenses.bsd3;
}
