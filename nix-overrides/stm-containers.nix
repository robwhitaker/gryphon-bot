{ mkDerivation, base, deferred-folds, focus, foldl, free, hashable
, HTF, list-t, QuickCheck, quickcheck-text, rerebase, stdenv
, stm-hamt, transformers
}:
mkDerivation {
  pname = "stm-containers";
  version = "1.2";
  sha256 = "6c4d98b6a3182fa0dd99235cea1aa95a3c876f8be5cbb78f7700a17d64b7177a";
  libraryHaskellDepends = [
    base deferred-folds focus hashable list-t stm-hamt transformers
  ];
  testHaskellDepends = [
    deferred-folds focus foldl free HTF list-t QuickCheck
    quickcheck-text rerebase
  ];
  doCheck = false;
  homepage = "https://github.com/nikita-volkov/stm-containers";
  description = "Containers for STM";
  license = stdenv.lib.licenses.mit;
}
