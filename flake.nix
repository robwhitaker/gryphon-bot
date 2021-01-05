{
  description = "Discord bot for the Habitican Evolution party on Habitica";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskell.packages.ghc8103.override {
          overrides = hsPackagesNew: hsPackagesOld:
            let
              hsOverride = pkg: hsPackagesNew.callPackage (./nix-overrides + "/${pkg}.nix") {};
            in {
              calamity = hsOverride "calamity";
              generic-override-aeson = hsOverride "generic-override-aeson";
              stm-containers = hsOverride "stm-containers";
              haskell-src = hsOverride "haskell-src";
              stm-hamt = hsOverride "stm-hamt";
              primitive-extras = hsOverride "primitive-extras";
              happy = hsOverride "happy";

              gryphon-bot = hsPackagesNew.callPackage ./. {};
            };
        };
        botDeps = haskellPackages.gryphon-bot.getBuildInputs.haskellBuildInputs;
        customGhc = haskellPackages.ghcWithHoogle (_: botDeps);
      in
      rec {
        packages = flake-utils.lib.flattenTree {
          gryphon-bot = haskellPackages.gryphon-bot;
        };
        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            customGhc

            cabal2nix
            cabal-install

            floskell
            ghcid
            hlint
          ];
        };
        defaultPackage = packages.gryphon-bot;
      }
    );
}
