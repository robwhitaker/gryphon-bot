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
        haskellPackages = pkgs.haskell.packages.ghc92.override {
          overrides = final: prev: {
            # Add our bot to haskellPackages
            gryphon-bot = final.callCabal2nix "gryphon-bot" self { };
          };
        };
        customGhc = haskellPackages.ghcWithHoogle (_:
          haskellPackages.gryphon-bot.getBuildInputs.haskellBuildInputs
        );
      in
      {
        packages.gryphon-bot = haskellPackages.gryphon-bot;
        defaultPackage = self.packages.${system}.gryphon-bot;

        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            # Haskell tooling
            customGhc
            cabal2nix
            cabal-install
            haskell-language-server

            # Nix formatter
            pkgs.nixpkgs-fmt

            # Needed for cabal to build the project
            pkgs.zlib
          ];
        };
      }
    );
}
