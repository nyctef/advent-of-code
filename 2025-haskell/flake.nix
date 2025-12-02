{
  description = "Advent of Code 2025 - Haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskell.packages.ghc9122;

        packageName = "aoc2025";
      in
      {
        packages.${packageName} =
          pkgs.haskell.lib.overrideCabal
            (haskellPackages.callCabal2nix packageName ./. { })
            (oldAttrs: {
              # Limit GHC to 2 parallel jobs instead of default
              # this was an attempt to fix GHC getting OOM-killed in WSL on some machines
              configureFlags = (oldAttrs.configureFlags or []) ++ [
                "--ghc-option=-j2"
                "--ghc-option=+RTS"
                "--ghc-option=-A128M"  # Allocation area
                "--ghc-option=-M2G"    # Max heap size per process
                "--ghc-option=-RTS"
              ];
            });

        packages.default = self.packages.${system}.${packageName};

        devShells.default = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            ghc
            cabal-install
            haskell-language-server
            ghcid
            ormolu
            hlint

            pkgs.zlib
            pkgs.pkg-config
            pkgs.pcre
          ];

          inputsFrom = [
            self.packages.${system}.${packageName}.env
          ];

          shellHook = ''
            echo "Quick start:"
            echo "  cabal run                - Run puzzles with actual inputs"
            echo "  cabal test               - Run tests with example inputs"
            echo "  cabal repl               - Start a repl where per-day code can be imported"
            echo "  ghcid                    - Show build output and auto-reload on file changes"
            echo "  ./format.sh              - Format all Haskell files with ormolu"
            echo "  ./lint.sh                - Lint all Haskell files with hlint"
          '';
        };
      }
    );
}
