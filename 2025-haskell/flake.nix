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

            # Useful tools
            pkgs.zlib
            pkgs.pkg-config
            pkgs.pcre
          ];

          inputsFrom = [
            self.packages.${system}.${packageName}.env
          ];

          shellHook = ''
            echo "Advent of Code 2025 - Haskell Development Environment"
            echo "GHC version: $(ghc --version)"
            echo "Cabal version: $(cabal --version | head -n1)"
            echo ""
            echo "Quick start:"
            echo "  cabal build              - Build the project"
            echo "  cabal run aoc2025        - Run the main executable"
            echo "  cabal test               - Run tests"
            echo "  cabal repl               - Start GHCi REPL"
            echo "  ghcid                    - Auto-reload on file changes"
            echo "  ./format.sh              - Format all Haskell files with ormolu"
          '';
        };
      }
    );
}
