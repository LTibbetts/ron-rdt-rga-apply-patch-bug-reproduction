{ pkgs, ... }:

{
  # Haskell development environment
  languages.haskell = {
    enable = true;
    package = pkgs.haskell.compiler.ghc96;
  };

  packages = [
    pkgs.cabal-install
    pkgs.zlib
  ];

  env.LD_LIBRARY_PATH = "${pkgs.zlib}/lib";

  enterShell = ''
    echo "RON RGA bug demonstration project"
    echo "  ghc version: $(ghc --version)"
    echo "  cabal version: $(cabal --version | head -1)"
    echo ""
    echo "Run:  cabal test"
  '';
}
