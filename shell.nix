let
  pkgs = import <nixpkgs> { };
  hsPkgs = import ./default.nix;
in
hsPkgs.shellFor {
  packages = ps: [ ps.esa-space-protocols ];

  # Builds a Hoogle documentation index of all dependencies,
  # and provides a "hoogle" command to search the index.
  # withHoogle = true;
  withHoogle = false;

  # Haskell tools compiled using haskell.nix
  # See overlays/tools.nix for more details
  # tools = { cabal = "3.2.0.0"; hlint = "2.2.11"; };
  # tools = { ghcide = "0.2.0"; };

  # Other packages (compiled using nixpkgs haskell infrastructure)
  buildInputs = [
    pkgs.cabal-install
    pkgs.stack
    # pkgs.haskell.packages.ghc865.ghcide
  ];

  # Prevents cabal from choosing alternate plans, so that
  # *all* dependencies are provided by Nix.
  # exactDeps = true;
}
