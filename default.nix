let
  haskell-nix-src = builtins.fetchGit {
    url = "https://github.com/input-output-hk/haskell.nix";
    rev = "1383a63ff6005bba577635b9e054375dc7a296d0";
  };
  haskellNix = (import haskell-nix-src) { };

  # haskell.nix provides access to the nixpkgs pins which are used by our CI, hence
  # you will be more likely to get cache hits when using these.
  # But you can also just use your own, e.g. '<nixpkgs>'
  nixpkgsSrc = haskellNix.sources.nixpkgs-2003;

  # haskell.nix provides some arguments to be passed to nixpkgs, including some patches
  # and also the haskell.nix functionality itself as an overlay.
  nixpkgsArgs = haskellNix.nixpkgsArgs;

  # import nixpkgs with overlays
  # pkgs = import nixpkgs nixpkgsArgs;
  pkgs = import nixpkgsSrc nixpkgsArgs;
in
pkgs.haskell-nix.stackProject {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "AURIS";
    src = ./.;
  };
  modules = [{ }];
}
