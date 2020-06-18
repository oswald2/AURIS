let
  haskell-nix-src = (import <nixpkgs> { }).fetchgit {
    url = "https://github.com/input-output-hk/haskell.nix";
    rev = "6cf92c4e983f346123e41dac237ca2cc5c4b9ff6";
    sha256 = "0q6bpfyqpimkd9hh727cwd6qf1jc5fqvl3mrgw0im5dja6djvvg9";
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

in pkgs.haskell-nix.stackProject {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "AURIS";
    src = ./.;
  };
  modules = [{
    # Bundled FLTK breaks when configured without "--libdir" (custom Setup.hs)
    packages.fltkhs.flags.bundled = false;
    # fltkhs is not correctly depended upon in fltkhs.cabal
    packages.fltkhs.components.library.libs = [ pkgs.fltk14 pkgs.libjpeg ];
    packages.fltkhs.components.library.build-tools =
      [ pkgs.fltk14 pkgs.autoconf ];
    # NOTE(SN): these libs might only be required when linking exes
    packages.Chart-fltkhs.components.library.libs = [
      pkgs.fontconfig
      pkgs.libpng
      pkgs.xorg.libX11
      pkgs.xorg.libXext
      pkgs.xorg.libXfixes
      pkgs.xorg.libXft
      pkgs.xorg.libXrender
      pkgs.zlib
    ];
  }];
}
