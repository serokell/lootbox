{ _expose ? false }:

let
  sources = import ./nix/sources.nix;
in

let
  pkgs = import sources.nixpkgs {
    inherit (import sources."haskell.nix") config overlays;
  };
  project = pkgs.haskell-nix.stackProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
  };
in

if _expose
then project
else pkgs.haskell-nix.haskellLib.selectLocalPackages project
