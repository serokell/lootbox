{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }: with pkgs;

let
  jemalloc450 = import ./jemalloc450.nix { inherit pkgs; };
  rocksdb = pkgs.rocksdb.override { jemalloc = jemalloc450; };
in

haskell.lib.buildStackProject {
  inherit ghc;
  name = "default-stack-shell";
  buildInputs = with pkgs; [
    rocksdb
    zlib
  ];
  
  nativeBuildInputs = with pkgs; [
    git    # HACK: Provide access to Git inside Nix env that is executed by Stack
  ];

  LANG = "en_US.UTF-8";    # HACK: Haddock fails without this inside pure Nix environment
}
