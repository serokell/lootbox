with import <nixpkgs> { };
let hspkgs = haskell.packages.ghc844; in
haskell.lib.buildStackProject {
   ghc = hspkgs.ghc;
   name = "lootbox";
   buildInputs = [ zlib gmp git icu zeromq ];
}

