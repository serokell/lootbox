with import <nixpkgs> { };
let hspkgs = haskell.packages.ghc865; in
haskell.lib.buildStackProject {
   ghc = hspkgs.ghc;
   name = "lootbox";
   buildInputs = [ zlib gmp git icu zeromq ];
}

