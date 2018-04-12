with import <nixpkgs> { };
let hspkgs = haskell.packages.ghc822; in
haskell.lib.buildStackProject {
   ghc = hspkgs.ghc;
   name = "lootbox";
   buildInputs = [ zlib gmp git icu ];
}

