{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Loot.Network.TypeLevel
       ( toStringList
       , UniqSymbols
       ) where

import Universum

import GHC.TypeLits (ErrorMessage ((:<>:), ShowType, Text), KnownSymbol, Symbol, TypeError,
                     symbolVal)


-- | Allows to lower a type-level list of string to terms.
class SymbolList (a :: [Symbol]) where
    toStringList :: IsString s => Proxy a -> [s]

instance SymbolList '[] where
    toStringList _ = []

instance forall s ss. (KnownSymbol s, SymbolList ss) => SymbolList (s ': ss) where
    toStringList _ = fromString (symbolVal (Proxy :: Proxy s)) : toStringList (Proxy :: Proxy ss)


-- | Constraint that guarantees that types in the list do not repeet.
type family UniqTypes ss :: Constraint where
    UniqTypes '[] = ()
    UniqTypes (s ': ss) = (NoType s ss, UniqTypes ss)

type family NoType (s :: k) (ss :: [k]) :: Constraint where
    NoType _ '[] = ()
    NoType s (s ': _) = TypeError ('Text "Duplicate item: " ':<>: 'ShowType s)
    NoType s (_ ': ss) = NoType s ss


type UniqSymbols ss = (SymbolList ss, UniqTypes ss)
