{-# LANGUAGE DataKinds #-}

-- | Capabilities-based RIO.
module Loot.Crio
       ( Crio

       , startCrio
       , withComponent
       , withComponents

       , CapImpl (CapImpl)

       , Component (..)

       , withCapImpl

       , HasCap
       , HasCaps
       , HasNoCap

       , Context
       , HasContext
       , askContext
       , localContext
       ) where

import Monad.Capabilities (CapImpl (CapImpl), CapsT (..), Context, HasCap, HasCaps, HasContext,
                           HasNoCap, askContext, emptyCaps, localContext, withCap)

import Loot.Crio.Internal (Component (..), Crio, withComponent, withComponents)


-- | Initialise a Crio computation with no capabilities.
--
-- This function is a way to get a clean system with no components running.
-- In other words, it moves you from 'IO' to a 'Crio' computation in which
-- you can use 'withComponents' or 'withComponent' to add more components.
startCrio :: Crio '[] a -> IO a
startCrio = flip runCapsT emptyCaps

withCapImpl
    :: ( HasCap cap caps
       , Typeable cap
       )
    => (cap (Crio caps) -> Crio caps a)
    -> Crio caps a
withCapImpl = withCap
