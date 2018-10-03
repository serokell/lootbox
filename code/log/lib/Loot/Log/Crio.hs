{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Logging interface for use with the @loot-crio@ library.
module Loot.Log.Crio
       ( mkLogging

       , module Loot.Log
       ) where

import Loot.Crio (Component (..), Crio, askContext, localContext, withCapImpl, HasCap, HasContext, Context, CapImpl (CapImpl))

import Loot.Log
import Loot.Log.Internal.Context (LogContext, MonadHasLogContext (..))
import Loot.Log.Internal.Core (Logging (..), MonadLogging (..))

instance HasCap Logging caps => MonadLogging (Crio caps) where
    emit r = withCapImpl $ \cap -> _emit cap r

instance HasContext LogContext caps => MonadHasLogContext (Crio caps) where
    askLogCtx = askContext
    localLogCtx = localContext


mkLogging :: LoggingImpl IO -> Component Logging '[] '[Context LogContext]
mkLogging loggingImpl' = Component{cIni, cFini}
  where
    cIni = let loggingImpl :: CapImpl Logging '[Context LogContext] IO
               loggingImpl = CapImpl $ Logging
                   { _emit = \r -> do
                       ctx <- askContext
                       lift $ loggingImpl' ctx r
                   }
           in pure loggingImpl
    cFini _ = pure ()

{-
-- | Add a logging implementation to your 'CapsT'.
--
-- This function adds an implementation of 'LogContext' storage and then
-- implements 'Logging' using it and the 'LoggingImpl' provided.
withLogging
    :: forall m caps a.
       ( Monad m
       , HasNoCap Logging caps
       , HasNoCap (Context LogContext) caps
       ) => LoggingImpl m
         -> Crio (Logging ': Context LogContext ': caps) a -> Crio caps a
withLogging loggingImpl' =
    let loggingImpl :: CapImpl Logging '[Context LogContext] m
        loggingImpl = CapImpl $ Logging
            { _emit = \r -> do
                ctx <- askContext
                lift $ loggingImpl' ctx r
            }
    in withReaderT (addCap loggingImpl . addCap (newContext mempty))
-}
