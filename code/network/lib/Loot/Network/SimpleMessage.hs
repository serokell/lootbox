{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}

-- | Simple/stupid messaging. I've tried to make it really cool from
-- the scratch, but failed. To be improved later.

module Loot.Network.SimpleMessage where

import qualified Data.Dependent.Map as D
import Data.Functor.Identity (Identity (..))
import Data.GADT.Compare (GEq (..))
import qualified Data.Map as M
import Data.Reflection
import Data.Singletons.TypeLits hiding (natVal)
import Data.Tagged
import Unsafe.Coerce (unsafeCoerce)


class Binary b where
    parse :: ByteString -> b


data Msg1 = Msg1 String
data Msg2 = Msg2 Integer
instance Binary Msg1
instance Binary Msg2

class Binary d => Message d where
    getMsgTag :: Word32

instance Message Msg1 where
    getMsgTag = 1

instance Message Msg2 where
    getMsgTag = 2

data Runner = forall d. Message d => Runner { unRunner :: d -> IO () }

type RunnerMap = Map Word32 Runner

createRunnerMap :: [Runner] -> RunnerMap
createRunnerMap runners =
    let bunch = map (\r@(Runner (_ :: d -> IO ())) -> (getMsgTag @d,r)) runners
    in M.fromList bunch

processRunners :: RunnerMap -> Word32 -> ByteString -> IO ()
processRunners rm i bs = case rm M.! i of Runner x -> x (parse bs)

testAll :: IO ()
testAll = do
    let bs = "aoeukek"
    let r1 = Runner $ \(Msg1 s) -> putStrLn s
    let r2 = Runner $ \(Msg2 i) -> print $ i + i
    let rm = createRunnerMap [r1,r2]
    processRunners rm 2 "aoeusnthaeou"
