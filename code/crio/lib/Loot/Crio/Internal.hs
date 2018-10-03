{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Loot.Crio.Internal
       ( Crio
       , Component (..)

       , upcastCrio

       , Finaliser
       , CompProvides
       , CompIniRequires
       , CompRunRequires
       , CompsProvide

       , withComponent
       , withComponents
       ) where

import Control.Exception.Safe (bracket)
import Data.HList (HList (HCons, HNil))
import Data.Singletons (Apply, TyFun)
import Data.Singletons.Prelude as Type ((:++), Map)
import Unsafe.Coerce (unsafeCoerce)

import Monad.Capabilities (CapImpl (getCapImpl), Capabilities, CapabilitiesBuilder (AddCap, BaseCaps),
                           CapsT (..), HasCaps, HasNoCap, addCap, buildCaps, withCapsT)


-- | @Crio@ is a @RIO@ monad (that is, 'Reader' over 'IO') where
-- the Reader context is managed through the @caps@ library.
type Crio caps = CapsT caps IO

upcastCrio
    :: HasCaps icaps caps
    => Crio icaps a
    -> Crio caps  a
upcastCrio = upcastCapsT
  where
    upcastCapsT
        :: HasCaps icaps caps
        => CapsT icaps IO a
        -> CapsT caps  IO a
    upcastCapsT r = CapsT $ \caps -> runCapsT r (downcastCapabilities caps)

    downcastCapabilities
        :: HasCaps icaps caps
        => Capabilities caps  IO
        -> Capabilities icaps IO
    downcastCapabilities = unsafeCoerce  -- HACK


type Finaliser cap iniCaps runCaps = cap (Crio runCaps) -> Crio iniCaps ()

-- | A component is a Capability together with initialisation
-- and finalisation actions.
data Component cap iniCaps runCaps = Component
    { cIni  :: Crio iniCaps (CapImpl cap runCaps IO)
    , cFini :: Finaliser cap iniCaps runCaps
    }

type family CompProvides comp where
    CompProvides (Component cap _ _) = cap
data CompProvidesSym :: TyFun Type ((Type -> Type) -> Type) -> Type
type instance Apply CompProvidesSym comp = CompProvides comp

type family CompIniRequires comp where
    CompIniRequires (Component _ iniCaps _) = iniCaps

type family CompRunRequires comp where
    CompRunRequires (Component _ _ runCaps) = runCaps


type family CompsProvide comps caps where
    CompsProvide '[] caps = caps
    CompsProvide (comp ': comps) caps = CompProvides comp ': CompsProvide comps caps


-- | Perform an action that requires a component to be active.
--
-- Initialises the component, adds its capability to the monad,
-- performs the action, finalises the component.
withComponent
    :: ( HasCaps iniCaps ctxCaps
       , HasCaps runCaps (cap ': ctxCaps)
       , HasNoCap cap ctxCaps
       , HasCaps runCaps runCaps
       , Typeable cap
       )
    => Component cap iniCaps runCaps  -- ^ Component to add
    -> Crio (cap ': ctxCaps) a        -- ^ Action to perform
    -> Crio ctxCaps a
withComponent Component{cIni, cFini} act =
    bracket
        (upcastCrio cIni)
        (upcastCrio . cFini . getCapImpl)
        (\impl -> withCapsT (addCap impl) act)


-- | Perform an action that requires multiple components to be active.
--
-- Initialises components in order (from right to left), adds all capabilities
-- to the monad, performs the action, finalises all components in reverse order.
--
-- Each component is initialised independently of the others, that is
-- ok for components to depend on each other when runnin, but it is not ok
-- for any component to require any other one during initialisation.
withComponents
    :: forall (comps :: [Type]) ctxCaps newCaps a.
       ( IniComponents comps ctxCaps
       , ExtendCaps (newCaps :++ ctxCaps) comps ctxCaps
       , newCaps ~ Type.Map CompProvidesSym comps
       )
    => HList comps
    -> Crio (newCaps :++ ctxCaps) a
    -> Crio ctxCaps a
withComponents comps act =
    bracket
        (iniComponents comps)
        (finiComponents (Proxy :: Proxy comps))
        (\iniResults ->
            withCapsT (extendCaps (Proxy :: Proxy comps) iniResults)
          $ act
        )


data IniResultSym :: TyFun Type Type -> Type
type instance Apply IniResultSym (Component cap iniCaps runCaps) =
    (CapImpl cap runCaps IO, Finaliser cap iniCaps runCaps)

-- | An instance of this class asserts that:
--
-- 1. Initialisation of components in @comps@ is independent from each other.
-- 2. Each component in @comps@ can be initialised in a context that has @ctxCaps@.
class IniComponents comps ctxCaps where
    iniComponents
        :: HList comps
        -> Crio ctxCaps (HList (Type.Map IniResultSym comps))
    finiComponents
        :: Proxy comps
        -> HList (Type.Map IniResultSym comps)
        -> Crio ctxCaps ()

instance IniComponents '[] ctxCaps where
    iniComponents HNil = pure HNil
    finiComponents _ HNil = pure ()

instance ( comp ~ Component cap iniCaps runCaps
         , HasCaps iniCaps ctxCaps
         , IniComponents comps ctxCaps
         , HasCaps runCaps runCaps
         ) => IniComponents (comp ': comps) ctxCaps where
    iniComponents (HCons comp comps) =
        HCons <$> iniComponent comp <*> iniComponents comps
      where
        iniComponent Component{cIni, cFini} = (, cFini) <$> upcastCrio cIni

    finiComponents _ (HCons iniRes iniRess) =
        finiComponents (Proxy :: Proxy comps) iniRess >> finiComponent iniRes
      where
        finiComponent (impl, finaliser) = upcastCrio (finaliser $ getCapImpl impl)


-- | An instance of this class asserts that:
--
-- 1. Each component in @comps@ provides a different capability and none of them
--    are already in @ctxCaps@.
-- 2. @ctxCaps@ contains all capabilities required by every capability from @comps@.
class ExtendCaps allCaps comps ctxCaps where
    extendingBuilder
        :: Proxy allCaps
        -> Proxy comps
        -> Proxy ctxCaps
        -> HList (Type.Map IniResultSym comps)
        -> Capabilities ctxCaps IO
        -> CapabilitiesBuilder allCaps (Type.Map CompProvidesSym comps :++ ctxCaps) IO

instance ExtendCaps allCaps '[] ctxCaps where
    extendingBuilder _ _ _ HNil caps = BaseCaps caps

instance ( comp ~ Component cap iniCaps runCaps
         , HasCaps runCaps allCaps
         , HasNoCap cap (Type.Map CompProvidesSym comps :++ ctxCaps)
         , ExtendCaps allCaps comps ctxCaps
         , Typeable cap
         ) => ExtendCaps allCaps (comp ': comps) ctxCaps where
    extendingBuilder pAll _ pCtx (HCons (impl, _) iniRess) caps =
        AddCap impl (extendingBuilder pAll (Proxy :: Proxy comps) pCtx iniRess caps)

extendCaps
    :: forall comps newCaps ctxCaps.
       ( ExtendCaps (newCaps :++ ctxCaps) comps ctxCaps
       , newCaps ~ Type.Map CompProvidesSym comps
       )
    => Proxy comps
    -> HList (Type.Map IniResultSym comps)
    -> Capabilities ctxCaps IO
    -> Capabilities (newCaps :++ ctxCaps) IO
extendCaps _ iniRess =
    buildCaps
  . extendingBuilder
      (Proxy :: Proxy (newCaps :++ ctxCaps))
      (Proxy :: Proxy comps)
      (Proxy :: Proxy ctxCaps)
      iniRess
