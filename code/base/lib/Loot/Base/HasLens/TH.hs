-- | Utilities to work with 'HasLens'.

module Loot.Base.HasLens.TH
    ( deriveHasLens
    , deriveHasLensPath
    , deriveHasLensPathWith
    , deriveHasLensDirect
    , deriveHasLensDirectWith
    ) where

import Control.Lens (DefName (TopName), LensRules, lensField, lensRules, to, traversed)
import qualified Data.List as L
import qualified Data.Text as T
import qualified Language.Haskell.TH as TH

import Loot.Base.HasLens.Class (HasLens (..))

-- | Where to get lens to produce 'HasLens' instance.
data DeriveHasLensType
    = DeriveViaLensOf
      -- ^ Using `lensOf` to correspond a field accessor
    | DeriveViaFieldLens LensRules
      -- ^ Using lens generated using `makeLensesWith` to correspond a field
      -- accessor

-- | @ deriveHasLensExts deriveWay preLens ctx ctxPart @ traverses all
-- fields of @ ctxPart @ datatype and for each field of type @ F @ produces
-- @ instance HasLens ctx F where lensOf = preLens . fieldLens @,
-- where @ preLens :: Lens' ctx ctxPart @ holds and
-- @ fieldLens :: Lens' ctxPart F @ is either @ lensOf \@F @ or a lens
-- generated with @ makeLenses @, depending on @ deriveWay @ parameter.
deriveHasLensExt
    :: DeriveHasLensType
    -> TH.Q TH.Exp
    -> TH.Name    -- ^ Name of @H@ type
    -> TH.Name    -- ^ Name of @M@ type
    -> TH.Q [TH.Dec]
deriveHasLensExt deriveWay preLensQ ctxName tyName = do
    ty <- TH.reify tyName
    decs <- case ty of
        TH.TyConI (TH.DataD _ _ _ _ [TH.RecC _ fields] _) -> do
            let fieldNames = fields ^.. traversed . _1 . to show . to toUnqualifiedName
            forM fields $ \(quaFieldName, _, fieldTy) -> do
                let fieldName = toUnqualifiedName (show quaFieldName)
                let mkLensName rules = toLensName rules fieldNames fieldName
                mkInstance fieldTy mkLensName
        _ -> fail "Expected name of a datatype with exactly one constructor \
                  \and only record fields"
    return $ fold decs
  where
    toUnqualifiedName = TH.mkName . toString . L.last . T.splitOn "."
    toLensDecl rules = rules ^. lensField
    toLensName rules fieldNames fieldName =
        case (toLensDecl rules) tyName fieldNames fieldName of
              TopName x:_ -> return (TH.mkName $ show x)
              _           -> fail ("No lens generated for field '" <>
                                   show fieldName <> "', did you specify \
                                   \correct rules?")

    mkInstance fieldTy mkLensName = do
      preLens <- preLensQ
      let addPreLens = TH.UInfixE (preLens) (TH.VarE $ TH.mkName ".")
      fieldLens <- [e| lensOf @($(pure fieldTy)) |]
      case deriveWay of
        DeriveViaLensOf ->
            [d| instance HasLens $(pure fieldTy) $(TH.conT ctxName) $(pure fieldTy) where
                      lensOf = $(pure $ addPreLens fieldLens)
                |]
        DeriveViaFieldLens rules -> do
            lensName <- mkLensName rules
            [d| instance HasLens $(pure fieldTy) $(TH.conT ctxName) $(pure fieldTy) where
                      lensOf = $(pure $ addPreLens (TH.VarE lensName))
                |]

-- | Shortcut for 'deriveHasLensExt'.
-- Implementation will look like @ lensOf = topLens . lensOf @FieldType @.
deriveHasLens :: TH.Name -> TH.Name -> TH.Name -> TH.Q [TH.Dec]
deriveHasLens topLens = deriveHasLensExt DeriveViaLensOf (TH.varE topLens)

-- | Shortcut for 'deriveHasLensExt'.
-- Implementation will look like @ lensOf = fieldLens @.
deriveHasLensDirect :: TH.Name -> TH.Q [TH.Dec]
deriveHasLensDirect = deriveHasLensDirectWith lensRules

-- | Shortcut for 'deriveHasLensExt'.
-- Implementation will look like @ lensOf = fieldLens @.
--
-- Fits for case when 'Control.Lens.makeLensesWith' was used to generate lenses,
-- otherwise see 'deriveHasLensDirect'.
deriveHasLensDirectWith :: LensRules -> TH.Name -> TH.Q [TH.Dec]
deriveHasLensDirectWith rules tyName =
    deriveHasLensExt (DeriveViaFieldLens rules) (TH.varE 'id) tyName tyName

{- | Shortcut for 'deriveHasLensExt'.
Implementation will look like @ lensOf = myComplexLens . fieldLens @.

May be useful, if you for some reason do not want to rely on 'lensOf' as
field-accessing lens.

@ myComplexLens @ parameter is supposed to be passed in Quasy-Quotes:

@
[e| myComplexLens |]
@

If 'makeLensesWith' was used to produce lenses for fields, see
'deriveHasLensPathWith'.
-}
deriveHasLensPath :: TH.Q TH.Exp -> TH.Name -> TH.Name -> TH.Q [TH.Dec]
deriveHasLensPath = deriveHasLensPathWith lensRules

{- | Shortcut for 'deriveHasLensExt'.
Implementation will look like @ lensOf = myComplexLens . fieldLens @.

Fits for case when 'Control.Lens.makeLensesWith' was used to generate lenses,
otherwise see 'deriveHasLensPath'.
-}
deriveHasLensPathWith :: LensRules -> TH.Q TH.Exp -> TH.Name -> TH.Name -> TH.Q [TH.Dec]
deriveHasLensPathWith rules = deriveHasLensExt (DeriveViaFieldLens rules)
