-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Derive.Superclass
-- Copyright   :  (c) Song Zhang
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  haskell.zhang.song `at` hotmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

module Data.Derive.Superclass
  ( deriving_superclasses
  , strategy_deriving_superclasses
  , newtype_deriving_superclasses
  , gnds
  ) where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.State
import           Data.Derive.TopDown.CxtGen
import           Data.Derive.TopDown.IsInstance
import           Data.Derive.TopDown.Lib
import           Data.Derive.TopDown.Types
import           Data.List                      ( foldl1'
                                                , nub
                                                )
import           Language.Haskell.TH

-- Only support class that has paramter with kind * or * -> *
deriving_superclasses'
  :: Maybe DerivStrategy -> ClassName -> TypeName -> StateT [Type] Q [Dec]
deriving_superclasses' st cn tn = do
  pnames             <- lift $ reifyTypeParameters tn
  types              <- get
  isCnHighOrderClass <- lift $ isHigherOrderClass cn
  let t = if isCnHighOrderClass
        then
          let pns = init pnames
          in  if null pns
                then ConT tn
                else foldl1' AppT (ConT tn : (map VarT pns))
        else foldl1' AppT (ConT tn : (map VarT pnames))
  let tp = AppT (ConT cn) t

  isIns <- lift $ isInstance' cn [t]
  if (isIns || elem tp types)
    then return []
    else do
      classContext <- if isCnHighOrderClass
        then return []
        else lift $ genInferredContext cn tn
      let topClassInstance = [StandaloneDerivD st classContext tp]
      modify (tp :)
      ci <- lift $ reify cn
      case ci of
        ClassI (ClassD ctx _ _ _ _) _ -> do
          let classConTs = map getTypeConstructor ctx
          ss <- fmap (nub . concat) $ forM classConTs $ \superCln ->
            case superCln of
              ConT className -> do
                superclass_decls <- deriving_superclasses' st className tn
                return superclass_decls
              x -> error $ "cannot generate class for " ++ show x
          return $ topClassInstance ++ ss
        _ -> error $ show cn ++ "is not type class"

{- | Note: It cannot be used with mutual recursive types.

For mutual recursive types, you need to put them together. For mutual recursive types @T1@ and @T2@:

@
fmap concat (sequence [(deriving_superclasses ''Ord ''T1), (deriving_superclasses ''Ord ''T2)])
@
-}
deriving_superclasses :: ClassName -> TypeName -> Q [Dec]
deriving_superclasses cn tn =
  evalStateT (deriving_superclasses' Nothing cn tn) []

strategy_deriving_superclasses
  :: DerivStrategy  -- ^ deriving strategy
  -> ClassName      -- ^ class name
  -> TypeName       -- ^ type name
  -> Q [Dec]
strategy_deriving_superclasses st cn tn =
  evalStateT (deriving_superclasses' (Just st) cn tn) []

-- | Use newtype strategy to derive all the superclass instances.
newtype_deriving_superclasses :: ClassName -> TypeName -> Q [Dec]
newtype_deriving_superclasses = strategy_deriving_superclasses NewtypeStrategy

-- | Abbreviation for @newtype_deriving_superclasses@. for generalized newtype deriving
gnds :: ClassName -> TypeName -> Q [Dec]
gnds = newtype_deriving_superclasses
