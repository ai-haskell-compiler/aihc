{-# LANGUAGE TemplateHaskell, Rank2Types #-}

-- | Derive lenses for "Lens.Family2".
-- 
-- Example usage:
-- 
-- 
-- > {-# LANGUAGE TemplateHaskell, Rank2Types #-}
-- > 
-- > import Lens.Family2
-- > import Lens.Family2.TH
-- > 
-- > data Foo a = Foo { _bar :: Int, _baz :: a }
-- >            deriving (Show, Read, Eq, Ord)
-- > $(makeLenses ''Foo)
-- 
module Lens.Family2.TH (
    makeLenses
  , makeLensesBy
  , makeLensesFor

  , makeTraversals

  , mkLenses
  , mkLensesBy
  , mkLensesFor
  ) where

import Language.Haskell.TH
import Lens.Family.THCore


-- | Derive lenses for the record selectors in 
-- a single-constructor data declaration,
-- or for the record selector in a newtype declaration.
-- Lenses will only be generated for record fields which
-- are prefixed with an underscore.
-- 
-- Example usage:
-- 
-- > $(makeLenses ''Foo)
makeLenses :: Name -> Q [Dec]
makeLenses = makeLensesBy defaultNameTransform

{-# DEPRECATED mkLenses "Use makeLenses instead." #-}
mkLenses :: Name -> Q [Dec]
mkLenses = makeLenses


-- | Derive lenses with the provided name transformation
-- and filtering function. Produce @Just lensName@ to generate a lens
-- of the resultant name, or @Nothing@ to not generate a lens
-- for the input record name.
-- 
-- Example usage:
-- 
-- > $(makeLensesBy (\n -> Just (n ++ "L")) ''Foo)
makeLensesBy :: (String -> Maybe String) -> Name -> Q [Dec]
makeLensesBy = deriveLenses deriveLensSig

{-# DEPRECATED mkLensesBy "Use makeLensesBy instead." #-}
mkLensesBy :: (String -> Maybe String) -> Name -> Q [Dec]
mkLensesBy = makeLensesBy


-- | Derive lenses, specifying explicit pairings of @(fieldName, lensName)@.
-- 
-- Example usage:
-- 
-- > $(makeLensesFor [("_foo", "fooLens"), ("bar", "lbar")] ''Foo)
makeLensesFor :: [(String, String)] -> Name -> Q [Dec]
makeLensesFor fields = makeLensesBy (`lookup` fields)

{-# DEPRECATED mkLensesFor "Use makeLensesFor instead." #-}
mkLensesFor :: [(String, String)] -> Name -> Q [Dec]
mkLensesFor = makeLensesFor


-- TODO
deriveLensSig :: Name -> LensTypeInfo -> ConstructorFieldInfo -> Q [Dec]
deriveLensSig _ _ _ = return []
