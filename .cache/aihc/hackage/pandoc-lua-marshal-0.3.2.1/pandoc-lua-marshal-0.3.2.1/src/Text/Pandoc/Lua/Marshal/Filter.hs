{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{- |
Copyright  : © 2021-2026 Albert Krewinkel
License    : MIT
Maintainer : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Marshaling/unmarshaling functions Lua filters, i.e., tables containing
functions to be called on specific elements.
-}
module Text.Pandoc.Lua.Marshal.Filter
  ( -- * Filters
    Filter (..)
  , WalkingOrder (..)
  , peekFilter
  , lookup
  , member
    -- * Individual filter functions
  , FilterFunction (..)
  , peekFilterFunction
  , pushFilterFunction
  , getFunctionFor
    -- * Names in filter functions
  , baseFunctionName
  , listFunctionName
  , valueFunctionNames
  ) where

import Prelude hiding (lookup)
import Control.Applicative ((<|>), optional)
import Control.Monad ((<$!>), void)
import Data.Data
  ( Data, dataTypeConstrs, dataTypeName, dataTypeOf
  , showConstr, toConstr, tyconUQname )
import Data.Foldable (foldrM)
import Data.Map (Map)
import Data.Proxy (Proxy (Proxy))
import Data.String (IsString (fromString))
import HsLua
import Text.Pandoc.Definition (Pandoc, Meta, Block, Inline)
import qualified Data.Map.Strict as Map

-- | Filter function stored in the registry
newtype FilterFunction = FilterFunction Reference

-- | Pushes a filter function to the stack.
--
-- Filter functions are stored in the registry and retrieved from there.
pushFilterFunction :: LuaError e => FilterFunction -> LuaE e ()
pushFilterFunction (FilterFunction fnRef) =
  void $ getref registryindex fnRef

-- | Retrieves a filter function from the stack.
--
-- The value at the given index must be a function. It is stored in the
-- Lua registry.
peekFilterFunction :: Peeker e FilterFunction
peekFilterFunction = typeChecked "function" isfunction $ \idx -> liftLua $ do
  pushvalue idx
  FilterFunction <$> ref registryindex


-- | Collection of filter functions (at most one function per element
-- constructor)
data Filter = Filter
  { filterWalkingOrder :: WalkingOrder
  , filterMap :: Map Name FilterFunction
  }

-- | Description of how an AST should be traversed.
data WalkingOrder
  = WalkForEachType  -- ^ Process each type separately, traversing the
                     -- tree bottom-up (leaves to root) for each type.
  | WalkTopdown      -- ^ Traverse the tree top-down, from root to
                     -- leaves and depth first, in a single traversal.

-- | Retrieves a default `Filter` object from the stack, suitable for
-- filtering a full document.
peekFilter :: LuaError e => Peeker e Filter
peekFilter = peekFilter' $
    baseFunctionName (Proxy @Pandoc)
  : baseFunctionName (Proxy @Meta)
  : baseFunctionName (Proxy @Block)
  : baseFunctionName (Proxy @Inline)
  : listFunctionName (Proxy @Block)
  : listFunctionName (Proxy @Inline)
  :  valueFunctionNames (Proxy @Inline)
  ++ valueFunctionNames (Proxy @Block)

-- | Retrieves a `Filter` object from the stack, fetching all functions
-- in the given list of names.
peekFilter' :: LuaError e => [Name] -> Peeker e Filter
peekFilter' fnNames idx = do
  let go constr acc = liftLua $ do
        _ <- getfield idx constr
        runPeek (peekFilterFunction top `lastly` pop 1) >>= \case
          Success fn -> pure $ Map.insert constr fn acc
          Failure {} -> pure acc
  walkingSequence <- do
    _ <- liftLua $ getfield idx "traverse"
    optional (peekText top) `lastly` pop 1 >>= \case
      Just "typewise" -> pure WalkForEachType
      Just "topdown"  -> pure WalkTopdown
      _               -> pure WalkForEachType
  Filter walkingSequence <$!> foldrM go Map.empty fnNames

-- | Looks up a filter function in a Lua 'Filter'.
lookup :: Name -> Filter -> Maybe FilterFunction
lookup name = (name `Map.lookup`) . filterMap

-- | Checks whether the 'Filter' contains a function of the given name.
member :: Name -> Filter -> Bool
member name = (name `Map.member`) . filterMap

-- | Filter function names for a given type.
valueFunctionNames :: forall a. Data a => Proxy a -> [Name]
valueFunctionNames _ = map (fromString . show) . dataTypeConstrs . dataTypeOf
                     $ (undefined :: a)

-- | The name of a type's base function, which is called if there is no
-- more specific function for a value.
baseFunctionName :: forall a. Data a => Proxy a -> Name
baseFunctionName _ =
  fromString . tyconUQname . dataTypeName . dataTypeOf
  $ (undefined :: a)

-- | The name of the functions that's called on lists of the given type.
listFunctionName :: forall a. Data a => Proxy a -> Name
listFunctionName _ =
  fromString . (++ "s") . tyconUQname . dataTypeName . dataTypeOf
  $ (undefined :: a)

-- | Finds the best filter function for a given element; returns
-- 'Nothing' if no such function exists.
getFunctionFor :: forall a. Data a => Filter -> a -> Maybe FilterFunction
getFunctionFor filter' x =
  let constrName = fromString . showConstr . toConstr $ x
      typeName = fromString . tyconUQname . dataTypeName . dataTypeOf $ x
  in constrName `lookup` filter' <|>
     typeName   `lookup` filter'
