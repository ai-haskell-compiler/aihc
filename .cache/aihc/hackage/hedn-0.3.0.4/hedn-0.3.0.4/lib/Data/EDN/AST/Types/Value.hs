{-# LANGUAGE DeriveDataTypeable #-}

module Data.EDN.AST.Types.Value
  ( TaggedValue
  , Value(..)
  , EDNList
  , EDNVec
  , EDNMap
  , EDNSet
  , mkList
  , mkVec
  , mkMap
  , mkSet
  ) where

import Data.Data (Data)
import Data.Foldable (toList)
import Data.Text (Text)

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V

import Data.EDN.AST.Types.Tagged (Tagged)

type TaggedValue = Tagged Text Value

data Value
  = Nil                   -- ^ @ nil               @
  | Boolean   !Bool       -- ^ @ true | false      @
  | String    !Text       -- ^ @ "a string"        @
  | Character !Char       -- ^ @ \c                @
  | Symbol    !Text !Text -- ^ @ a-prefix/a-symbol @
  | Keyword   !Text       -- ^ @ :a-keyword        @
  | Integer   !Int        -- ^ @ 42                @
  | Floating  !Double     -- ^ @ 3.14              @
  | List      !EDNList    -- ^ @ (a list)          @
  | Vec       !EDNVec     -- ^ @ [a vector]        @
  | Map       !EDNMap     -- ^ @ {:a map}          @
  | Set       !EDNSet     -- ^ @ #{a set}          @
  deriving (Eq, Ord, Show, Data)

type EDNList = [TaggedValue]
type EDNVec  = V.Vector TaggedValue
type EDNMap  = M.Map TaggedValue TaggedValue
type EDNSet  = S.Set TaggedValue

mkList :: Foldable f => f TaggedValue -> Value
mkList = List . toList

mkVec :: Foldable f => f TaggedValue -> Value
mkVec = Vec . V.fromList . toList

mkMap :: Foldable f => f (TaggedValue, TaggedValue) -> Value
mkMap = Map . M.fromList . toList

mkSet :: Foldable f => f TaggedValue -> Value
mkSet = Set . S.fromList . toList
