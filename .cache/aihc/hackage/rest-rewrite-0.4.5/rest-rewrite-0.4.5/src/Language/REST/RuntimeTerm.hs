{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.REST.RuntimeTerm
  ( RuntimeTerm(..)
  , ToRuntimeTerm(..)
  , subTerms
  , contains
  )
where

import           Data.Hashable
import           GHC.Generics (Generic)
import           Text.Printf
import qualified Data.List as L

import           Language.REST.Op

-- | Ground terms
data RuntimeTerm = App Op [RuntimeTerm] deriving (Eq, Ord, Generic, Hashable)

instance Show RuntimeTerm where
  show (App op []) = show op
  show (App op ts) = printf "%s(%s)" (show op) $ L.intercalate ", " (map show ts)

-- | Transformable to a ground term
class ToRuntimeTerm a where
  toRuntimeTerm :: a -> RuntimeTerm

instance ToRuntimeTerm Op where
  toRuntimeTerm op = App op []

instance ToRuntimeTerm RuntimeTerm where
  toRuntimeTerm = id

-- | @subTerms t@ returns a list of pairs @(s, f)@, where @s@ is a subterm of @t@,
-- and @f@ is a function that takes a replacement @s'@ for @s@, and generates a new
-- term where @s@ is replaced with @s'@ in @t@. Also includes the pair (t, id),
-- representing the term itself.
-- TODO: Consider more efficient implementations
subTerms :: RuntimeTerm -> [(RuntimeTerm, RuntimeTerm -> RuntimeTerm)]
subTerms t@(App f ts) = (t, id) : concatMap st [0..length ts - 1]
  where
    st :: Int -> [(RuntimeTerm, RuntimeTerm -> RuntimeTerm)]
    st i =
      let
        ti = ts !! i
        go t' =
          App f $ take i ts ++ [t'] ++ drop (i + 1) ts
        go2 (srt, toFull) = (srt, go . toFull)
      in
        map go2 (subTerms ti)


-- | @t `contains` u@ iff @t == u@ or @u@ is a subterm of @t@
contains :: RuntimeTerm -> RuntimeTerm -> Bool
contains t1 t2 | t1 == t2 = True
contains (App _ ts) t     = any (contains t) ts
