{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}


module Language.REST.Types (
    prettyPrint
  , PPArgs(..)
  , Relation(..)
  , toOrderedSet
  ) where

import GHC.Generics (Generic)
import Prelude hiding (GT, EQ)

import           Data.Hashable
import qualified Data.List as L
import qualified Data.HashSet as S
import qualified Data.Set as OS
import qualified Data.Text as T
import           Text.Printf

import           Language.REST.Op
import           Language.REST.MetaTerm as MT

-- | Arguments used for pretty-printing terms
data PPArgs = PPArgs
  {
    -- | A list of pairs @(search, rep)@. If any operator starts with @search@
    --   for some element in the list, during the printing the operator is
    --   printed with the corresponding @rep@ in place of @search@.
    ppReplace  :: [(T.Text, T.Text)]

    -- | A list of pairs @(search, rep)@. If any operator matches @search@, then it's
    --   corresponding term is printed in infix style with operator @rep@.
  , ppInfixOps :: [(T.Text, T.Text)]

    -- | Used to override printing for some terms. When @ppCustom m = Just s@, then @m@
    --   be printed as @s@.
  , ppCustom   :: MetaTerm -> Maybe T.Text
  }

prettyPrint :: ToMetaTerm a => PPArgs -> a -> String
prettyPrint (PPArgs substs infixOps custom) t = T.unpack $ go $ replaceAll $ toMetaTerm t where

  replace s | Just (from, to) <- L.find ((`T.isPrefixOf` s) . fst) substs
            = T.append to $ T.drop (T.length from) s
  replace s  = s

  replaceAll :: MT.MetaTerm -> MT.MetaTerm
  replaceAll (MT.Var x)            = MT.Var x
  replaceAll (MT.RWApp (Op op) ts) = MT.RWApp (Op (replace op)) (map replaceAll ts)



  go :: MT.MetaTerm -> T.Text
  go (MT.Var x) = T.pack x
  go mt | Just s <- custom mt    = s
  go (MT.RWApp (Op op) [t1, t2]) | Just op' <- L.lookup op infixOps
    = T.pack $ printf "%s %s %s" (goParens t1) op' (goParens t2)
  go (MT.RWApp (Op op) [])       = op
  go (MT.RWApp (Op op) xs)       = T.concat [op, "(" , T.intercalate ", " (map go xs) , ")"]

  goParens mt | needsParens mt = T.pack $ printf "(%s)" (go mt)
  goParens mt       = go mt

  needsParens (MT.RWApp (Op op) _) = op `elem` map fst infixOps
  needsParens _                    = False

data Relation = GT | GTE | EQ deriving (Eq, Generic, Hashable)

instance Show Relation where
  show GT  = ">"
  show GTE = "≥"
  show EQ  = "≅"


toOrderedSet :: (Eq a, Hashable a, Ord a) => S.HashSet a -> OS.Set a
toOrderedSet = OS.fromList . S.toList
