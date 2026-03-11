{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.REST.ConcreteOC where

import qualified Language.REST.OCAlgebra as AOC
import qualified Language.REST.Internal.WQO as WQO
import           Language.REST.RuntimeTerm
import           Language.REST.RPO
import           Language.REST.Op

import Data.Hashable
import GHC.Generics (Generic)
import qualified Data.Set as S

newtype ConcreteOC = ConcreteOC (S.Set (WQO.WQO Op))
  deriving (Eq, Ord, Generic, Hashable)

instance Show ConcreteOC where
  show (ConcreteOC ords) = show (S.size ords) ++ " orderings"

concreteOC :: Monad m => S.Set Op -> AOC.OCAlgebra ConcreteOC RuntimeTerm m
concreteOC ops = AOC.OCAlgebra (return . isSat) refine (ConcreteOC (WQO.orderings ops)) union notStrongerThan
  where
    union (ConcreteOC ord1) (ConcreteOC ord2) = ConcreteOC $ S.union ord1 ord2
    notStrongerThan (ConcreteOC ord1) (ConcreteOC ord2) = return $ ord1 == ord2 || ord2 `S.isSubsetOf` ord1

    isSat :: ConcreteOC -> Bool
    isSat (ConcreteOC ords) = not $ S.null ords

    refine :: ConcreteOC -> RuntimeTerm -> RuntimeTerm -> ConcreteOC
    refine (ConcreteOC ords) t u = ConcreteOC (S.filter (\ord -> synGTE ord t u) ords)
