{-# OPTIONS_GHC -dsuppress-all #-}
{-# LANGUAGE CPP, TemplateHaskell #-}

-- This module tests the "definitional associativity" of applicative functors
-- from:
-- - ap-normalize
-- - base
-- - transformers
--
-- An operation (here (<*>)) is definitionally associative if it is
-- associative only by unfolding its definition and by simplification
-- (beta-reduction, and sometimes eta-conversion for data types, to commute
-- "case" expressions).

import Control.Applicative (liftA2, ZipList)
import Data.Monoid (Endo)

import Control.Monad.ST (ST)
import Data.Functor.Product (Product)
import GHC.Conc (STM)

import Control.Monad.Trans.Accum (Accum)
import Control.Monad.Trans.Cont (ContT)
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.RWS (RWS)
import Control.Monad.Trans.Reader (Reader)
import Control.Monad.Trans.Select (Select)
import Control.Monad.Trans.Writer (Writer)

import Test.Inspection

import ApNormalize (Aps)
import ApNormalize.DList (ApDList)

assoc1, assoc2 :: Applicative f => f a -> f b -> f c -> f (a, b, c)
assoc1 x y z = liftA2 (,,) x y <*> z
assoc2 x y z = liftA2 (\x (y, z) -> (x, y, z)) x (liftA2 (,) y z)

#ifdef __STDC__
#define CONCAT(x,y) x##y
#else
-- cpp -traditional
#define CONCAT(x,y) x'_'y
#endif

#define TEST_ASSOC_(NAME,M,FFF,CSTR) \
CONCAT(assoc1,NAME), CONCAT(assoc2,NAME) :: CSTR M a -> M b -> M c -> M (a, b, c) ; \
CONCAT(assoc1,NAME) = assoc1 ; \
CONCAT(assoc2,NAME) = assoc2 ; \
inspect $ {-'-} 'CONCAT(assoc1,NAME) FFF {-'-} 'CONCAT(assoc2,NAME)
-- Those {-'-} {-'-} trick CPP into tokenizing single-quoted strings
-- (clang was quite confused in particular).

#define TEST_ASSOC(NAME,M,FFF) TEST_ASSOC_(NAME,M,FFF,)


-- Aps is actually not definitionally associative (it needs to know
-- that computations were wrapped with 'liftAps' to do its work).
TEST_ASSOC_(Aps,Aps f,=/=,Applicative f =>)

-- Applicative difference lists are definitionally associative.
TEST_ASSOC(ApDList,ApDList f,==-)


-- Most of the fully concrete monads are definitionally associative.
-- Unlike monad transformers with an abstract monad.
TEST_ASSOC(IO,IO,===)
TEST_ASSOC(ST,ST s,===)
TEST_ASSOC(STM,STM,===)
TEST_ASSOC(Maybe,Maybe,===)
TEST_ASSOC(ProductMaybe,Product Maybe Maybe,===)
TEST_ASSOC(Either,Either e,===)
TEST_ASSOC(Reader,Reader r,===)
TEST_ASSOC(State,Lazy.State s,==-)
TEST_ASSOC(SState,Strict.State s,==-)
TEST_ASSOC(Cont,ContT r m,===)

-- Writer-like monads are only definitionally associative when the
-- monoid is also definitionally associative.
TEST_ASSOC(AccumEndo,Accum (Endo w),===)
TEST_ASSOC(WriterEndo,Writer (Endo w),===)
TEST_ASSOC(RWSEndo,RWS r (Endo w) s,==-)
TEST_ASSOC_(Accum,Accum w,=/=,Monoid w =>)
TEST_ASSOC_(Writer,Writer w,=/=,Monoid w =>)
TEST_ASSOC_(RWS,RWS r w s,=/=,Monoid w =>)

-- These are NOT definitionally associative
TEST_ASSOC(List,[],=/=)
TEST_ASSOC(ZipList,ZipList,=/=)
TEST_ASSOC(Select,Select r,=/=)

main :: IO ()
main = pure ()
