{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE Safe                 #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.TypeNums.Equality where

import           Data.Type.Bool     (Not)
import qualified Data.Type.Equality as DTE
import           Data.TypeNums.Arithmetic.Internal(Simplify)
import           Data.TypeNums.Ints(TInt(..))
import           Data.TypeNums.Rats.Type(Rat(..))
import           GHC.TypeLits(Nat)

infix 4 ==?, /=?, ==, /=

-- | Boolean type-level equals.  Useful for e.g.
--   @'Data.Type.Bool.If' (x ==? 0)@
type family (a :: k1) ==? (b :: k2) :: Bool where
  (a :: Rat) ==? (b :: Rat) = (DTE.==) (Simplify a) (Simplify b)

  -- We can omit all "False" cases because that will be
  -- caught by the final fallthrough case

  -- Nat and TInt
  (a :: Nat) ==? ('Pos a)   = 'True
  ('Pos a)   ==? (a :: Nat) = 'True

  -- Nat/TInt and Rat
  (a :: Nat) ==? (r :: Rat) = (a ':% 1) ==? r
  (r :: Rat) ==? (a :: Nat) = r ==? (a ':% 1)
  (a :: TInt) ==? (r :: Rat) = (a ':% 1) ==? r
  (r :: Rat) ==? (a :: TInt) = r ==? (a ':% 1)
  
  (a :: k)   ==? (b :: k)   = (DTE.==) a b
  _          ==? _          = 'False

-- | Boolean type-level not-equals.
type (a :: k1) /=? (b :: k2) = Not (a ==? b)

-- | Equality constraint, used as e.g. @(x == 3) => _@
type (a :: k1) == (b :: k2) = (a ==? b) ~ 'True

-- | Not-equal constraint
type (a :: k1) /= (b :: k2) = (a ==? b) ~ 'False
