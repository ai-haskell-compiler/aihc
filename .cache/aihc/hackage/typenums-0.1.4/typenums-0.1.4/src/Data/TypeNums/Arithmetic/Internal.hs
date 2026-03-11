{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE Safe                 #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

#if MIN_VERSION_base(4,12,0)
{-# LANGUAGE NoStarIsType #-}
#endif

-- | This module exposes the inner workings of type-level arithmetic for
--   further extensions.
module Data.TypeNums.Arithmetic.Internal (
  -- * Kind Results
    AddK
  , SubK
  , MulK
  , IntDivK
  , ExpK
  , NegK

  -- * Unary operations
  , Abs
  , Negate
  , Recip
  , Simplify

  -- ** Rounding operations
  , Truncate
  , Floor
  , Ceiling

  -- * Binary operations
  , Add
  , Sub
  , Mul
  , RatDiv
  , DivMod
  , QuotRem
  , Div
  , Mod
  , Quot
  , Rem
  , GCD
  , LCM
  , Exp
  , IntLog
) where

import Data.Type.Bool     (If)
import qualified Data.Type.Equality as DTE
import Data.TypeNums.Ints
import Data.TypeNums.Rats.Type
import GHC.TypeLits(Nat)
import qualified GHC.TypeLits as G

-- TODO: Reduce type-level rationals?  Probably not 100% necessary as the
-- user should only see them via ratVal which already handles reduction
-- at value level.

-- | The kind of the result of addition.
--
-- @since 0.1.2
type family AddK k1 k2 where
  AddK Rat  _    = Rat
  AddK _    Rat  = Rat
  AddK Nat  Nat  = Nat
  AddK Nat  TInt = TInt
  AddK TInt Nat  = TInt
  AddK TInt TInt = TInt

-- | The kind of the result of subtraction.
--
-- @since 0.1.2
type family SubK k1 k2 where
  SubK Rat  _    = Rat
  SubK _    Rat  = Rat
  SubK Nat  Nat  = Nat
  SubK Nat  TInt = TInt
  SubK TInt Nat  = TInt
  SubK TInt TInt = TInt

-- | The kind of the result of multiplication.
--
-- @since 0.1.2
type family MulK k1 k2 where
  MulK k    k    = k
  MulK Rat  _    = Rat
  MulK _    Rat  = Rat
  MulK Nat  TInt = TInt
  MulK TInt Nat  = TInt

-- | The kind of the result of division by a natural number
--
-- @since 0.1.4
type family IntDivK k where
  IntDivK Nat = Nat
  IntDivK TInt = TInt

-- | The kind of the result of type-level exponentiation
type family ExpK k1 k2 where
  ExpK Nat Nat = Nat
  ExpK TInt Nat = TInt
  ExpK Rat Nat = Rat
  ExpK _   TInt = Rat

-- | The kind of the result of negation
--
-- @since 0.1.4
type family NegK k where
  NegK Nat = TInt
  NegK TInt = TInt
  NegK Rat = Rat

-- | The sum of two type-level numbers.
--
-- @since 0.1.2
type family Add (x :: k1) (y :: k2) :: AddK k1 k2 where
  Add (x :: Nat) (y :: Nat)  = x G.+ y

  Add ('Pos x)   ('Pos y)   = 'Pos (x G.+ y)
  Add ('Neg x)   ('Pos y)   = If (x G.<=? y)
                                            ('Pos (y G.- x))
                                            ('Neg (x G.- y))
  Add ('Pos x)   ('Neg y)   = If (y G.<=? x)
                                            ('Pos (x G.- y))
                                            ('Neg (y G.- x))
  Add ('Neg x)   ('Neg y)   = 'Neg (x G.+ y)

  Add ('Pos x)   (y :: Nat) = 'Pos (x G.+ y)
  Add ('Neg x)   (y :: Nat) = If (x G.<=? y)
                                            ('Pos (y G.- x))
                                            ('Neg (x G.- y))
  Add (x :: Nat)  ('Pos y)  = 'Pos (x G.+ y)
  Add (x :: Nat)  ('Neg y)  = If (y G.<=? x)
                                            ('Pos (x G.- y))
                                            ('Neg (y G.- x))

  Add (x :: Rat) (y :: Rat) = Simplify (AddRat x y)
  Add (x :: Rat) y          = Simplify (AddRat x (y ':% 1))
  Add x          (y :: Rat) = Simplify (AddRat (x ':% 1) y)

type family AddRat (x :: Rat) (y :: Rat) :: Rat where
  AddRat (n1 ':% d1) (n2 ':% d2) = (Add (Mul n1 d2) (Mul n2 d1)) ':% (Mul d1 d2)

-- | The difference of two type-level numbers
--
--   For the difference of two naturals @a@ and @b@, @a-b@ is also a natural,
--   so only exists for @a@ >= @b@.
-- @since 0.1.2
type family Sub (x :: k1) (y :: k2) :: SubK k1 k2 where
  Sub (x :: Nat) (y :: Nat)  = x G.- y

  Sub ('Pos x)   ('Pos y)   = Add x ('Neg y)
  Sub ('Neg x)   ('Pos y)   = Add ('Neg x) ('Neg y)
  Sub ('Pos x)   ('Neg y)   = Add ('Pos x) ('Pos y)
  Sub ('Neg x)   ('Neg y)   = Add ('Neg x) ('Pos y)

  Sub ('Pos x)   (y :: Nat) = Add ('Pos x) ('Neg y)
  Sub ('Neg x)   (y :: Nat) = Add ('Neg x) ('Neg y)
  Sub (x :: Nat)  ('Pos y)  = Add x ('Neg y)
  Sub (x :: Nat)  ('Neg y)  = Add x ('Pos y)

  Sub (x :: Rat) (y :: Rat) = Simplify (SubRat x y)
  Sub (x :: Rat) y          = Simplify (SubRat x (y ':% 1))
  Sub x          (y :: Rat) = Simplify (SubRat (x ':% 1) y)

type family SubRat (x :: Rat) (y :: Rat) :: Rat where
-- Denominators are wrapped in Pos here to force the products to evaluate
-- as kind TInt instead of Nat.  Without this, it is possible to end up with,
-- for example, @(14-15) :: Nat@ which does not produce a Nat and therefore
-- causes typing to fail.
  SubRat (n1 ':% d1) (n2 ':% d2) = (Sub (Mul n1 ('Pos d2)) (Mul n2 ('Pos d1))) ':% (Mul d1 d2)

-- | The product of two type-level numbers
--
-- @since 0.1.2
type family Mul (x :: k1) (y :: k2) :: MulK k1 k2 where
  Mul (x :: Nat) (y :: Nat) = x G.* y

  Mul ('Pos x)   ('Pos y)   = 'Pos (x G.* y)
  Mul ('Neg x)   ('Pos y)   = 'Neg (x G.* y)
  Mul ('Pos x)   ('Neg y)   = 'Neg (x G.* y)
  Mul ('Neg x)   ('Neg y)   = 'Pos (x G.* y)

  Mul ('Pos x)   (y :: Nat) = 'Pos (x G.* y)
  Mul ('Neg x)   (y :: Nat) = 'Neg (x G.* y)
  Mul (x :: Nat)  ('Pos y)  = 'Pos (x G.* y)
  Mul (x :: Nat)  ('Neg y)  = 'Neg (x G.* y)

  Mul (x :: Rat) (y :: Rat) = Simplify (MulRat x y)
  Mul (x :: Rat) y          = Simplify (MulRat x (y ':% 1))
  Mul x          (y :: Rat) = Simplify (MulRat (x ':% 1) y)

type family MulRat (x :: Rat) (y :: Rat) :: Rat where
  MulRat (n1 ':% d1) (n2 ':% d2) = (Mul n1 n2) ':% (Mul d1 d2)

-- | The reciprocal of a type-level number
--
-- @since 0.1.4
type family Recip (x :: k) :: Rat where
  Recip (x :: Nat) = 'Pos 1 ':% x
  Recip ('Pos x)   = 'Pos 1 ':% x
  Recip ('Neg x)   = 'Neg 1 ':% x
  Recip ('Pos x ':% y) = 'Pos y ':% x
  Recip ('Neg x ':% y) = 'Neg y ':% x
  Recip ((x :: Nat) ':% y) = 'Pos y ':% x

-- | The result of dividing two type-level numbers.
--
-- @since 0.1.4
type family RatDiv (x :: k1) (y :: k2) :: Rat where
  RatDiv (x :: Nat) (y :: Nat) = Simplify (x ':% y)
  RatDiv (x :: TInt) (y :: Nat) = Simplify (x ':% y)
  RatDiv (x :: Nat) ('Pos y) = Simplify (x ':% y)
  RatDiv (x :: Nat) ('Neg y) = Simplify (('Neg x) ':% y)
  RatDiv (x :: TInt) ('Neg y) = Simplify ((Negate x) ':% y)
  RatDiv (x :: TInt) ('Pos y) = Simplify (x ':% y)

  -- Cases with Mul don't need "Simplify" as Mul already simplifies
  RatDiv (x :: Rat) (y :: Rat) = Mul x (Recip y)
  RatDiv x (y :: Rat) = Mul x (Recip y)
  RatDiv (x :: Rat) y = Mul x (RatDiv 1 y)

-- | The result of negating a 'TInt'
--
-- @since 0.1.4
type family Negate (x :: k) :: NegK k where
  Negate (x :: Nat) = Negate ('Pos x)
  Negate ('Pos 0) = 'Pos 0
  Negate ('Neg 0) = 'Pos 0
  Negate ('Pos x) = 'Neg x
  Negate ('Neg x) = 'Pos x
  Negate (x ':% y) = (Negate x) ':% y


infixl 7 `Div`, `Mod`, `Quot`, `Rem`

-- | The quotient and remainder of a type-level integer and a natural number.
--   For a negative dividend, the remainder part is positive such that
--   x = q*y + r
-- @since 0.1.4
type family DivMod (x :: k) (y :: Nat) :: (IntDivK k, IntDivK k) where
  DivMod _ 0 = G.TypeError ('G.Text "Divisor must not be 0")
  DivMod (x :: Nat) y = UnPos (DivModAux ('Pos x) y ('Pos 0))
  DivMod ('Pos x) y = DivModAux ('Pos x) y ('Pos 0)
  DivMod ('Neg x) y = DivModNegFixup (DivModAux ('Pos x) y ('Pos 0)) y

-- | The quotient and remainder of a type-level integer and a natural number.
--   For a negative dividend, the remainder part is negative such that
--   x = q*y + r
-- @since 0.1.4
type family QuotRem (x :: k) (y :: Nat) :: (IntDivK k, IntDivK k) where
  QuotRem ('Neg x) y = QuotRemFixup (DivMod ('Neg x) y) y
  QuotRem x y = DivMod x y

-- | The quotient of a type-level integer and a natural number.
--
-- @since 0.1.4
type family Div (x :: k) (y :: Nat) :: IntDivK k where
  Div x y = Fst (DivMod x y)

-- | The remainder of a type-level integer and a natural number
--   For a negative number, behaves similarly to 'mod'.
-- @since 0.1.4
type family Mod (x :: k) (y :: Nat) :: IntDivK k where
  Mod x y = Snd (DivMod x y)

-- Fixes up the calculated quotient for a negative dividend.
-- Subtracts 1 to obtain the behaviour of 'div' instead of 'quot'
type family DivModNegFixup (x :: (TInt, TInt)) (y :: Nat) :: (TInt, TInt) where
  DivModNegFixup '(a, 'Pos 0) y = '( Negate a, 'Pos 0 )
  DivModNegFixup '(a, 'Neg 0) y = '( Negate a, 'Pos 0 )
  DivModNegFixup '(a, b) y = '( Negate (Add 1 a), Sub y b )

-- Converts the result of DivMod for a negative dividend
-- into the result of QuotRem for a negative dividend
type family QuotRemFixup (x :: (TInt, TInt)) (y :: Nat) :: (TInt, TInt) where
  QuotRemFixup '(d, 'Pos 0) y = '( d, 'Pos 0 )
  QuotRemFixup '(d, 'Neg 0) y = '( d, 'Pos 0 )
  QuotRemFixup '(d,m) y = '(Add 1 d, Sub m y)

-- | The integer part of the result of dividing an integer by a
--   natural number
--
-- @since 0.1.4
type family Quot (x :: k) (y :: Nat) :: IntDivK k where
  Quot x y = Fst (QuotRem x y)

-- | The remainder of the result of dividing an integer by a
--   natural number
--
-- @since 0.1.4
type family Rem (x :: k) (y :: Nat) :: IntDivK k where
  Rem x y = Snd (QuotRem x y)

-- Integer division of positive / positive
type family DivModAux (x :: TInt) (y :: Nat) (a :: TInt) :: (TInt, TInt) where
  DivModAux ('Pos x) y a = DivModAux (Sub ('Pos x) y) y (Add 1 a)
  DivModAux ('Neg x) y a = '(Sub a 1, Sub y ('Pos x))

-- Internal function to unwrap a Pos TInt to a Nat
type family UnPos (x :: k1) :: k2 where
  UnPos ('Pos x) = x
  UnPos '( 'Pos x, 'Pos y) = '(x, y)

type family Fst (x :: (k1, k2)) :: k1 where
  Fst '(x,y) = x

type family Snd (x :: (k1, k2)) :: k2 where
  Snd '(x,y) = y

-- | The absolute value of a type-level number
--
-- @since 0.1.4
type family Abs (x :: k) :: k where
  Abs (x :: Nat) = x
  Abs ('Pos x) = 'Pos x
  Abs ('Neg x) = 'Pos x
  Abs (x ':% y) = Simplify (Abs x ':% y)

-- | The greatest common divisor of two type-level integers
--
-- @since 0.1.4
type family GCD (x :: k1) (y :: k2) :: Nat where
  GCD (x :: Nat) (y :: Nat) = GCDAux x y
  GCD (x :: Nat) (y :: TInt) = GCDAux x (UnPos (Abs y))
  GCD (x :: TInt) (y :: Nat) = GCDAux (UnPos (Abs x)) y
  GCD (x :: TInt) (y :: TInt) = GCDAux (UnPos (Abs x)) (UnPos (Abs y))

-- Euclidean algorithm for calculating the GCD of two natural numbers
type family GCDAux (x :: Nat) (y :: Nat) :: Nat where
  GCDAux x 0 = x
  GCDAux x y = GCDAux y (Rem x y)

-- | The least common multiple of two type-level integers
--
-- @since 0.1.4
type family LCM (x :: k1) (y :: k2) :: Nat where
  LCM (x :: Nat) (y :: Nat) = Mul (Div (Abs x) (GCD x y)) (Abs y)
  LCM (x :: Nat) (y :: TInt) = UnPos (Mul (Div (Abs x) (GCD x y)) (Abs y))
  LCM (x :: TInt) (y :: Nat) = UnPos (Mul (Div (Abs x) (GCD x y)) (Abs y))
  LCM (x :: TInt) (y :: TInt) = UnPos (Mul (Div (Abs x) (GCD x y)) (Abs y))

-- | Reduce a type-level rational into its canonical form
--
-- @since 0.1.4
type family Simplify (x :: Rat) :: Rat where
  Simplify ((x :: Nat) ':% y) = (Quot ('Pos x) (GCD x y)) ':% (Quot y (GCD x y))
  Simplify ((x :: TInt) ':% y) = (Quot x (GCD x y)) ':% (Quot y (GCD x y))

-- | Exponentiation of a type-level number by an integer
--
-- @since 0.1.4
type family Exp (x :: k1) (y :: k2) :: ExpK k1 k2 where
  Exp (x :: Nat)  (y :: Nat) = x G.^ y
  Exp ('Pos x)    (y :: Nat) = 'Pos (x G.^ y)
  Exp ('Neg x)    (y :: Nat) = ExpAux ('Pos 1) ('Neg x) y
  Exp (x :: Rat)  (y :: Nat) = Simplify (ExpAux (1 ':% 1) x y)
  Exp (x :: Rat)  ('Pos y)   = Simplify (ExpAux (1 ':% 1) x y)
  Exp (x :: Rat)  ('Neg y)   = Simplify (Recip (ExpAux (1 ':% 1) x y))
  Exp x           ('Pos y)   = Simplify (ExpAux (1 ':% 1) (x ':% 1) y)
  Exp x           ('Neg y)   = Simplify (Recip (ExpAux (1 ':% 1) (x ':% 1) y))

type family ExpAux (acc :: k1) (x :: k1) (y :: Nat) :: k1 where
  ExpAux acc _ 0 = acc
  ExpAux acc x y = ExpAux (Mul x acc) x (Sub y 1)

-- | Round a type-level number towards zero
--
-- @since 0.1.4
type family Truncate (x :: k) :: TInt where
  Truncate (x :: Nat)          = 'Pos x
  Truncate (x :: TInt)         = x
  Truncate (x ':% 0)           = G.TypeError ('G.Text ("The denominator must not be 0"))
  Truncate ((x :: Nat) ':% y)  = 'Pos (Quot x y)
  Truncate ((x :: TInt) ':% y) = Quot x y

-- | Round a type-level number towards negative infinity
--
-- @since 0.1.4
type family Floor (x :: k) :: TInt where
  Floor (x :: Nat)          = 'Pos x
  Floor (x :: TInt)         = x
  Floor (x ':% 0)           = G.TypeError ('G.Text ("The denominator must not be 0"))
  Floor ((x :: Nat) ':% y)  = 'Pos (Div x y)
  Floor ((x :: TInt) ':% y) = Div x y

-- | Round a type-level number towards positive infinity
--
-- @since 0.1.4
type family Ceiling (x :: k) :: TInt where
  Ceiling (x :: Nat)  = 'Pos x
  Ceiling (x :: TInt) = x
  Ceiling (x ':% 0)   = G.TypeError ('G.Text ("The denominator must not be 0"))
  Ceiling (x ':% y)   = Add 1 (Floor (Sub x 1 ':% y))

-- | The floor of the logarithm of a type-level number
--   NB. unlike 'G.Log2', @Log n 0@ here is a type error.
--
-- @since 0.1.4
type family IntLog (n :: Nat) (x :: k) :: TInt where
  IntLog 0 _ = G.TypeError ('G.Text "Invalid IntLog base: 0")
  IntLog 1 _ = G.TypeError ('G.Text "Invalid IntLog base: 1")
  IntLog _ 0 = G.TypeError ('G.Text "IntLog n 0 is infinite")
  IntLog _ ('Pos 0) = G.TypeError ('G.Text "IntLog n 0 is infinite")
  IntLog _ ('Neg 0) = G.TypeError ('G.Text "IntLog n 0 is infinite")
  IntLog _ (0 ':% _) = G.TypeError ('G.Text "IntLog n 0 is infinite")
  IntLog _ (_ ':% 0) = G.TypeError ('G.Text "IntLog parameter has zero denominator")
  IntLog _ ('Neg _) = G.TypeError ('G.Text "IntLog of a negative does not exist")
  IntLog n (x :: Nat) = IntLog n ('Pos x)
  IntLog n ('Pos x) = IntLogAux n ('Pos x)
  IntLog n (x :: Rat) =
    If (Floor x DTE.== 'Pos 0)  -- Using DTE here to avoid a module cycle
      (NegLogFudge n x (Negate (IntLogAux n (Floor (Recip x)))))
      (IntLog n (Floor x))

type family IntLogAux (n :: Nat) (x :: TInt) :: TInt where
  IntLogAux n ('Pos 0) = 'Neg 1
  IntLogAux n ('Pos 1) = 'Pos 0
  IntLogAux n ('Pos x) = Add 1 (IntLogAux n (Div ('Pos x) n))

-- For rationals x/y >= 1, 
-- IntLog(x/y) = IntLog(Floor(x/y))
--
-- For rationals x/y < 1, consider the reciprocal y/x
-- IntLog(x/y) = Floor(Log(x/y)) = -Ceiling(Log(y/x)) = -Ceiling(Log(Floor(y/x)))
--
-- To convert IntLog(..) into Ceiling(Log(..)), we use "NegLogFudge"

-- Convert the implicit floor in IntLog(y/x) into a ceiling
type family NegLogFudge (n :: Nat) (x :: Rat) (lg :: TInt) where
  NegLogFudge n x lg =
    If (Simplify (Exp n lg) DTE.== Simplify x)
      lg
      (Sub lg 1)
