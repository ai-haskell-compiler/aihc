{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}

module Type.Data.Num.Decimal.Number (
    Decimal, decimal,
    Dec, Zero, Pos, Neg, EndAsc, (:<), EndDesc, (:>),
    Singleton(..), singleton, singletonFromProxy,
    integerFromSingleton, integralFromSingleton,
    integralFromProxy,

    Integer(..), Natural(..),
    Positive(..), Negative(..),

    reifyIntegral, reifyNatural,
    reifyPositive, reifyNegative,
    reifyPos, reifyNeg,

    Digits(..),

    (:+:), (:-:), (:*:),
    Pred, Succ, Compare, IsEven, Pow2, Log2Ceil,

    (:<:), (:<=:), (:==:),
    (:>:), (:>=:), (:/=:),

    FromUnary, ToUnary,
    ToUnaryAcc, UnaryAcc, -- for Proof
    ) where

import qualified Type.Data.Num.Decimal.Digit as Digit
import qualified Type.Data.Num.Unary.Literal as UnaryLit
import qualified Type.Data.Num.Unary as Unary
import qualified Type.Data.Num as Op
import qualified Type.Data.Ord as Ord
import qualified Type.Base.Proxy as Proxy

import Type.Data.Num.Decimal.Digit
          (Dec0, Dec1, Dec2, Dec3, Dec4, Dec5, Dec6, Dec7, Dec8, Dec9)
import Type.Data.Bool (If, False, True)
import Type.Data.Ord (LT, GT, EQ)
import Type.Base.Proxy (Proxy(Proxy))

import Data.Maybe.HT (toMaybe)
import Data.Tuple.HT (swap)
import qualified Data.List as List

import Text.Printf (printf)

import qualified Prelude as P
import Prelude hiding (Integer)


-- | Representation name for decimal type level numbers.
data Decimal

-- | The wrapper type for decimal type level numbers.
data Dec x
data Zero
data Neg x xs
data Pos x xs


infixl 9 :<

data ds :< d
{-
instance (Show ds, Show d) => Show (ds :< d) where
    show _ = show (undefined :: ds) ++ show (undefined :: d)
-}

infixr 9 :>

data d :> ds
{-
instance (Show ds, Show d) => Show (d :> ds) where
    show _ = show (undefined :: d) ++ show (undefined :: ds)
-}


-- | The terminator type for ascending decimal digit lists.
{-
we could add a type parameter to EndAsc
in order to assert non-empty ascending lists
-}
data EndAsc
instance Show EndAsc where
    show _ = ""

-- | The terminator type for descending decimal digit lists.
data EndDesc
instance Show EndDesc where
    show _ = ""


instance Op.Representation Decimal where
    reifyIntegral _ i k = reifyIntegral i (k . decimal)

decimal :: Proxy n -> Proxy (Dec n)
decimal Proxy = Proxy

stripDec :: Proxy (Dec n) -> Proxy n
stripDec Proxy = Proxy


instance Integer a => Proxy.Show (Dec a) where
   showsPrec prec =
      (\n -> showParen (prec>10) (showString (printf "decimal d%d" n)))
       . integerFromSingleton . singletonFromProxy . stripDec


reifyIntegral :: P.Integer -> (forall s. Integer s => Proxy s -> w) -> w
reifyIntegral n f =
    if n < 0
      then
          case reverse $ digits $ negate n of
              [] -> error "negative means non-zero"
              x:xs ->
                  Digit.reifyPos x (\d -> go xs (\ds -> f (negDigits d ds)))
      else
          case reverse $ digits n of
              [] -> f (Proxy :: Proxy Zero)
              x:xs ->
                  Digit.reifyPos x (\d -> go xs (\ds -> f (posDigits d ds)))
  where
   go :: [P.Integer] -> (forall s. Digits s => Proxy s -> w) -> w
   go [] k = k (Proxy :: Proxy EndDesc)
   go (j:js) k = Digit.reify j (\d -> go js (\ds -> k (consDigits d ds)))


negDigits :: Proxy d -> Proxy ds -> Proxy (Neg d ds)
negDigits Proxy Proxy = Proxy

posDigits :: Proxy d -> Proxy ds -> Proxy (Pos d ds)
posDigits Proxy Proxy = Proxy

consDigits :: Proxy d -> Proxy ds -> Proxy (d :> ds)
consDigits Proxy Proxy = Proxy


digits :: P.Integer -> [P.Integer]
digits =
    List.unfoldr (\n -> toMaybe (n/=0) (swap $ quotRem n 10))


reifyPos ::
    P.Integer ->
    (forall x xs. (Digit.Pos x, Digits xs) => Proxy (Pos x xs) -> a) ->
    Maybe a
reifyPos n f =
    reifyIntegral n
        (runCont $ switch reject reject (accept f))

reifyNeg ::
    P.Integer ->
    (forall x xs. (Digit.Pos x, Digits xs) => Proxy (Neg x xs) -> a) ->
    Maybe a
reifyNeg n f =
    reifyIntegral n
        (runCont $ switch reject (accept f) reject)

reifyPositive ::
    P.Integer -> (forall s. (Positive s) => Proxy s -> a) -> Maybe a
reifyPositive n f = reifyPos n f

reifyNegative ::
    P.Integer -> (forall s. (Negative s) => Proxy s -> a) -> Maybe a
reifyNegative n f = reifyNeg n f

reifyNatural ::
    P.Integer -> (forall s. (Natural s) => Proxy s -> a) -> Maybe a
reifyNatural n f =
    reifyIntegral n
        (runCont $ switch (accept f) reject (accept f))

newtype Cont a s = Cont {runCont :: Proxy s -> Maybe a}

accept :: (Proxy s -> a) -> Cont a s
accept f = Cont (Just . f)

reject :: Cont a s
reject = Cont (const Nothing)


instance Integer x => Op.Integer (Dec x) where
    singleton = singletonToGeneric singleton
    type Repr (Dec x) = Decimal

singletonToGeneric :: Singleton x -> Op.Singleton (Dec x)
singletonToGeneric (Singleton n) = Op.Singleton n


class Integer n where
    switch ::
        f Zero ->
        (forall x xs. (Digit.Pos x, Digits xs) => f (Neg x xs)) ->
        (forall x xs. (Digit.Pos x, Digits xs) => f (Pos x xs)) ->
        f n

instance Integer Zero where
    switch x _ _ = x
instance (Digit.Pos x, Digits xs) => Integer (Neg x xs) where
    switch _ x _ = x
instance (Digit.Pos x, Digits xs) => Integer (Pos x xs) where
    switch _ _ x = x


class Integer n => Natural n where
    switchNat ::
        f Zero ->
        (forall x xs. (Digit.Pos x, Digits xs) => f (Pos x xs)) ->
        f n

instance Natural Zero where
    switchNat x _ = x
instance (Digit.Pos x, Digits xs) => Natural (Pos x xs) where
    switchNat _ x = x


class Natural n => Positive n where
    switchPos ::
        (forall x xs. (Digit.Pos x, Digits xs) => f (Pos x xs)) ->
        f n

instance (Digit.Pos x, Digits xs) => Positive (Pos x xs) where
    switchPos x = x


class Integer n => Negative n where
    switchNeg ::
        (forall x xs. (Digit.Pos x, Digits xs) => f (Neg x xs)) ->
        f n

instance (Digit.Pos x, Digits xs) => Negative (Neg x xs) where
    switchNeg x = x


newtype Singleton x = Singleton P.Integer

singleton :: (Integer x) => Singleton x
singleton =
    switch
        (Singleton 0)
        (withProxy $ \(Digit.Singleton n) proxy ->
             negate (fromDigits (fromIntegral n) proxy))
        (withProxy $ \(Digit.Singleton n) proxy ->
             fromDigits (fromIntegral n) proxy)

withProxy ::
    (Digit.C x) =>
    (Digit.Singleton x -> Proxy xs -> P.Integer) -> Singleton (cons x xs)
withProxy f = Singleton $ f Digit.singleton Proxy

integerFromSingleton :: (Integer n) => Singleton n -> P.Integer
integerFromSingleton (Singleton n) = n

integralFromSingleton :: (Integer n, Num a) => Singleton n -> a
integralFromSingleton = fromInteger . integerFromSingleton

singletonFromProxy :: (Integer n) => Proxy n -> Singleton n
singletonFromProxy Proxy = singleton

integralFromProxy :: (Integer n, Num a) => Proxy n -> a
integralFromProxy = integralFromSingleton . singletonFromProxy


class Digits xs where
    switchDigits ::
        f EndDesc ->
        (forall xh xl. (Digit.C xh, Digits xl) => f (xh :> xl)) ->
        f xs
instance Digits EndDesc where
    switchDigits x _ = x
instance (Digit.C xh, Digits xl) => Digits (xh :> xl) where
    switchDigits _ x = x

newtype
    FromDigits y xs =
        FromDigits {runFromDigits :: y -> Proxy xs -> y}

fromDigits :: (Num y, Digits xs) => y -> Proxy xs -> y
fromDigits =
    runFromDigits $
    switchDigits
        (FromDigits $ \acc _ -> acc)
        (FromDigits $ \acc ->
         withDigits $ \(Digit.Singleton n) xl ->
             fromDigits (10*acc + fromIntegral n) xl)

withDigits ::
    (Digit.C xh) =>
    (Digit.Singleton xh -> Proxy xl -> a) -> Proxy (xh :> xl) -> a
withDigits f Proxy = f Digit.singleton Proxy


type Id x = x

type family NormalizePos x
type instance NormalizePos EndDesc = Zero
type instance NormalizePos (Dec0 :> xl) = NormalizePos xl
type instance NormalizePos (Dec1 :> xl) = Pos Dec1 xl
type instance NormalizePos (Dec2 :> xl) = Pos Dec2 xl
type instance NormalizePos (Dec3 :> xl) = Pos Dec3 xl
type instance NormalizePos (Dec4 :> xl) = Pos Dec4 xl
type instance NormalizePos (Dec5 :> xl) = Pos Dec5 xl
type instance NormalizePos (Dec6 :> xl) = Pos Dec6 xl
type instance NormalizePos (Dec7 :> xl) = Pos Dec7 xl
type instance NormalizePos (Dec8 :> xl) = Pos Dec8 xl
type instance NormalizePos (Dec9 :> xl) = Pos Dec9 xl

type family NormalizeNeg x
type instance NormalizeNeg EndDesc = Zero
type instance NormalizeNeg (Dec0 :> xl) = NormalizeNeg xl
type instance NormalizeNeg (Dec1 :> xl) = Neg Dec1 xl
type instance NormalizeNeg (Dec2 :> xl) = Neg Dec2 xl
type instance NormalizeNeg (Dec3 :> xl) = Neg Dec3 xl
type instance NormalizeNeg (Dec4 :> xl) = Neg Dec4 xl
type instance NormalizeNeg (Dec5 :> xl) = Neg Dec5 xl
type instance NormalizeNeg (Dec6 :> xl) = Neg Dec6 xl
type instance NormalizeNeg (Dec7 :> xl) = Neg Dec7 xl
type instance NormalizeNeg (Dec8 :> xl) = Neg Dec8 xl
type instance NormalizeNeg (Dec9 :> xl) = Neg Dec9 xl

type family Ascending x y
type instance Ascending y EndDesc = y
type instance Ascending y (xh :> xl) = Ascending (y :< xh) xl

type AscendingNonEmpty x xs = Ascending (EndAsc:<x) xs

type family Descending x y
type instance Descending EndAsc y = y
type instance Descending (xh :< xl) y = Descending xh (xl :> y)

type NormalizePosDesc xs = NormalizePos (Descending xs EndDesc)
type NormalizeNegDesc xs = NormalizeNeg (Descending xs EndDesc)


-- type family Op.IsPositive x
type instance Op.IsPositive (Dec x) = IsPositive x
type family IsPositive x
type instance IsPositive (Neg _x _xs) = False
type instance IsPositive Zero         = False
type instance IsPositive (Pos _x _xs) = True

-- type family Op.IsZero x
type instance Op.IsZero (Dec x) = IsZero x
type family IsZero x
type instance IsZero (Neg _x _xs) = False
type instance IsZero Zero         = True
type instance IsZero (Pos _x _xs) = False

-- type family Op.IsNegative x
type instance Op.IsNegative (Dec x) = IsNegative x
type family IsNegative x
type instance IsNegative (Neg _x _xs) = True
type instance IsNegative Zero         = False
type instance IsNegative (Pos _x _xs) = False

-- type family Op.IsNatural x
type instance Op.IsNatural (Dec x) = IsNatural x
type family IsNatural x
type instance IsNatural (Neg _x _xs) = False
type instance IsNatural Zero         = True
type instance IsNatural (Pos _x _xs) = True

-- type family Op.Negate x
type instance Op.Negate (Dec x) = Dec (Negate x)

type family Negate x
type instance Negate Zero       = Zero
type instance Negate (Neg x xs) = Pos x xs
type instance Negate (Pos x xs) = Neg x xs

-- type family Op.One r
type instance Op.One Decimal = Dec One
type One = Pos Dec1 EndDesc

-- type family Op.Succ x
type instance Op.Succ (Dec x) = Dec (Succ x)

type family Succ x
type instance Succ Zero = One
type instance Succ (Pos x xs) =
        NormalizePosDesc (SuccAsc (AscendingNonEmpty x xs))
type instance Succ (Neg x xs) =
        NormalizeNegDesc (PredAsc (AscendingNonEmpty x xs))

type family SuccAsc x
type instance SuccAsc EndAsc = EndAsc :< Dec1
type instance SuccAsc (x :< Dec0) = x :< Dec1
type instance SuccAsc (x :< Dec1) = x :< Dec2
type instance SuccAsc (x :< Dec2) = x :< Dec3
type instance SuccAsc (x :< Dec3) = x :< Dec4
type instance SuccAsc (x :< Dec4) = x :< Dec5
type instance SuccAsc (x :< Dec5) = x :< Dec6
type instance SuccAsc (x :< Dec6) = x :< Dec7
type instance SuccAsc (x :< Dec7) = x :< Dec8
type instance SuccAsc (x :< Dec8) = x :< Dec9
type instance SuccAsc (x :< Dec9) = SuccAsc x :< Dec0

-- type family Op.Pred x
type instance Op.Pred (Dec x) = Dec (Pred x)

type family Pred x
type instance Pred Zero = Neg Dec1 EndDesc
type instance Pred (Neg x xs) =
        NormalizeNegDesc (SuccAsc (AscendingNonEmpty x xs))
type instance Pred (Pos x xs) =
        NormalizePosDesc (PredAsc (AscendingNonEmpty x xs))

type family PredAsc x
type instance PredAsc (x :< Dec0) = PredAsc x :< Dec9
type instance PredAsc (x :< Dec1) = x :< Dec0
type instance PredAsc (x :< Dec2) = x :< Dec1
type instance PredAsc (x :< Dec3) = x :< Dec2
type instance PredAsc (x :< Dec4) = x :< Dec3
type instance PredAsc (x :< Dec5) = x :< Dec4
type instance PredAsc (x :< Dec6) = x :< Dec5
type instance PredAsc (x :< Dec7) = x :< Dec6
type instance PredAsc (x :< Dec8) = x :< Dec7
type instance PredAsc (x :< Dec9) = x :< Dec8

--------------------
-- Addition

type family AddDigit x y
-- putStr $ unlines $ concat $ [ [ "type instance AddDigit Dec" ++ show x ++ " Dec" ++ show y ++ " = Dec" ++ show ((x+y) `mod` 10) | y <- [0..9] ] ++ [ "" ] | x <- [0..9] ]
type instance AddDigit Dec0 Dec0 = Dec0
type instance AddDigit Dec0 Dec1 = Dec1
type instance AddDigit Dec0 Dec2 = Dec2
type instance AddDigit Dec0 Dec3 = Dec3
type instance AddDigit Dec0 Dec4 = Dec4
type instance AddDigit Dec0 Dec5 = Dec5
type instance AddDigit Dec0 Dec6 = Dec6
type instance AddDigit Dec0 Dec7 = Dec7
type instance AddDigit Dec0 Dec8 = Dec8
type instance AddDigit Dec0 Dec9 = Dec9

type instance AddDigit Dec1 Dec0 = Dec1
type instance AddDigit Dec1 Dec1 = Dec2
type instance AddDigit Dec1 Dec2 = Dec3
type instance AddDigit Dec1 Dec3 = Dec4
type instance AddDigit Dec1 Dec4 = Dec5
type instance AddDigit Dec1 Dec5 = Dec6
type instance AddDigit Dec1 Dec6 = Dec7
type instance AddDigit Dec1 Dec7 = Dec8
type instance AddDigit Dec1 Dec8 = Dec9
type instance AddDigit Dec1 Dec9 = Dec0

type instance AddDigit Dec2 Dec0 = Dec2
type instance AddDigit Dec2 Dec1 = Dec3
type instance AddDigit Dec2 Dec2 = Dec4
type instance AddDigit Dec2 Dec3 = Dec5
type instance AddDigit Dec2 Dec4 = Dec6
type instance AddDigit Dec2 Dec5 = Dec7
type instance AddDigit Dec2 Dec6 = Dec8
type instance AddDigit Dec2 Dec7 = Dec9
type instance AddDigit Dec2 Dec8 = Dec0
type instance AddDigit Dec2 Dec9 = Dec1

type instance AddDigit Dec3 Dec0 = Dec3
type instance AddDigit Dec3 Dec1 = Dec4
type instance AddDigit Dec3 Dec2 = Dec5
type instance AddDigit Dec3 Dec3 = Dec6
type instance AddDigit Dec3 Dec4 = Dec7
type instance AddDigit Dec3 Dec5 = Dec8
type instance AddDigit Dec3 Dec6 = Dec9
type instance AddDigit Dec3 Dec7 = Dec0
type instance AddDigit Dec3 Dec8 = Dec1
type instance AddDigit Dec3 Dec9 = Dec2

type instance AddDigit Dec4 Dec0 = Dec4
type instance AddDigit Dec4 Dec1 = Dec5
type instance AddDigit Dec4 Dec2 = Dec6
type instance AddDigit Dec4 Dec3 = Dec7
type instance AddDigit Dec4 Dec4 = Dec8
type instance AddDigit Dec4 Dec5 = Dec9
type instance AddDigit Dec4 Dec6 = Dec0
type instance AddDigit Dec4 Dec7 = Dec1
type instance AddDigit Dec4 Dec8 = Dec2
type instance AddDigit Dec4 Dec9 = Dec3

type instance AddDigit Dec5 Dec0 = Dec5
type instance AddDigit Dec5 Dec1 = Dec6
type instance AddDigit Dec5 Dec2 = Dec7
type instance AddDigit Dec5 Dec3 = Dec8
type instance AddDigit Dec5 Dec4 = Dec9
type instance AddDigit Dec5 Dec5 = Dec0
type instance AddDigit Dec5 Dec6 = Dec1
type instance AddDigit Dec5 Dec7 = Dec2
type instance AddDigit Dec5 Dec8 = Dec3
type instance AddDigit Dec5 Dec9 = Dec4

type instance AddDigit Dec6 Dec0 = Dec6
type instance AddDigit Dec6 Dec1 = Dec7
type instance AddDigit Dec6 Dec2 = Dec8
type instance AddDigit Dec6 Dec3 = Dec9
type instance AddDigit Dec6 Dec4 = Dec0
type instance AddDigit Dec6 Dec5 = Dec1
type instance AddDigit Dec6 Dec6 = Dec2
type instance AddDigit Dec6 Dec7 = Dec3
type instance AddDigit Dec6 Dec8 = Dec4
type instance AddDigit Dec6 Dec9 = Dec5

type instance AddDigit Dec7 Dec0 = Dec7
type instance AddDigit Dec7 Dec1 = Dec8
type instance AddDigit Dec7 Dec2 = Dec9
type instance AddDigit Dec7 Dec3 = Dec0
type instance AddDigit Dec7 Dec4 = Dec1
type instance AddDigit Dec7 Dec5 = Dec2
type instance AddDigit Dec7 Dec6 = Dec3
type instance AddDigit Dec7 Dec7 = Dec4
type instance AddDigit Dec7 Dec8 = Dec5
type instance AddDigit Dec7 Dec9 = Dec6

type instance AddDigit Dec8 Dec0 = Dec8
type instance AddDigit Dec8 Dec1 = Dec9
type instance AddDigit Dec8 Dec2 = Dec0
type instance AddDigit Dec8 Dec3 = Dec1
type instance AddDigit Dec8 Dec4 = Dec2
type instance AddDigit Dec8 Dec5 = Dec3
type instance AddDigit Dec8 Dec6 = Dec4
type instance AddDigit Dec8 Dec7 = Dec5
type instance AddDigit Dec8 Dec8 = Dec6
type instance AddDigit Dec8 Dec9 = Dec7

type instance AddDigit Dec9 Dec0 = Dec9
type instance AddDigit Dec9 Dec1 = Dec0
type instance AddDigit Dec9 Dec2 = Dec1
type instance AddDigit Dec9 Dec3 = Dec2
type instance AddDigit Dec9 Dec4 = Dec3
type instance AddDigit Dec9 Dec5 = Dec4
type instance AddDigit Dec9 Dec6 = Dec5
type instance AddDigit Dec9 Dec7 = Dec6
type instance AddDigit Dec9 Dec8 = Dec7
type instance AddDigit Dec9 Dec9 = Dec8

-- | If adding @x@ and @y@ would not carry, then
--   @AddCarry x y z@ evaluates to @z@.  Otherwise,
--   @AddCarry x y z@ evaluates to @SuccAsc z@
type family AddCarry x y z
-- putStr $ unlines $ concat $ [ [ "type instance AddCarry Dec" ++ show x ++ " Dec" ++ show y ++ " x = " ++ (if x + y >= 10 then "SuccAsc" else "Id") ++ " x" | y <- [0..9] ] ++ [ "" ] | x <- [0..9] ]
type instance AddCarry Dec0 Dec0 x = Id x
type instance AddCarry Dec0 Dec1 x = Id x
type instance AddCarry Dec0 Dec2 x = Id x
type instance AddCarry Dec0 Dec3 x = Id x
type instance AddCarry Dec0 Dec4 x = Id x
type instance AddCarry Dec0 Dec5 x = Id x
type instance AddCarry Dec0 Dec6 x = Id x
type instance AddCarry Dec0 Dec7 x = Id x
type instance AddCarry Dec0 Dec8 x = Id x
type instance AddCarry Dec0 Dec9 x = Id x

type instance AddCarry Dec1 Dec0 x = Id x
type instance AddCarry Dec1 Dec1 x = Id x
type instance AddCarry Dec1 Dec2 x = Id x
type instance AddCarry Dec1 Dec3 x = Id x
type instance AddCarry Dec1 Dec4 x = Id x
type instance AddCarry Dec1 Dec5 x = Id x
type instance AddCarry Dec1 Dec6 x = Id x
type instance AddCarry Dec1 Dec7 x = Id x
type instance AddCarry Dec1 Dec8 x = Id x
type instance AddCarry Dec1 Dec9 x = SuccAsc x

type instance AddCarry Dec2 Dec0 x = Id x
type instance AddCarry Dec2 Dec1 x = Id x
type instance AddCarry Dec2 Dec2 x = Id x
type instance AddCarry Dec2 Dec3 x = Id x
type instance AddCarry Dec2 Dec4 x = Id x
type instance AddCarry Dec2 Dec5 x = Id x
type instance AddCarry Dec2 Dec6 x = Id x
type instance AddCarry Dec2 Dec7 x = Id x
type instance AddCarry Dec2 Dec8 x = SuccAsc x
type instance AddCarry Dec2 Dec9 x = SuccAsc x

type instance AddCarry Dec3 Dec0 x = Id x
type instance AddCarry Dec3 Dec1 x = Id x
type instance AddCarry Dec3 Dec2 x = Id x
type instance AddCarry Dec3 Dec3 x = Id x
type instance AddCarry Dec3 Dec4 x = Id x
type instance AddCarry Dec3 Dec5 x = Id x
type instance AddCarry Dec3 Dec6 x = Id x
type instance AddCarry Dec3 Dec7 x = SuccAsc x
type instance AddCarry Dec3 Dec8 x = SuccAsc x
type instance AddCarry Dec3 Dec9 x = SuccAsc x

type instance AddCarry Dec4 Dec0 x = Id x
type instance AddCarry Dec4 Dec1 x = Id x
type instance AddCarry Dec4 Dec2 x = Id x
type instance AddCarry Dec4 Dec3 x = Id x
type instance AddCarry Dec4 Dec4 x = Id x
type instance AddCarry Dec4 Dec5 x = Id x
type instance AddCarry Dec4 Dec6 x = SuccAsc x
type instance AddCarry Dec4 Dec7 x = SuccAsc x
type instance AddCarry Dec4 Dec8 x = SuccAsc x
type instance AddCarry Dec4 Dec9 x = SuccAsc x

type instance AddCarry Dec5 Dec0 x = Id x
type instance AddCarry Dec5 Dec1 x = Id x
type instance AddCarry Dec5 Dec2 x = Id x
type instance AddCarry Dec5 Dec3 x = Id x
type instance AddCarry Dec5 Dec4 x = Id x
type instance AddCarry Dec5 Dec5 x = SuccAsc x
type instance AddCarry Dec5 Dec6 x = SuccAsc x
type instance AddCarry Dec5 Dec7 x = SuccAsc x
type instance AddCarry Dec5 Dec8 x = SuccAsc x
type instance AddCarry Dec5 Dec9 x = SuccAsc x

type instance AddCarry Dec6 Dec0 x = Id x
type instance AddCarry Dec6 Dec1 x = Id x
type instance AddCarry Dec6 Dec2 x = Id x
type instance AddCarry Dec6 Dec3 x = Id x
type instance AddCarry Dec6 Dec4 x = SuccAsc x
type instance AddCarry Dec6 Dec5 x = SuccAsc x
type instance AddCarry Dec6 Dec6 x = SuccAsc x
type instance AddCarry Dec6 Dec7 x = SuccAsc x
type instance AddCarry Dec6 Dec8 x = SuccAsc x
type instance AddCarry Dec6 Dec9 x = SuccAsc x

type instance AddCarry Dec7 Dec0 x = Id x
type instance AddCarry Dec7 Dec1 x = Id x
type instance AddCarry Dec7 Dec2 x = Id x
type instance AddCarry Dec7 Dec3 x = SuccAsc x
type instance AddCarry Dec7 Dec4 x = SuccAsc x
type instance AddCarry Dec7 Dec5 x = SuccAsc x
type instance AddCarry Dec7 Dec6 x = SuccAsc x
type instance AddCarry Dec7 Dec7 x = SuccAsc x
type instance AddCarry Dec7 Dec8 x = SuccAsc x
type instance AddCarry Dec7 Dec9 x = SuccAsc x

type instance AddCarry Dec8 Dec0 x = Id x
type instance AddCarry Dec8 Dec1 x = Id x
type instance AddCarry Dec8 Dec2 x = SuccAsc x
type instance AddCarry Dec8 Dec3 x = SuccAsc x
type instance AddCarry Dec8 Dec4 x = SuccAsc x
type instance AddCarry Dec8 Dec5 x = SuccAsc x
type instance AddCarry Dec8 Dec6 x = SuccAsc x
type instance AddCarry Dec8 Dec7 x = SuccAsc x
type instance AddCarry Dec8 Dec8 x = SuccAsc x
type instance AddCarry Dec8 Dec9 x = SuccAsc x

type instance AddCarry Dec9 Dec0 x = Id x
type instance AddCarry Dec9 Dec1 x = SuccAsc x
type instance AddCarry Dec9 Dec2 x = SuccAsc x
type instance AddCarry Dec9 Dec3 x = SuccAsc x
type instance AddCarry Dec9 Dec4 x = SuccAsc x
type instance AddCarry Dec9 Dec5 x = SuccAsc x
type instance AddCarry Dec9 Dec6 x = SuccAsc x
type instance AddCarry Dec9 Dec7 x = SuccAsc x
type instance AddCarry Dec9 Dec8 x = SuccAsc x
type instance AddCarry Dec9 Dec9 x = SuccAsc x

type family AddAsc x y
type instance AddAsc EndAsc y = y
type instance AddAsc (xh :< xl) EndAsc = xh :< xl
type instance AddAsc (xh :< xl) (yh :< yl) =
        AddCarry xl yl (AddAsc xh yh) :< AddDigit xl yl

type AddPos x xs y ys =
        NormalizePosDesc
           (AddAsc (AscendingNonEmpty x xs) (AscendingNonEmpty y ys))

-- type family x Op.:+: y
type instance Dec x Op.:+: Dec y = Dec (x :+: y)

type family x :+: y
type instance (Zero    ) :+: y          = y
type instance (Pos x xs) :+: (Zero    ) = Pos x xs
type instance (Neg x xs) :+: (Zero    ) = Neg x xs
type instance (Pos x xs) :+: (Pos y ys) = AddPos x xs y ys
type instance (Neg x xs) :+: (Neg y ys) = Negate (AddPos x xs y ys)
type instance (Pos x xs) :+: (Neg y ys) = SubPos x xs y ys
type instance (Neg x xs) :+: (Pos y ys) = SubPos y ys x xs

--------------------
-- Subtraction

type family SubDigit x y
-- putStr $ unlines $ concat $ [ [ "type instance SubDigit Dec" ++ show x ++ " Dec" ++ show y ++ " = Dec" ++ show ((x-y) `mod` 10) | y <- [0..9] ] ++ [ "" ] | x <- [0..9] ]
type instance SubDigit Dec0 Dec0 = Dec0
type instance SubDigit Dec0 Dec1 = Dec9
type instance SubDigit Dec0 Dec2 = Dec8
type instance SubDigit Dec0 Dec3 = Dec7
type instance SubDigit Dec0 Dec4 = Dec6
type instance SubDigit Dec0 Dec5 = Dec5
type instance SubDigit Dec0 Dec6 = Dec4
type instance SubDigit Dec0 Dec7 = Dec3
type instance SubDigit Dec0 Dec8 = Dec2
type instance SubDigit Dec0 Dec9 = Dec1

type instance SubDigit Dec1 Dec0 = Dec1
type instance SubDigit Dec1 Dec1 = Dec0
type instance SubDigit Dec1 Dec2 = Dec9
type instance SubDigit Dec1 Dec3 = Dec8
type instance SubDigit Dec1 Dec4 = Dec7
type instance SubDigit Dec1 Dec5 = Dec6
type instance SubDigit Dec1 Dec6 = Dec5
type instance SubDigit Dec1 Dec7 = Dec4
type instance SubDigit Dec1 Dec8 = Dec3
type instance SubDigit Dec1 Dec9 = Dec2

type instance SubDigit Dec2 Dec0 = Dec2
type instance SubDigit Dec2 Dec1 = Dec1
type instance SubDigit Dec2 Dec2 = Dec0
type instance SubDigit Dec2 Dec3 = Dec9
type instance SubDigit Dec2 Dec4 = Dec8
type instance SubDigit Dec2 Dec5 = Dec7
type instance SubDigit Dec2 Dec6 = Dec6
type instance SubDigit Dec2 Dec7 = Dec5
type instance SubDigit Dec2 Dec8 = Dec4
type instance SubDigit Dec2 Dec9 = Dec3

type instance SubDigit Dec3 Dec0 = Dec3
type instance SubDigit Dec3 Dec1 = Dec2
type instance SubDigit Dec3 Dec2 = Dec1
type instance SubDigit Dec3 Dec3 = Dec0
type instance SubDigit Dec3 Dec4 = Dec9
type instance SubDigit Dec3 Dec5 = Dec8
type instance SubDigit Dec3 Dec6 = Dec7
type instance SubDigit Dec3 Dec7 = Dec6
type instance SubDigit Dec3 Dec8 = Dec5
type instance SubDigit Dec3 Dec9 = Dec4

type instance SubDigit Dec4 Dec0 = Dec4
type instance SubDigit Dec4 Dec1 = Dec3
type instance SubDigit Dec4 Dec2 = Dec2
type instance SubDigit Dec4 Dec3 = Dec1
type instance SubDigit Dec4 Dec4 = Dec0
type instance SubDigit Dec4 Dec5 = Dec9
type instance SubDigit Dec4 Dec6 = Dec8
type instance SubDigit Dec4 Dec7 = Dec7
type instance SubDigit Dec4 Dec8 = Dec6
type instance SubDigit Dec4 Dec9 = Dec5

type instance SubDigit Dec5 Dec0 = Dec5
type instance SubDigit Dec5 Dec1 = Dec4
type instance SubDigit Dec5 Dec2 = Dec3
type instance SubDigit Dec5 Dec3 = Dec2
type instance SubDigit Dec5 Dec4 = Dec1
type instance SubDigit Dec5 Dec5 = Dec0
type instance SubDigit Dec5 Dec6 = Dec9
type instance SubDigit Dec5 Dec7 = Dec8
type instance SubDigit Dec5 Dec8 = Dec7
type instance SubDigit Dec5 Dec9 = Dec6

type instance SubDigit Dec6 Dec0 = Dec6
type instance SubDigit Dec6 Dec1 = Dec5
type instance SubDigit Dec6 Dec2 = Dec4
type instance SubDigit Dec6 Dec3 = Dec3
type instance SubDigit Dec6 Dec4 = Dec2
type instance SubDigit Dec6 Dec5 = Dec1
type instance SubDigit Dec6 Dec6 = Dec0
type instance SubDigit Dec6 Dec7 = Dec9
type instance SubDigit Dec6 Dec8 = Dec8
type instance SubDigit Dec6 Dec9 = Dec7

type instance SubDigit Dec7 Dec0 = Dec7
type instance SubDigit Dec7 Dec1 = Dec6
type instance SubDigit Dec7 Dec2 = Dec5
type instance SubDigit Dec7 Dec3 = Dec4
type instance SubDigit Dec7 Dec4 = Dec3
type instance SubDigit Dec7 Dec5 = Dec2
type instance SubDigit Dec7 Dec6 = Dec1
type instance SubDigit Dec7 Dec7 = Dec0
type instance SubDigit Dec7 Dec8 = Dec9
type instance SubDigit Dec7 Dec9 = Dec8

type instance SubDigit Dec8 Dec0 = Dec8
type instance SubDigit Dec8 Dec1 = Dec7
type instance SubDigit Dec8 Dec2 = Dec6
type instance SubDigit Dec8 Dec3 = Dec5
type instance SubDigit Dec8 Dec4 = Dec4
type instance SubDigit Dec8 Dec5 = Dec3
type instance SubDigit Dec8 Dec6 = Dec2
type instance SubDigit Dec8 Dec7 = Dec1
type instance SubDigit Dec8 Dec8 = Dec0
type instance SubDigit Dec8 Dec9 = Dec9

type instance SubDigit Dec9 Dec0 = Dec9
type instance SubDigit Dec9 Dec1 = Dec8
type instance SubDigit Dec9 Dec2 = Dec7
type instance SubDigit Dec9 Dec3 = Dec6
type instance SubDigit Dec9 Dec4 = Dec5
type instance SubDigit Dec9 Dec5 = Dec4
type instance SubDigit Dec9 Dec6 = Dec3
type instance SubDigit Dec9 Dec7 = Dec2
type instance SubDigit Dec9 Dec8 = Dec1
type instance SubDigit Dec9 Dec9 = Dec0

-- | If subtracting @y@ from @x@ would not borrow, then
--   @Borrow x y z@ evaluates to @z@.  Otherwise,
--   @Borrow x y z@ evaluates to @PredAsc z@
type family Borrow x y z
-- putStr $ unlines $ concat $ [ [ "type instance Borrow Dec" ++ show x ++ " Dec" ++ show y ++ " x = " ++ (if x < y then "PredAsc" else "Id") ++ " x" | y <- [0..9] ] ++ [ "" ] | x <- [0..9] ]
type instance Borrow Dec0 Dec0 x = Id x
type instance Borrow Dec0 Dec1 x = PredAsc x
type instance Borrow Dec0 Dec2 x = PredAsc x
type instance Borrow Dec0 Dec3 x = PredAsc x
type instance Borrow Dec0 Dec4 x = PredAsc x
type instance Borrow Dec0 Dec5 x = PredAsc x
type instance Borrow Dec0 Dec6 x = PredAsc x
type instance Borrow Dec0 Dec7 x = PredAsc x
type instance Borrow Dec0 Dec8 x = PredAsc x
type instance Borrow Dec0 Dec9 x = PredAsc x

type instance Borrow Dec1 Dec0 x = Id x
type instance Borrow Dec1 Dec1 x = Id x
type instance Borrow Dec1 Dec2 x = PredAsc x
type instance Borrow Dec1 Dec3 x = PredAsc x
type instance Borrow Dec1 Dec4 x = PredAsc x
type instance Borrow Dec1 Dec5 x = PredAsc x
type instance Borrow Dec1 Dec6 x = PredAsc x
type instance Borrow Dec1 Dec7 x = PredAsc x
type instance Borrow Dec1 Dec8 x = PredAsc x
type instance Borrow Dec1 Dec9 x = PredAsc x

type instance Borrow Dec2 Dec0 x = Id x
type instance Borrow Dec2 Dec1 x = Id x
type instance Borrow Dec2 Dec2 x = Id x
type instance Borrow Dec2 Dec3 x = PredAsc x
type instance Borrow Dec2 Dec4 x = PredAsc x
type instance Borrow Dec2 Dec5 x = PredAsc x
type instance Borrow Dec2 Dec6 x = PredAsc x
type instance Borrow Dec2 Dec7 x = PredAsc x
type instance Borrow Dec2 Dec8 x = PredAsc x
type instance Borrow Dec2 Dec9 x = PredAsc x

type instance Borrow Dec3 Dec0 x = Id x
type instance Borrow Dec3 Dec1 x = Id x
type instance Borrow Dec3 Dec2 x = Id x
type instance Borrow Dec3 Dec3 x = Id x
type instance Borrow Dec3 Dec4 x = PredAsc x
type instance Borrow Dec3 Dec5 x = PredAsc x
type instance Borrow Dec3 Dec6 x = PredAsc x
type instance Borrow Dec3 Dec7 x = PredAsc x
type instance Borrow Dec3 Dec8 x = PredAsc x
type instance Borrow Dec3 Dec9 x = PredAsc x

type instance Borrow Dec4 Dec0 x = Id x
type instance Borrow Dec4 Dec1 x = Id x
type instance Borrow Dec4 Dec2 x = Id x
type instance Borrow Dec4 Dec3 x = Id x
type instance Borrow Dec4 Dec4 x = Id x
type instance Borrow Dec4 Dec5 x = PredAsc x
type instance Borrow Dec4 Dec6 x = PredAsc x
type instance Borrow Dec4 Dec7 x = PredAsc x
type instance Borrow Dec4 Dec8 x = PredAsc x
type instance Borrow Dec4 Dec9 x = PredAsc x

type instance Borrow Dec5 Dec0 x = Id x
type instance Borrow Dec5 Dec1 x = Id x
type instance Borrow Dec5 Dec2 x = Id x
type instance Borrow Dec5 Dec3 x = Id x
type instance Borrow Dec5 Dec4 x = Id x
type instance Borrow Dec5 Dec5 x = Id x
type instance Borrow Dec5 Dec6 x = PredAsc x
type instance Borrow Dec5 Dec7 x = PredAsc x
type instance Borrow Dec5 Dec8 x = PredAsc x
type instance Borrow Dec5 Dec9 x = PredAsc x

type instance Borrow Dec6 Dec0 x = Id x
type instance Borrow Dec6 Dec1 x = Id x
type instance Borrow Dec6 Dec2 x = Id x
type instance Borrow Dec6 Dec3 x = Id x
type instance Borrow Dec6 Dec4 x = Id x
type instance Borrow Dec6 Dec5 x = Id x
type instance Borrow Dec6 Dec6 x = Id x
type instance Borrow Dec6 Dec7 x = PredAsc x
type instance Borrow Dec6 Dec8 x = PredAsc x
type instance Borrow Dec6 Dec9 x = PredAsc x

type instance Borrow Dec7 Dec0 x = Id x
type instance Borrow Dec7 Dec1 x = Id x
type instance Borrow Dec7 Dec2 x = Id x
type instance Borrow Dec7 Dec3 x = Id x
type instance Borrow Dec7 Dec4 x = Id x
type instance Borrow Dec7 Dec5 x = Id x
type instance Borrow Dec7 Dec6 x = Id x
type instance Borrow Dec7 Dec7 x = Id x
type instance Borrow Dec7 Dec8 x = PredAsc x
type instance Borrow Dec7 Dec9 x = PredAsc x

type instance Borrow Dec8 Dec0 x = Id x
type instance Borrow Dec8 Dec1 x = Id x
type instance Borrow Dec8 Dec2 x = Id x
type instance Borrow Dec8 Dec3 x = Id x
type instance Borrow Dec8 Dec4 x = Id x
type instance Borrow Dec8 Dec5 x = Id x
type instance Borrow Dec8 Dec6 x = Id x
type instance Borrow Dec8 Dec7 x = Id x
type instance Borrow Dec8 Dec8 x = Id x
type instance Borrow Dec8 Dec9 x = PredAsc x

type instance Borrow Dec9 Dec0 x = Id x
type instance Borrow Dec9 Dec1 x = Id x
type instance Borrow Dec9 Dec2 x = Id x
type instance Borrow Dec9 Dec3 x = Id x
type instance Borrow Dec9 Dec4 x = Id x
type instance Borrow Dec9 Dec5 x = Id x
type instance Borrow Dec9 Dec6 x = Id x
type instance Borrow Dec9 Dec7 x = Id x
type instance Borrow Dec9 Dec8 x = Id x
type instance Borrow Dec9 Dec9 x = Id x

type family SubAsc x y
type instance SubAsc x EndAsc = x
type instance SubAsc (xh :< xl) (yh :< yl) =
        SubAsc (Borrow xl yl xh) yh :< SubDigit xl yl

type family SubOrd c x y
type instance SubOrd GT  x  y = NormalizePosDesc (SubAsc x y)
type instance SubOrd EQ _x _y = Zero
type instance SubOrd LT  x  y = NormalizeNegDesc (SubAsc y x)

type SubCmp x y = SubOrd (CompareAsc x y EQ) x y

type SubPos x xs y ys =
        SubCmp (AscendingNonEmpty x xs) (AscendingNonEmpty y ys)


-- type family x Op.:-: y
type instance Dec x Op.:-: Dec y = Dec (x :-: y)

type x :-: y = x :+: Negate y


--------------------
-- Multiplication

-- type family Mul2 x
type instance Op.Mul2 (Dec x) = Dec (x :+: x)
type Mul2Asc x = AddAsc x x

-- type family x Op.:*: y
type instance (Dec x) Op.:*: (Dec y) = Dec (x :*: y)

type family x :*: y
type instance Zero         :*: _y   = Zero
type instance (Pos _x _xs) :*: Zero = Zero
type instance (Neg _x _xs) :*: Zero = Zero
type instance (Pos x xs) :*: (Pos y ys) = NormalizePosDesc (MulPos x xs y ys)
type instance (Neg x xs) :*: (Neg y ys) = NormalizePosDesc (MulPos x xs y ys)
type instance (Pos x xs) :*: (Neg y ys) = NormalizeNegDesc (MulPos x xs y ys)
type instance (Neg x xs) :*: (Pos y ys) = NormalizeNegDesc (MulPos x xs y ys)

type MulPos x xs y ys =
        MulScaleAsc (AscendingNonEmpty x xs) (AscendingNonEmpty y ys)

{-
type MulPos x xs y ys =
        MulAsc (AscendingNonEmpty x xs) (Pos y ys) EndAsc
-}

type family MulAsc x y z
type instance MulAsc _x Zero       z = z
type instance MulAsc  x (Pos y ys) z =
         MulAsc (Mul2Asc x) (Div2 (Pos y ys))
            (If (IsEven (Pos y ys)) z (AddAsc z x))



-----------
-- Scale

type family MulLo x y
-- putStr $ unlines $ concat $ [ [ "type instance MulLo Dec" ++ show x ++ " Dec" ++ show y ++ " = Dec" ++ show ((x*y) `mod` 10) | y <- [0..9] ] ++ [ "" ] | x <- [0..9] ]

type instance MulLo Dec0 Dec0 = Dec0
type instance MulLo Dec0 Dec1 = Dec0
type instance MulLo Dec0 Dec2 = Dec0
type instance MulLo Dec0 Dec3 = Dec0
type instance MulLo Dec0 Dec4 = Dec0
type instance MulLo Dec0 Dec5 = Dec0
type instance MulLo Dec0 Dec6 = Dec0
type instance MulLo Dec0 Dec7 = Dec0
type instance MulLo Dec0 Dec8 = Dec0
type instance MulLo Dec0 Dec9 = Dec0

type instance MulLo Dec1 Dec0 = Dec0
type instance MulLo Dec1 Dec1 = Dec1
type instance MulLo Dec1 Dec2 = Dec2
type instance MulLo Dec1 Dec3 = Dec3
type instance MulLo Dec1 Dec4 = Dec4
type instance MulLo Dec1 Dec5 = Dec5
type instance MulLo Dec1 Dec6 = Dec6
type instance MulLo Dec1 Dec7 = Dec7
type instance MulLo Dec1 Dec8 = Dec8
type instance MulLo Dec1 Dec9 = Dec9

type instance MulLo Dec2 Dec0 = Dec0
type instance MulLo Dec2 Dec1 = Dec2
type instance MulLo Dec2 Dec2 = Dec4
type instance MulLo Dec2 Dec3 = Dec6
type instance MulLo Dec2 Dec4 = Dec8
type instance MulLo Dec2 Dec5 = Dec0
type instance MulLo Dec2 Dec6 = Dec2
type instance MulLo Dec2 Dec7 = Dec4
type instance MulLo Dec2 Dec8 = Dec6
type instance MulLo Dec2 Dec9 = Dec8

type instance MulLo Dec3 Dec0 = Dec0
type instance MulLo Dec3 Dec1 = Dec3
type instance MulLo Dec3 Dec2 = Dec6
type instance MulLo Dec3 Dec3 = Dec9
type instance MulLo Dec3 Dec4 = Dec2
type instance MulLo Dec3 Dec5 = Dec5
type instance MulLo Dec3 Dec6 = Dec8
type instance MulLo Dec3 Dec7 = Dec1
type instance MulLo Dec3 Dec8 = Dec4
type instance MulLo Dec3 Dec9 = Dec7

type instance MulLo Dec4 Dec0 = Dec0
type instance MulLo Dec4 Dec1 = Dec4
type instance MulLo Dec4 Dec2 = Dec8
type instance MulLo Dec4 Dec3 = Dec2
type instance MulLo Dec4 Dec4 = Dec6
type instance MulLo Dec4 Dec5 = Dec0
type instance MulLo Dec4 Dec6 = Dec4
type instance MulLo Dec4 Dec7 = Dec8
type instance MulLo Dec4 Dec8 = Dec2
type instance MulLo Dec4 Dec9 = Dec6

type instance MulLo Dec5 Dec0 = Dec0
type instance MulLo Dec5 Dec1 = Dec5
type instance MulLo Dec5 Dec2 = Dec0
type instance MulLo Dec5 Dec3 = Dec5
type instance MulLo Dec5 Dec4 = Dec0
type instance MulLo Dec5 Dec5 = Dec5
type instance MulLo Dec5 Dec6 = Dec0
type instance MulLo Dec5 Dec7 = Dec5
type instance MulLo Dec5 Dec8 = Dec0
type instance MulLo Dec5 Dec9 = Dec5

type instance MulLo Dec6 Dec0 = Dec0
type instance MulLo Dec6 Dec1 = Dec6
type instance MulLo Dec6 Dec2 = Dec2
type instance MulLo Dec6 Dec3 = Dec8
type instance MulLo Dec6 Dec4 = Dec4
type instance MulLo Dec6 Dec5 = Dec0
type instance MulLo Dec6 Dec6 = Dec6
type instance MulLo Dec6 Dec7 = Dec2
type instance MulLo Dec6 Dec8 = Dec8
type instance MulLo Dec6 Dec9 = Dec4

type instance MulLo Dec7 Dec0 = Dec0
type instance MulLo Dec7 Dec1 = Dec7
type instance MulLo Dec7 Dec2 = Dec4
type instance MulLo Dec7 Dec3 = Dec1
type instance MulLo Dec7 Dec4 = Dec8
type instance MulLo Dec7 Dec5 = Dec5
type instance MulLo Dec7 Dec6 = Dec2
type instance MulLo Dec7 Dec7 = Dec9
type instance MulLo Dec7 Dec8 = Dec6
type instance MulLo Dec7 Dec9 = Dec3

type instance MulLo Dec8 Dec0 = Dec0
type instance MulLo Dec8 Dec1 = Dec8
type instance MulLo Dec8 Dec2 = Dec6
type instance MulLo Dec8 Dec3 = Dec4
type instance MulLo Dec8 Dec4 = Dec2
type instance MulLo Dec8 Dec5 = Dec0
type instance MulLo Dec8 Dec6 = Dec8
type instance MulLo Dec8 Dec7 = Dec6
type instance MulLo Dec8 Dec8 = Dec4
type instance MulLo Dec8 Dec9 = Dec2

type instance MulLo Dec9 Dec0 = Dec0
type instance MulLo Dec9 Dec1 = Dec9
type instance MulLo Dec9 Dec2 = Dec8
type instance MulLo Dec9 Dec3 = Dec7
type instance MulLo Dec9 Dec4 = Dec6
type instance MulLo Dec9 Dec5 = Dec5
type instance MulLo Dec9 Dec6 = Dec4
type instance MulLo Dec9 Dec7 = Dec3
type instance MulLo Dec9 Dec8 = Dec2
type instance MulLo Dec9 Dec9 = Dec1


type family MulHi x y
-- putStr $ unlines $ concat $ [ [ "type instance MulHi Dec" ++ show x ++ " Dec" ++ show y ++ " = Dec" ++ show ((x*y) `div` 10) | y <- [0..9] ] ++ [ "" ] | x <- [0..9] ]

type instance MulHi Dec0 Dec0 = Dec0
type instance MulHi Dec0 Dec1 = Dec0
type instance MulHi Dec0 Dec2 = Dec0
type instance MulHi Dec0 Dec3 = Dec0
type instance MulHi Dec0 Dec4 = Dec0
type instance MulHi Dec0 Dec5 = Dec0
type instance MulHi Dec0 Dec6 = Dec0
type instance MulHi Dec0 Dec7 = Dec0
type instance MulHi Dec0 Dec8 = Dec0
type instance MulHi Dec0 Dec9 = Dec0

type instance MulHi Dec1 Dec0 = Dec0
type instance MulHi Dec1 Dec1 = Dec0
type instance MulHi Dec1 Dec2 = Dec0
type instance MulHi Dec1 Dec3 = Dec0
type instance MulHi Dec1 Dec4 = Dec0
type instance MulHi Dec1 Dec5 = Dec0
type instance MulHi Dec1 Dec6 = Dec0
type instance MulHi Dec1 Dec7 = Dec0
type instance MulHi Dec1 Dec8 = Dec0
type instance MulHi Dec1 Dec9 = Dec0

type instance MulHi Dec2 Dec0 = Dec0
type instance MulHi Dec2 Dec1 = Dec0
type instance MulHi Dec2 Dec2 = Dec0
type instance MulHi Dec2 Dec3 = Dec0
type instance MulHi Dec2 Dec4 = Dec0
type instance MulHi Dec2 Dec5 = Dec1
type instance MulHi Dec2 Dec6 = Dec1
type instance MulHi Dec2 Dec7 = Dec1
type instance MulHi Dec2 Dec8 = Dec1
type instance MulHi Dec2 Dec9 = Dec1

type instance MulHi Dec3 Dec0 = Dec0
type instance MulHi Dec3 Dec1 = Dec0
type instance MulHi Dec3 Dec2 = Dec0
type instance MulHi Dec3 Dec3 = Dec0
type instance MulHi Dec3 Dec4 = Dec1
type instance MulHi Dec3 Dec5 = Dec1
type instance MulHi Dec3 Dec6 = Dec1
type instance MulHi Dec3 Dec7 = Dec2
type instance MulHi Dec3 Dec8 = Dec2
type instance MulHi Dec3 Dec9 = Dec2

type instance MulHi Dec4 Dec0 = Dec0
type instance MulHi Dec4 Dec1 = Dec0
type instance MulHi Dec4 Dec2 = Dec0
type instance MulHi Dec4 Dec3 = Dec1
type instance MulHi Dec4 Dec4 = Dec1
type instance MulHi Dec4 Dec5 = Dec2
type instance MulHi Dec4 Dec6 = Dec2
type instance MulHi Dec4 Dec7 = Dec2
type instance MulHi Dec4 Dec8 = Dec3
type instance MulHi Dec4 Dec9 = Dec3

type instance MulHi Dec5 Dec0 = Dec0
type instance MulHi Dec5 Dec1 = Dec0
type instance MulHi Dec5 Dec2 = Dec1
type instance MulHi Dec5 Dec3 = Dec1
type instance MulHi Dec5 Dec4 = Dec2
type instance MulHi Dec5 Dec5 = Dec2
type instance MulHi Dec5 Dec6 = Dec3
type instance MulHi Dec5 Dec7 = Dec3
type instance MulHi Dec5 Dec8 = Dec4
type instance MulHi Dec5 Dec9 = Dec4

type instance MulHi Dec6 Dec0 = Dec0
type instance MulHi Dec6 Dec1 = Dec0
type instance MulHi Dec6 Dec2 = Dec1
type instance MulHi Dec6 Dec3 = Dec1
type instance MulHi Dec6 Dec4 = Dec2
type instance MulHi Dec6 Dec5 = Dec3
type instance MulHi Dec6 Dec6 = Dec3
type instance MulHi Dec6 Dec7 = Dec4
type instance MulHi Dec6 Dec8 = Dec4
type instance MulHi Dec6 Dec9 = Dec5

type instance MulHi Dec7 Dec0 = Dec0
type instance MulHi Dec7 Dec1 = Dec0
type instance MulHi Dec7 Dec2 = Dec1
type instance MulHi Dec7 Dec3 = Dec2
type instance MulHi Dec7 Dec4 = Dec2
type instance MulHi Dec7 Dec5 = Dec3
type instance MulHi Dec7 Dec6 = Dec4
type instance MulHi Dec7 Dec7 = Dec4
type instance MulHi Dec7 Dec8 = Dec5
type instance MulHi Dec7 Dec9 = Dec6

type instance MulHi Dec8 Dec0 = Dec0
type instance MulHi Dec8 Dec1 = Dec0
type instance MulHi Dec8 Dec2 = Dec1
type instance MulHi Dec8 Dec3 = Dec2
type instance MulHi Dec8 Dec4 = Dec3
type instance MulHi Dec8 Dec5 = Dec4
type instance MulHi Dec8 Dec6 = Dec4
type instance MulHi Dec8 Dec7 = Dec5
type instance MulHi Dec8 Dec8 = Dec6
type instance MulHi Dec8 Dec9 = Dec7

type instance MulHi Dec9 Dec0 = Dec0
type instance MulHi Dec9 Dec1 = Dec0
type instance MulHi Dec9 Dec2 = Dec1
type instance MulHi Dec9 Dec3 = Dec2
type instance MulHi Dec9 Dec4 = Dec3
type instance MulHi Dec9 Dec5 = Dec4
type instance MulHi Dec9 Dec6 = Dec5
type instance MulHi Dec9 Dec7 = Dec6
type instance MulHi Dec9 Dec8 = Dec7
type instance MulHi Dec9 Dec9 = Dec8


type family ScaleLo x ys
type instance ScaleLo _x EndAsc = EndAsc
type instance ScaleLo  x (yh :< yl) = ScaleLo x yh :< MulLo x yl

type family ScaleHi x ys
type instance ScaleHi _x EndAsc = EndAsc
type instance ScaleHi  x (yh :< yl) = ScaleHi x yh :< MulHi x yl

type family MulScaleAsc xs ys
type instance MulScaleAsc EndAsc _ys = EndAsc
type instance MulScaleAsc (xh :< xl) ys =
         AddAsc
            (ScaleLo xl ys)
            (AddAsc (MulScaleAsc xh ys) (ScaleHi xl ys) :< Dec0)


-----------
-- Division / Modulus

-- type family Op.IsEven x
type instance Op.IsEven (Dec x) = IsEven x

type family IsEven x
type instance IsEven Zero = True
type instance IsEven (Neg x xs) = IsEvenAsc (AscendingNonEmpty x xs)
type instance IsEven (Pos x xs) = IsEvenAsc (AscendingNonEmpty x xs)

type family IsEvenAsc x
type instance IsEvenAsc EndAsc = True
type instance IsEvenAsc (_xh :< xl) = IsEvenDigit xl

type family IsEvenDigit x
type instance IsEvenDigit Dec0 = True
type instance IsEvenDigit Dec1 = False
type instance IsEvenDigit Dec2 = True
type instance IsEvenDigit Dec3 = False
type instance IsEvenDigit Dec4 = True
type instance IsEvenDigit Dec5 = False
type instance IsEvenDigit Dec6 = True
type instance IsEvenDigit Dec7 = False
type instance IsEvenDigit Dec8 = True
type instance IsEvenDigit Dec9 = False

-- type family Op.Div2 x
type instance Op.Div2 (Dec x) = Dec (Div2 x)

type family Div2 x
type instance Div2 Zero = Zero
type instance Div2 (Neg x xs) =
        NormalizeNegDesc (Div2Asc (AscendingNonEmpty x xs))
type instance Div2 (Pos x xs) =
        NormalizePosDesc (Div2Asc (AscendingNonEmpty x xs))

type family Div2Digit x
type instance Div2Digit Dec0 = Dec0
type instance Div2Digit Dec1 = Dec0
type instance Div2Digit Dec2 = Dec1
type instance Div2Digit Dec3 = Dec1
type instance Div2Digit Dec4 = Dec2
type instance Div2Digit Dec5 = Dec2
type instance Div2Digit Dec6 = Dec3
type instance Div2Digit Dec7 = Dec3
type instance Div2Digit Dec8 = Dec4
type instance Div2Digit Dec9 = Dec4

type family Div2Asc x
type instance Div2Asc EndAsc = EndAsc
type instance Div2Asc (xh :< xl) =
        Div2Pos xh (Div2Digit xl) (If (IsEvenAsc xh) Dec0 Dec5)

type Div2Pos xh xl rem =
        AddCarry xl rem (Div2Asc xh) :< AddDigit xl rem

---------------
-- Exponentiation

type instance Op.Pow2 (Dec x) = Dec (Pow2 x)

type family Pow2 x
type instance Pow2 Zero = One
type instance Pow2 (Pos x xs) =
        NormalizePosDesc (Pow2Asc (Pos x xs) (EndAsc :< Dec1))

type family Pow2Asc x y
type instance Pow2Asc Zero y = y
type instance Pow2Asc (Pos x xs) y =
        Pow2Asc (Pred (Pos x xs)) (Mul2Asc y)

---------------
-- Logarithm
type instance Op.Log2Ceil (Dec x) = Dec (Log2Ceil x)

type family Log2Ceil x
type instance Log2Ceil (Pos x xs) =
        NormalizePosDesc (Log2C (Pred (Pos x xs)) EndAsc)

type family Log2C x y
type instance Log2C Zero y = y
type instance Log2C (Pos x xs) y = Log2C (Div2 (Pos x xs)) (SuccAsc y)

---------------
-- Comparison

type family CompareDigit x y
-- putStr $ unlines $ concat $ [ [ "type instance CompareDigit Dec" ++ show x ++ " Dec" ++ show y ++ " = " ++ (if x < y then "LT" else (if x > y then "GT" else "EQ")) | y <- [0..9] ] ++ [ "" ] | x <- [0..9] ]
type instance CompareDigit Dec0 Dec0 = EQ
type instance CompareDigit Dec0 Dec1 = LT
type instance CompareDigit Dec0 Dec2 = LT
type instance CompareDigit Dec0 Dec3 = LT
type instance CompareDigit Dec0 Dec4 = LT
type instance CompareDigit Dec0 Dec5 = LT
type instance CompareDigit Dec0 Dec6 = LT
type instance CompareDigit Dec0 Dec7 = LT
type instance CompareDigit Dec0 Dec8 = LT
type instance CompareDigit Dec0 Dec9 = LT

type instance CompareDigit Dec1 Dec0 = GT
type instance CompareDigit Dec1 Dec1 = EQ
type instance CompareDigit Dec1 Dec2 = LT
type instance CompareDigit Dec1 Dec3 = LT
type instance CompareDigit Dec1 Dec4 = LT
type instance CompareDigit Dec1 Dec5 = LT
type instance CompareDigit Dec1 Dec6 = LT
type instance CompareDigit Dec1 Dec7 = LT
type instance CompareDigit Dec1 Dec8 = LT
type instance CompareDigit Dec1 Dec9 = LT

type instance CompareDigit Dec2 Dec0 = GT
type instance CompareDigit Dec2 Dec1 = GT
type instance CompareDigit Dec2 Dec2 = EQ
type instance CompareDigit Dec2 Dec3 = LT
type instance CompareDigit Dec2 Dec4 = LT
type instance CompareDigit Dec2 Dec5 = LT
type instance CompareDigit Dec2 Dec6 = LT
type instance CompareDigit Dec2 Dec7 = LT
type instance CompareDigit Dec2 Dec8 = LT
type instance CompareDigit Dec2 Dec9 = LT

type instance CompareDigit Dec3 Dec0 = GT
type instance CompareDigit Dec3 Dec1 = GT
type instance CompareDigit Dec3 Dec2 = GT
type instance CompareDigit Dec3 Dec3 = EQ
type instance CompareDigit Dec3 Dec4 = LT
type instance CompareDigit Dec3 Dec5 = LT
type instance CompareDigit Dec3 Dec6 = LT
type instance CompareDigit Dec3 Dec7 = LT
type instance CompareDigit Dec3 Dec8 = LT
type instance CompareDigit Dec3 Dec9 = LT

type instance CompareDigit Dec4 Dec0 = GT
type instance CompareDigit Dec4 Dec1 = GT
type instance CompareDigit Dec4 Dec2 = GT
type instance CompareDigit Dec4 Dec3 = GT
type instance CompareDigit Dec4 Dec4 = EQ
type instance CompareDigit Dec4 Dec5 = LT
type instance CompareDigit Dec4 Dec6 = LT
type instance CompareDigit Dec4 Dec7 = LT
type instance CompareDigit Dec4 Dec8 = LT
type instance CompareDigit Dec4 Dec9 = LT

type instance CompareDigit Dec5 Dec0 = GT
type instance CompareDigit Dec5 Dec1 = GT
type instance CompareDigit Dec5 Dec2 = GT
type instance CompareDigit Dec5 Dec3 = GT
type instance CompareDigit Dec5 Dec4 = GT
type instance CompareDigit Dec5 Dec5 = EQ
type instance CompareDigit Dec5 Dec6 = LT
type instance CompareDigit Dec5 Dec7 = LT
type instance CompareDigit Dec5 Dec8 = LT
type instance CompareDigit Dec5 Dec9 = LT

type instance CompareDigit Dec6 Dec0 = GT
type instance CompareDigit Dec6 Dec1 = GT
type instance CompareDigit Dec6 Dec2 = GT
type instance CompareDigit Dec6 Dec3 = GT
type instance CompareDigit Dec6 Dec4 = GT
type instance CompareDigit Dec6 Dec5 = GT
type instance CompareDigit Dec6 Dec6 = EQ
type instance CompareDigit Dec6 Dec7 = LT
type instance CompareDigit Dec6 Dec8 = LT
type instance CompareDigit Dec6 Dec9 = LT

type instance CompareDigit Dec7 Dec0 = GT
type instance CompareDigit Dec7 Dec1 = GT
type instance CompareDigit Dec7 Dec2 = GT
type instance CompareDigit Dec7 Dec3 = GT
type instance CompareDigit Dec7 Dec4 = GT
type instance CompareDigit Dec7 Dec5 = GT
type instance CompareDigit Dec7 Dec6 = GT
type instance CompareDigit Dec7 Dec7 = EQ
type instance CompareDigit Dec7 Dec8 = LT
type instance CompareDigit Dec7 Dec9 = LT

type instance CompareDigit Dec8 Dec0 = GT
type instance CompareDigit Dec8 Dec1 = GT
type instance CompareDigit Dec8 Dec2 = GT
type instance CompareDigit Dec8 Dec3 = GT
type instance CompareDigit Dec8 Dec4 = GT
type instance CompareDigit Dec8 Dec5 = GT
type instance CompareDigit Dec8 Dec6 = GT
type instance CompareDigit Dec8 Dec7 = GT
type instance CompareDigit Dec8 Dec8 = EQ
type instance CompareDigit Dec8 Dec9 = LT

type instance CompareDigit Dec9 Dec0 = GT
type instance CompareDigit Dec9 Dec1 = GT
type instance CompareDigit Dec9 Dec2 = GT
type instance CompareDigit Dec9 Dec3 = GT
type instance CompareDigit Dec9 Dec4 = GT
type instance CompareDigit Dec9 Dec5 = GT
type instance CompareDigit Dec9 Dec6 = GT
type instance CompareDigit Dec9 Dec7 = GT
type instance CompareDigit Dec9 Dec8 = GT
type instance CompareDigit Dec9 Dec9 = EQ

type instance Ord.Compare (Dec x) (Dec y) = Compare x y
type family Compare x y
type instance Compare (Pos  x  xs) (Pos  y  ys) = ComparePos x xs y ys
type instance Compare (Neg  x  xs) (Neg  y  ys) = ComparePos y ys x xs
type instance Compare (Pos _x _xs) (Neg _y _ys) = GT
type instance Compare (Neg _x _xs) (Pos _y _ys) = LT
type instance Compare (Pos _x _xs) (Zero      ) = GT
type instance Compare (Neg _x _xs) (Zero      ) = LT
type instance Compare (Zero      ) (Neg _y _ys) = GT
type instance Compare (Zero      ) (Pos _y _ys) = LT
type instance Compare (Zero      ) (Zero      ) = EQ

type ComparePos x xs y ys =
        CompareAsc (AscendingNonEmpty x xs) (AscendingNonEmpty y ys) EQ

type family CompareAsc x y c
type instance CompareAsc EndAsc     EndAsc      c = c
type instance CompareAsc EndAsc     (_h :< _l) _c = LT
type instance CompareAsc (_h :< _l) EndAsc     _c = GT
type instance CompareAsc (xh :< xl) (yh :< yl) GT =
                 CompareDiff xh yh (CompareDigit xl yl) GT
type instance CompareAsc (xh :< xl) (yh :< yl) EQ =
                 CompareAsc xh yh (CompareDigit xl yl)
type instance CompareAsc (xh :< xl) (yh :< yl) LT =
                 CompareDiff xh yh (CompareDigit xl yl) LT

type family CompareDiff x y c l
type instance CompareDiff x y LT _c = CompareAsc x y LT
type instance CompareDiff x y EQ  c = CompareAsc x y c
type instance CompareDiff x y GT _c = CompareAsc x y GT


class x :<:  y; instance (x Ord.:<:  y) => Dec x :<:  Dec y
class x :<=: y; instance (x Ord.:<=: y) => Dec x :<=: Dec y
class x :>=: y; instance (x Ord.:>=: y) => Dec x :>=: Dec y
class x :>:  y; instance (x Ord.:>:  y) => Dec x :>:  Dec y
class x :==: y; instance (x Ord.:==: y) => Dec x :==: Dec y
class x :/=: y; instance (x Ord.:/=: y) => Dec x :/=: Dec y


type GreaterPos x xs y ys = Ord.IsGT (ComparePos x xs y ys)

instance Neg x xs :<: Zero
instance Neg x xs :<: Pos y ys
instance Zero     :<: Pos y ys

instance (ComparePos x xs y ys ~ GT) => Neg x xs :<: Neg y ys
instance (ComparePos x xs y ys ~ LT) => Pos x xs :<: Pos y ys


instance Neg x xs :<=: Zero
instance Neg x xs :<=: Pos y ys
instance Zero     :<=: Zero
instance Zero     :<=: Pos y ys

instance (GreaterPos y ys x xs ~ False) => Neg x xs :<=: Neg y ys
instance (GreaterPos x xs y ys ~ False) => Pos x xs :<=: Pos y ys


instance Zero     :>: Neg y ys
instance Pos x xs :>: Neg y ys
instance Pos x xs :>: Zero

instance (ComparePos x xs y ys ~ LT) => Neg x xs :>: Neg y ys
instance (ComparePos x xs y ys ~ GT) => Pos x xs :>: Pos y ys


instance Zero     :>=: Neg y ys
instance Pos x xs :>=: Neg y ys
instance Zero     :>=: Zero
instance Pos x xs :>=: Zero

instance (GreaterPos x xs y ys ~ False) => Neg x xs :>=: Neg y ys
instance (GreaterPos y ys x xs ~ False) => Pos x xs :>=: Pos y ys


instance Zero     :==: Zero

instance (ComparePos x xs y ys ~ EQ) => Neg x xs :==: Neg y ys
instance (ComparePos x xs y ys ~ EQ) => Pos x xs :==: Pos y ys


instance Zero     :/=: Neg y ys
instance Pos x xs :/=: Neg y ys
instance Neg x xs :/=: Zero
instance Pos x xs :/=: Zero
instance Neg x xs :/=: Pos y ys
instance Zero     :/=: Pos y ys

instance (Ord.IsEQ (ComparePos x xs y ys) ~ False) => Neg x xs :/=: Neg y ys
instance (Ord.IsEQ (ComparePos x xs y ys) ~ False) => Pos x xs :/=: Pos y ys


type family FromUnary n
type instance FromUnary Unary.Zero = Zero
type instance FromUnary (Unary.Succ n) = Succ (FromUnary n)

type family ToUnary n
type instance ToUnary Zero = Unary.Zero
type instance ToUnary (Pos x xs) = ToUnaryAcc (Digit.ToUnary x) xs

type family ToUnaryAcc m n
type instance ToUnaryAcc m EndDesc = m
type instance ToUnaryAcc m (x :> xs) =
                 ToUnaryAcc (UnaryAcc m x) xs

type UnaryAcc m x = Digit.ToUnary x Unary.:+: (m Unary.:*: UnaryLit.U10)
