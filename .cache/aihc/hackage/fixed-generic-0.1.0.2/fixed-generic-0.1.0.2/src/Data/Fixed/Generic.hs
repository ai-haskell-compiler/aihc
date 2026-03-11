{-# LANGUAGE BlockArguments, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Fixed.Generic (F(..), showF, changeUnit) where

import GHC.Internal.Read
import GHC.Internal.Text.ParserCombinators.ReadPrec
import GHC.Internal.Text.Read.Lex
import Data.Data
import Data.Fixed

newtype F (a :: k) n = MkF n deriving (Eq, Ord, Bounded)

tyF :: DataType
tyF = mkDataType "Data.Fixed.Generic.F" [conMkF]

conMkF :: Constr
conMkF = mkConstr tyF "MkF" [] Prefix

instance (Typeable k, Typeable a, Data n) => Data (F (a :: k) n) where
	gfoldl k z (MkF a) = k (z MkF) a
	gunfold k z _ = k (z MkF)
	dataTypeOf _ = tyF
	toConstr _ = conMkF

withResolution :: forall k (a :: k) f . HasResolution a => (Integer -> f) -> f
withResolution foo = foo . resolution $ Proxy @a

resl :: forall a n . (HasResolution a, Num n) => n
resl = fromInteger $ resolution (Proxy @a)

withResl :: forall k (a :: k) n f . (Num n, HasResolution a) => (n -> f) -> f
withResl foo = foo $ resl @a

instance Enum n => Enum (F a n) where
	succ (MkF a) = MkF (succ a)
	pred (MkF a) = MkF (succ a)
	toEnum = MkF . toEnum
	fromEnum (MkF a) = fromEnum a
	enumFrom (MkF a) = fmap MkF $ enumFrom a
	enumFromThen (MkF a) (MkF b) = fmap MkF (enumFromThen a b)
	enumFromTo (MkF a) (MkF b) = fmap MkF (enumFromTo a b)
	enumFromThenTo (MkF a) (MkF b) (MkF c) = fmap MkF (enumFromThenTo a b c)

instance (HasResolution a, Integral n) => Num (F a n) where
	MkF a + MkF b = MkF $ a + b
	MkF a - MkF b = MkF $ a - b
	MkF a * MkF b = MkF $ (a * b) `div` resl @a
	negate (MkF a) = MkF $ negate a
	abs (MkF a) = MkF $ abs a
	signum (MkF a) = fromIntegral $ signum a
	fromInteger i = withResl @_ @a \res -> MkF $ fromInteger i * res

instance (HasResolution a, Integral n) => Real (F a n) where
	toRational (MkF a) = toRational a / toRational (resolution (Proxy @a))

instance (HasResolution a, Integral n) => Fractional (F a n) where
	MkF a / MkF b = MkF $ (a * resl @a) `div` b
	recip (MkF a) = MkF $ (res * res) `div` a where res = resl @a
	fromRational r =
		withResolution @_ @a \res -> MkF (floor (r * (toRational res)))

instance (HasResolution a, Integral n) => RealFrac (F a n) where
	properFraction a = (i, a - fromIntegral i)
		where i = truncate a
	truncate f = truncate (toRational f)
	round f = round (toRational f)
	ceiling f = ceiling (toRational f)
	floor f = floor (toRational f)

chopZeros :: (Show n, Integral n) => n -> String
chopZeros 0 = ""
chopZeros a | a `mod` 10 == 0 = chopZeros (a `div` 10)
chopZeros a = show a

showIntegerZeros :: (Show n, Integral n) => Bool -> Int -> n -> String
showIntegerZeros True _ 0 = ""
showIntegerZeros chopTrailingZeros digits a = replicate (digits - length s) '0' ++ s' where
	s = show a
	s' = if chopTrailingZeros then chopZeros a else s

withDot :: String -> String
withDot "" = ""
withDot s = '.' : s

showF :: forall a n . (HasResolution a, Show n, Integral n) => Bool -> F a n -> String
showF chopTrailingZeros (MkF a) | a < 0 =
	"-" ++ showF chopTrailingZeros (MkF (negate a) :: F a n)
showF chopTrailingZeros (MkF a) =
	show i ++ withDot (showIntegerZeros chopTrailingZeros digits fracNum)
	where
	res = resl @a
	(i, d) = a `divMod` res
	digits = ceiling (logBase 10 (fromIntegral res) :: Double)
	maxnum = 10 ^ digits
	fracNum = divCeil (d * maxnum) res
	divCeil x y = (x + y - 1) `div` y

instance (HasResolution a, Show n, Integral n) => Show (F a n) where
	showsPrec p n = showParen (p > 6 && n < 0) $ showString $ showF False n

convertF :: forall a n . (HasResolution a, Integral n) => Lexeme -> ReadPrec (F a n)
convertF (Number n)
	| Just (i, f) <- numberToFixed e n =
		pure (fromInteger i + (fromInteger f / (10 ^ e)))
		where
		r = resl @a
		e = ceiling (logBase 10 (fromInteger r) :: Double)
convertF _ = pfail

instance (HasResolution a, Integral n) => Read (F a n) where
	readPrec = readNumber convertF
	readListPrec = readListPrecDefault
	readList = readListDefault

changeUnit :: F a n -> F a' n
changeUnit (MkF a) = MkF a
