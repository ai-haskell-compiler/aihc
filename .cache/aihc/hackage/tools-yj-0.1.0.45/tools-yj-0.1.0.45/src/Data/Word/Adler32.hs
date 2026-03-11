{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Word.Adler32 (

	A, fromWord32, toWord32, step, initial

	) where

import Prelude hiding (uncurry)
import Data.Bits
import Data.Word

step :: (Int, A) -> Word8 -> (Int, A)
step (n, A a b) w = case n of
	0 -> (5551, A (( a + fromIntegral w) `mod` 65521) ((b + a + fromIntegral w) `mod` 65521))
	_ -> (n - 1, A (a + fromIntegral w) (b + a + fromIntegral w))

data A = A !Word32 !Word32 deriving (Show, Eq)

initial :: (Int, A)
initial = (5551, A 1 0)

fromWord32 :: Word32 -> A
fromWord32 w = A (w .&. 0xffff) (w `shiftR` 16)

toWord32 :: A -> Word32
toWord32 = uncurry (.|.) . first (`mod` 65521) . second ((`shiftL` 16) . (`mod` 65521))

first :: (Word32 -> Word32) -> A -> A
first f (A a b) = A (f a) b

second :: (Word32 -> Word32) -> A -> A
second f (A a b) = A a (f b)

uncurry :: (Word32 -> Word32 -> a) -> A -> a
uncurry f (A a b) = f a b
