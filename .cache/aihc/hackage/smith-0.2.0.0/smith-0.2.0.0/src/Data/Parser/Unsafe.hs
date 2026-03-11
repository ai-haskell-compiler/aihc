{-# language BangPatterns #-}
{-# language BinaryLiterals #-}
{-# language DataKinds #-}
{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language MultiWayIf #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language UnboxedSums #-}
{-# language UnboxedTuples #-}

-- | Everything in this module is unsafe and can lead to
-- nondeterministic output or segfaults if used incorrectly.
module Data.Parser.Unsafe
  ( -- * Types
    Parser(..)
    -- * Functions
  , unconsume
  ) where

import GHC.Exts (Int(I#),Int#)
import Data.Bytes.Parser (Result(..),Slice(..))
import Data.Kind (Type)
import Data.Primitive.SmallArray (SmallArray(SmallArray))
import GHC.Exts (TYPE,State#,SmallArray#)

import qualified Control.Monad

type SmallVector# a = (# SmallArray# a, Int#, Int# #)
type ST# s (a :: TYPE r) = State# s -> (# State# s, a #)
type Result# e (a :: TYPE r) =
  (# e
  | (# a, Int#, Int# #) #) -- ints are offset and length

-- | A non-resumable toke parser.
newtype Parser :: Type -> Type -> Type -> Type -> Type where
  Parser :: { runParser :: SmallVector# a -> ST# s (Result# e b) }
         -> Parser a e s b

instance Functor (Parser a e s) where
  {-# inline fmap #-}
  fmap f (Parser g) = Parser
    (\x s0 -> case g x s0 of
      (# s1, r #) -> case r of
        (# e | #) -> (# s1, (# e | #) #)
        (# | (# a, b, c #) #) -> (# s1, (# | (# f a, b, c #) #) #)
    )

instance Applicative (Parser a e s) where
  pure = pureParser
  (<*>) = Control.Monad.ap

instance Monad (Parser a e s) where
  {-# inline (>>=) #-}
  Parser f >>= g = Parser
    (\x@(# arr, _, _ #) s0 -> case f x s0 of
      (# s1, r0 #) -> case r0 of
        (# e | #) -> (# s1, (# e | #) #)
        (# | (# y, b, c #) #) ->
          runParser (g y) (# arr, b, c #) s1
    )

pureParser :: b -> Parser a e s b
pureParser a = Parser
  (\(# _, b, c #) s -> (# s, (# | (# a, b, c #) #) #))

-- | Move the cursor back by @n@ tokens. Precondition: you
-- must have previously consumed at least @n@ tokens.
unconsume :: Int -> Parser a e s ()
unconsume n = uneffectful $ \_ off len ->
  Success (Slice (off - n) (len + n) ())

uneffectful ::
     (SmallArray a -> Int -> Int -> Result e b)
  -> Parser a e s b
{-# inline uneffectful #-}
uneffectful f = Parser
  ( \(# arr,off,len #) s0 -> (# s0, unboxResult (f (SmallArray arr) (I# off) (I# len)) #) )

unboxResult :: Result e a -> (# e | (# a, Int#, Int# #) #) 
unboxResult (Success (Slice (I# b) (I# c) a)) = (# | (# a, b, c #) #)
unboxResult (Failure e) = (# e | #)

