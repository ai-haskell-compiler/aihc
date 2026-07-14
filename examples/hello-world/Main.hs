{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

data State# s

data RealWorld

data Int32 = I32# Int32#

newtype CInt = CInt Int32

newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))

foreign import prim realWorld# :: State# RealWorld

foreign import ccall unsafe putchar :: CInt -> IO CInt

bind :: IO a -> (a -> IO b) -> IO b
bind (IO action) next =
  IO
    ( \state ->
        case action state of
          (# nextState, value #) ->
            case next value of
              IO nextAction -> nextAction nextState
    )

thenIO :: IO a -> IO b -> IO b
thenIO first second = bind first (\ignored -> second)

infixr 1 >>

(>>) :: IO a -> IO b -> IO b
(>>) first second = thenIO first second

char :: Int32# -> CInt
char value = CInt (I32# value)

main :: IO CInt
main =
  putchar (char 72#Int32)
    >> putchar (char 101#Int32)
    >> putchar (char 108#Int32)
    >> putchar (char 108#Int32)
    >> putchar (char 111#Int32)
    >> putchar (char 44#Int32)
    >> putchar (char 32#Int32)
    >> putchar (char 119#Int32)
    >> putchar (char 111#Int32)
    >> putchar (char 114#Int32)
    >> putchar (char 108#Int32)
    >> putchar (char 100#Int32)
    >> putchar (char 33#Int32)
    >> putchar (char 10#Int32)
