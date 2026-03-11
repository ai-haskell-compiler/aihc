module Data.Accessor.Monad.Trans.Example where

import Data.Accessor.Basic ((.>), )
import Data.Accessor.Tuple (first, second, )

import Data.Accessor.Monad.Trans.State ((%=), (%:), )
import qualified Data.Accessor.Monad.Trans.State as AState
import Control.Monad.Trans.State (State)

import Prelude hiding (init)


state :: State (Char,Int) Int
state =
   do AState.set first 'a'
      AState.modify second succ
      AState.get second

stateInfix :: State ((Char, Int), String) Int
stateInfix =
   do str <- AState.get second
      first.>first %= 'a'
      first.>second %: succ
      first.>first %= 'b'
      second %= '!' : str
      AState.get (first.>second)

stateLift :: State (Int, (Bool, Char)) ()
stateLift = do
  first %= 42
  AState.lift second $ do
    first  %: not
    second %= 'q'
