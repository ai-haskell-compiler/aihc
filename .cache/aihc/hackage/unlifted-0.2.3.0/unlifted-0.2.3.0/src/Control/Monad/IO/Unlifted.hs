{-# language MagicHash #-}

module Control.Monad.IO.Unlifted
  ( IO#(..)
  , lift
  , unlift
  ) where

import GHC.IO (IO(IO))
import Data.Unlifted (IO#(..))

lift :: IO# a -> IO a
lift (IO# f) = IO f

unlift :: IO a -> IO# a
unlift (IO f) = IO# f
