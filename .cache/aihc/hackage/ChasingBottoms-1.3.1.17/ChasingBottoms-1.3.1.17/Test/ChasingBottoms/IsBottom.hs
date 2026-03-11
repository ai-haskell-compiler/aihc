{-# LANGUAGE ScopedTypeVariables #-}
-- The following (possibly unnecessary) options are included due to
-- the use of unsafePerformIO below.
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

-- |
-- Module      :  Test.ChasingBottoms.IsBottom
-- Copyright   :  (c) Nils Anders Danielsson 2004-2022, 2024-2025
-- License     :  See the file LICENCE.
--
-- Maintainer  :  http://www.cse.chalmers.se/~nad/
-- Stability   :  experimental
-- Portability :  non-portable (exceptions)
--

module Test.ChasingBottoms.IsBottom
  ( isBottom
  , isBottomIO
  , bottom
  , nonBottomError
  , isBottomTimeOut
  , isBottomTimeOutIO
  ) where

import Prelude hiding (catch)
import qualified Control.Exception as E
import System.IO.Unsafe (unsafePerformIO)
import qualified Test.ChasingBottoms.TimeOut as T

-- | @'isBottom' a@ returns 'False' if @a@ is distinct from bottom. If
-- @a@ equals bottom and results in an exception of a certain kind
-- (see below), then @'isBottom' a = 'True'@. If @a@ never reaches a
-- weak head normal form and never throws one of these exceptions,
-- then @'isBottom' a@ never terminates.
--
-- The exceptions that yield 'True' correspond to \"pure bottoms\",
-- i.e. bottoms that can originate in pure code:
--
--   * 'E.ArrayException'
--
--   * 'E.ErrorCall'
--
--   * 'E.NoMethodError'
--
--   * 'E.NonTermination'
--
--   * 'E.PatternMatchFail'
--
--   * 'E.RecConError'
--
--   * 'E.RecSelError'
--
--   * 'E.RecUpdError'
--
-- Assertions are excluded, because their behaviour depends on
-- compiler flags (not pure, and a failed assertion should really
-- yield an exception and nothing else). The same applies to
-- arithmetic exceptions (machine dependent, except possibly for
-- 'E.DivideByZero', but the value infinity makes that case unclear as
-- well).

-- Should we use throw or throwIO below?
--   It doesn't seem to matter, and I don't think it matters, but
--   using throw won't give us any problems.

-- Check out a discussion about evaluate around
-- http://www.haskell.org/pipermail/glasgow-haskell-users/2002-May/003393.html.

-- From the docs:
--   evaluate undefined `seq` return ()  ==> return ()
--   catch (evaluate undefined) (\e -> return ())  ==> return ()

isBottom :: a -> Bool
isBottom = isBottomTimeOut Nothing

-- | 'bottom' generates a bottom that is suitable for testing using
-- 'isBottom'.
bottom :: a
bottom = error "_|_"

-- | @'nonBottomError' s@ raises an exception ('E.AssertionFailed')
-- that is not caught by 'isBottom'. Use @s@ to describe the
-- exception.

nonBottomError :: String -> a
nonBottomError = E.throw . E.AssertionFailed

-- | @'isBottomTimeOut' timeOutLimit@ works like 'isBottom', but if
-- @timeOutLimit@ is @'Just' lim@, then computations taking more than
-- @lim@ seconds are also considered to be equal to bottom. Note that
-- this is a very crude approximation of what a bottom is. Also note
-- that this \"function\" may return different answers upon different
-- invocations. Take it for what it is worth.
--
-- 'isBottomTimeOut' is subject to all the same vagaries as
-- 'T.timeOut'.

-- The following pragma is included due to the use of unsafePerformIO
-- below.
{-# NOINLINE isBottomTimeOut #-}
isBottomTimeOut :: Maybe Int -> a -> Bool
isBottomTimeOut timeOutLimit f =
  unsafePerformIO $ isBottomTimeOutIO timeOutLimit f

-- | A variant of 'isBottom' that lives in the 'IO' monad.

isBottomIO :: a -> IO Bool
isBottomIO = isBottomTimeOutIO Nothing

-- | A variant of 'isBottomTimeOut' that lives in the 'IO' monad.

isBottomTimeOutIO :: Maybe Int -> a -> IO Bool
isBottomTimeOutIO timeOutLimit f =
  maybeTimeOut (E.evaluate f) `E.catches`
    [ E.Handler (\(_ :: E.ArrayException)   -> return True)
    , E.Handler (\(_ :: E.ErrorCall)        -> return True)
    , E.Handler (\(_ :: E.NoMethodError)    -> return True)
    , E.Handler (\(_ :: E.NonTermination)   -> return True)
    , E.Handler (\(_ :: E.PatternMatchFail) -> return True)
    , E.Handler (\(_ :: E.RecConError)      -> return True)
    , E.Handler (\(_ :: E.RecSelError)      -> return True)
    , E.Handler (\(_ :: E.RecUpdError)      -> return True)
    ]
  where
  maybeTimeOut io = case timeOutLimit of
    Nothing -> do
      io
      return False
    Just lim -> do
      result <- T.timeOut lim io
      case result of               -- Note that evaluate bottom /= bottom.
        T.Value _        -> return False
        T.NonTermination -> return True
        T.Exception e    -> E.throw e  -- Catch the exception above.
