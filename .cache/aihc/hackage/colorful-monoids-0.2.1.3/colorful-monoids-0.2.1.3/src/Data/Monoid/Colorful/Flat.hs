-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Colorful.Flat
-- Copyright   :  Daniel Mendler (c) 2017,
-- License     :  MIT (see the file LICENSE)
--
-- Maintainer  :  mail@daniel-mendler.de
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides the flat 'Colored' type,
-- which is used internally for rendering the
-- nested 'Data.Monoid.Colorful.Colored' but is
-- also useful on its own. The API resembles
-- the API of 'Data.Monoid.Colorful'.
--
-----------------------------------------------------------

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

module Data.Monoid.Colorful.Flat (
  -- * Colored datatypes
  Colored(..)
  , Style(..)
  , Color(..)

  -- * Terminal type
  , Term(..)
  , hGetTerm
  , getTerm

  -- * Colorful printing to file handle
  , hPrintColored
  , printColored
  , hPrintColoredIO
  , printColoredIO
  , hPrintColoredS
  , printColoredS

  -- * Show with ANSI escape sequences
  , showColored
  , showColoredM
  , showColoredS

  -- * Reexport from Data.Semigroup
  , (<>)
) where

import System.IO (Handle, stdout, hPutStr)
import Data.Monoid.Colorful.Term
import Data.Monoid.Colorful.Settings
import Data.Monoid.Colorful.Color
import Data.Monoid.Colorful.SGR
import Data.Functor.Identity
import GHC.Generics (Generic, Generic1)
import Data.Semigroup ((<>))
import Data.Foldable (foldlM)

data Colored a
  = Value a
  | Style   !Style
  | Unstyle !Style
  | Fg      !Color
  | Bg      !Color
  | Push
  | Pop
  | Reset
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic, Generic1)

hPrintColoredIO :: Handle -> Term -> [Colored (IO ())] -> IO ()
hPrintColoredIO h = showColoredM id (hPutStr h)

printColoredIO :: Term -> [Colored (IO ())] -> IO ()
printColoredIO = hPrintColoredIO stdout

hPrintColored :: Foldable f => (Handle -> a -> IO ()) -> Handle -> Term -> f (Colored a) -> IO ()
hPrintColored f h = showColoredM (f h) (hPutStr h)

printColored :: Foldable f => (a -> IO ()) -> Term -> f (Colored a) -> IO ()
printColored f = hPrintColored (const f) stdout

hPrintColoredS :: Foldable f => Handle -> Term -> f (Colored String) -> IO ()
hPrintColoredS h = showColoredM (hPutStr h) (hPutStr h)

printColoredS :: Foldable f => Term -> f (Colored String) -> IO ()
printColoredS = hPrintColoredS stdout

showColoredS :: Foldable f => Term -> f (Colored String) -> ShowS
showColoredS = showColored (++) (++)

showColored :: (Foldable f, Monoid o) => (a -> o) -> (SGRCode -> o) -> Term -> f (Colored a) -> o
showColored str code term flat = runIdentity $ showColoredM (pure . str) (pure . code) term flat
{-# SPECIALIZE showColored :: Monoid o => (a -> o) -> (SGRCode -> o) -> Term -> [Colored a] -> o #-}

showColoredM :: (Foldable f, Monad m, Monoid o) => (a -> m o) -> (SGRCode -> m o) -> Term -> f (Colored a) -> m o
showColoredM str code term list = do
  s <- foldlM go (State mempty defaultSettings (defaultSettings, [])) list
  mappend (stateStr s) <$> code (sgrCode term (stateActive s) (fst $ stateStack s))
  where go s Push        = pure $ s { stateStack = pushStack $ stateStack s }
        go s Pop         = pure $ s { stateStack = popStack $ stateStack s }
        go s Reset       = pure $ s { stateStack = resetStack $ stateStack s }
        go s (Style   a) = pure $ mapTop (setStyle a True) s
        go s (Unstyle a) = pure $ mapTop (setStyle a False) s
        go s (Fg      a) = pure $ mapTop (setFg a) s
        go s (Bg      a) = pure $ mapTop (setBg a) s
        go s (Value   a) = do
          !x <- code (sgrCode term (stateActive s) (fst $ stateStack s))
          !y <- str a
          let !z = x `mappend` y
          pure $ s { stateStr = stateStr s `mappend` z, stateActive = fst $ stateStack s }
{-# SPECIALIZE showColoredM :: (Foldable f, Monoid o) => (a -> Identity o) -> (SGRCode -> Identity o) -> Term -> f (Colored a) -> Identity o #-}
{-# SPECIALIZE showColoredM :: (Foldable f, Monoid o) => (a -> (o -> o)) -> (SGRCode -> (o -> o)) -> Term -> f (Colored a) -> (o -> o) #-}
{-# SPECIALIZE showColoredM :: Monoid o => (a -> Identity o) -> (SGRCode -> Identity o) -> Term -> [Colored a] -> Identity o #-}
{-# SPECIALIZE showColoredM :: Monoid o => (a -> (o -> o)) -> (SGRCode -> (o -> o)) -> Term -> [Colored a] -> (o -> o) #-}

data State a = State
  { stateStr  :: !a
  , stateActive :: !Settings
  , stateStack  :: !(Settings, [Settings])
  }

mapTop :: (Settings -> Settings) -> State a -> State a
mapTop f s = let !t = f $ fst $ stateStack s in s { stateStack = (t, snd $ stateStack s) }
{-# INLINE mapTop #-}
