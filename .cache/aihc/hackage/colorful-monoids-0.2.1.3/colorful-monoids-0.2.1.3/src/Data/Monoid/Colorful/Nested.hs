{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Monoid.Colorful.Nested (
  Style(..)
  , Color(..)
  , Term(..)
  , Colored(..)
  , hGetTerm
  , getTerm
  , hPrintColored
  , printColored
  , hPrintColoredIO
  , printColoredIO
  , hPrintColoredS
  , printColoredS
  , showColored
  , showColoredM
  , showColoredS
) where

import System.IO (Handle)
import Data.Monoid.Colorful.Term
import Data.Monoid.Colorful.Color
import Data.Monoid.Colorful.SGR
import Data.Monoid.Colorful.Trustworthy
import Data.String (IsString(..))
import qualified Data.Semigroup as Sem
import GHC.Generics (Generic, Generic1)
import qualified Data.Monoid.Colorful.Flat as Flat
import Control.Monad (ap)

-- | Colored Monoid
data Colored a
  = Nil
  | Value a
  | Style   !Style (Colored a)
  | Unstyle !Style (Colored a)
  | Fg      !Color (Colored a)
  | Bg      !Color (Colored a)
  | Pair    (Colored a) (Colored a)
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Generic, Generic1)

-- | Free monad!
instance Applicative Colored where
  pure = Value
  (<*>) = ap

-- | Free monad!
instance Monad Colored where
  Nil         >>= _ = Nil
  Value     x >>= f = f x
  Style   a x >>= f = Style   a (x >>= f)
  Unstyle a x >>= f = Unstyle a (x >>= f)
  Fg      a x >>= f = Fg      a (x >>= f)
  Bg      a x >>= f = Bg      a (x >>= f)
  Pair    x y >>= f = Pair      (x >>= f) (y >>= f)

instance Sem.Semigroup (Colored a) where
  (<>) = Pair

instance Monoid (Colored a) where
  mempty = Nil
  mappend = (Sem.<>)

instance IsString a => IsString (Colored a) where
  fromString = Value . fromString

instance IsList (Colored a) where
  type Item (Colored a) = Colored a
  fromList = foldr Pair Nil
  toList   = (:[]) -- TODO, invalid

-- | Flatten the Monoid @Colored a@ and return @[Flat.Colored a]@
--
-- This function is used internally for rendering the Colored monoids.
flatten :: Colored a -> [Flat.Colored a]
flatten s = go s []
  where go (Value   a)      = (Flat.Value a:)
        go (Style   a b)    = (Flat.Push:) . (Flat.Style   a:) . go b . (Flat.Pop:)
        go (Unstyle a b)    = (Flat.Push:) . (Flat.Unstyle a:) . go b . (Flat.Pop:)
        go (Fg      a b)    = (Flat.Push:) . (Flat.Fg      a:) . go b . (Flat.Pop:)
        go (Bg      a b)    = (Flat.Push:) . (Flat.Bg      a:) . go b . (Flat.Pop:)
        go Nil              = id
        go (Pair    a b)    = go a . go b

hPrintColoredIO :: Handle -> Term -> Colored (IO ()) -> IO ()
hPrintColoredIO h t = Flat.hPrintColoredIO h t . flatten

printColoredIO :: Term -> Colored (IO ()) -> IO ()
printColoredIO t = Flat.printColoredIO t . flatten

hPrintColored :: (Handle -> a -> IO ()) -> Handle -> Term -> Colored a -> IO ()
hPrintColored f h t = Flat.hPrintColored f h t . flatten

printColored :: (a -> IO ()) -> Term -> Colored a -> IO ()
printColored f t = Flat.printColored f t . flatten

hPrintColoredS :: Handle -> Term -> Colored String -> IO ()
hPrintColoredS h t = Flat.hPrintColoredS h t . flatten

printColoredS :: Term -> Colored String -> IO ()
printColoredS t = Flat.printColoredS t . flatten

showColoredM :: (Monad f, Monoid o) => (a -> f o) -> (SGRCode -> f o) -> Term -> Colored a -> f o
showColoredM f g t = Flat.showColoredM f g t . flatten

showColored :: Monoid o => (a -> o) -> (SGRCode -> o) -> Term -> Colored a -> o
showColored f g t = Flat.showColored f g t . flatten

showColoredS :: Term -> Colored String -> ShowS
showColoredS t = Flat.showColoredS t . flatten
