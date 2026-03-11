{-# LANGUAGE CPP, DeriveFunctor, TypeSynonymInstances, FlexibleInstances, TypeFamilies #-}
-- |
-- The `build` function can be used to construct multi-line string literals in
-- a monadic way:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Data.String.Builder
-- >
-- > mystring :: String
-- > mystring = build $ do
-- >   "foo"
-- >   "bar"
-- >   "baz"
module Data.String.Builder (
-- * Functions
  build
, literal
-- * Types
, Builder
, BuilderM
) where

import           Control.Applicative
import           Control.Monad
import           Data.Monoid
import           Data.String

-- | A writer monad for string literals.
data BuilderM a = BuilderM a ShowS
  deriving Functor

instance Applicative BuilderM where
  pure = return
  (<*>) = ap

instance Monad BuilderM where
  return a            = BuilderM a id
  BuilderM a xs >>= f = case f a of
    BuilderM b ys -> BuilderM b (xs . ys)

type Builder = BuilderM ()

instance Monoid Builder where
  mempty = return ()
#if !MIN_VERSION_base(4,11,0)
  mappend = (>>)
#else
instance Semigroup Builder where
  (<>) = (>>)
#endif

-- | Add a literal string.
literal :: String -> Builder
literal = BuilderM () . showString

instance (a ~ ()) => IsString (BuilderM a) where
  fromString s = literal s >> literal "\n"

-- | Run a builder.
build :: Builder -> String
build (BuilderM () s) = s ""
