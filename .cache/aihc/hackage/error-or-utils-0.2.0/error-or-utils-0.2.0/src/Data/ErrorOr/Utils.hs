{-# LANGUAGE CPP #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A few utility functions with improved error reporting.
module Data.ErrorOr.Utils (
  tryRead, Data.ErrorOr.Utils.lookup, lookupIn, OverloadedLookup(..)
) where

import qualified Data.Map as M
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Char

#if __GLASGOW_HASKELL__ < 880
import Prelude hiding (fail)
import Data.Semigroup
import Control.Monad.Fail (MonadFail(..))
#endif

-- | A class used to implement `lookup` and `lookupIn`.
class OverloadedLookup t k v | t -> k, t -> v where overloadedLookup :: k -> t -> Maybe v
instance Eq k => OverloadedLookup [(k,v)] k v where overloadedLookup = List.lookup
instance Ord k => OverloadedLookup (M.Map k v) k v where overloadedLookup = M.lookup

-- | Overloaded lookup with good error messages.
lookup :: (OverloadedLookup t k v, Show k, Show t, MonadFail m, Applicative m) => k -> t -> m v
lookup k xs = lookupIn (shorten 256 (T.pack $ show xs)) k xs

-- | This is a version of `lookup` for where there is no Show instance for the collection.
lookupIn :: (OverloadedLookup t k v, Show k, MonadFail m, Applicative m) => T.Text -> k -> t -> m v
lookupIn name k =
  maybe (fail $ "Can't lookup " <> show k <> " in " <> T.unpack name) pure . overloadedLookup k

shorten :: Int -> T.Text -> T.Text
shorten maxLen msg =
  case T.splitAt maxLen msg of
    (x, t) | t == T.empty -> x
    (x, _rest) -> T.take (maxLen - 5) x <> "(...)"

-- | A read with better error messages.
tryRead :: (Read a, Show a, MonadFail m) => String -> m a
tryRead str =
  case reads str of
    [] -> fail ("Can't read: " ++ shorten 256 str)
    [(a, rest)]
      | all Data.Char.isSpace rest -> return a
    parses -> fail ("Ambiguous parse: '" ++ str ++ "' " ++ shorten 256 (show parses))
  where
    shorten :: Int -> [Char] -> [Char]
    shorten maxLen msg =
      case splitAt maxLen msg of
        (x, t) | t == [] -> x
        (x, _rest) -> take (maxLen - 5) x ++ "(...)"
