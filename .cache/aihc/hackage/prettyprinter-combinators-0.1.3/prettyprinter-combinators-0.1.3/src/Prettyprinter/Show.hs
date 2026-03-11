----------------------------------------------------------------------------
-- |
-- Module      :  Prettyprinter.Show
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Prettyprinter.Show
  ( ppShow
  , PPShow(..)
  -- * Reexports
  , Pretty(..)
  ) where

import Data.Text qualified as T
import Prettyprinter
import Prettyprinter qualified  as PP
import Prettyprinter.Combinators
import Prettyprinter.MetaDoc
import Text.Show.Pretty (parseValue, Value(..))

-- $setup
-- >>> :set -XDerivingVia
-- >>> import Data.IntMap (IntMap)
-- >>> import Data.Map.Strict (Map)
-- >>> import Data.Set (Set)

-- | Helper to use 'Show'-based prettyprinting with DerivingVia.
--
-- >>> :{
-- data TestWithDeriving a b = TestWithDeriving
--   { testSet         :: Maybe (Set a)
--   , testB           :: b
--   , testIntMap      :: IntMap String
--   , testComplexMap  :: Map (Maybe (Set Int)) (IntMap (Set String))
--   }
--   deriving (Show)
--   deriving Pretty via PPShow (TestWithDeriving a b)
-- :}
--
newtype PPShow a = PPShow { unPPShow :: a }

instance Show a => Pretty (PPShow a) where
  pretty = ppShow . unPPShow

ppShow :: Show a => a -> Doc ann
ppShow x =
  case parseValue y of
    Nothing -> pretty y
    Just y' -> mdPayload $ ppValue y'
  where
    y :: String
    y = show x

ppValue :: Value -> MetaDoc ann
ppValue = \case
  Con name args   ->
    constructorAppMetaDoc (atomicMetaDoc (pretty name)) (map ppValue args)
  InfixCons v xs  ->
    compositeMetaDoc $
    hsep (mdPayload (ppValue v) : concatMap (\(con, v') -> [pretty con, mdPayload (ppValue v')]) xs)
  Rec name fields ->
    compositeMetaDoc $
    ppDictHeader (pretty name) (map (\(field, v) -> T.pack field :-> mdPayload (ppValue v)) fields)
  Tuple xs        ->
    atomicMetaDoc $
    ppListWithDelim PP.lparen PP.rparen $ map (mdPayload . ppValue) xs
  List xs         ->
    atomicMetaDoc $
    ppListWith (mdPayload . ppValue) xs
  Neg x           -> atomicMetaDoc $ "-" <> mdPayload (ppValue x)
  Ratio x y       -> compositeMetaDoc $ mdPayload (ppValue x) <+> "%" <+> mdPayload (ppValue y)
  Integer x       -> stringMetaDoc x
  Float x         -> stringMetaDoc x
  Char x          -> stringMetaDoc x
  String x        -> stringMetaDoc x
#if MIN_VERSION_pretty_show (1, 10, 0)
  Date x          -> stringMetaDoc x
  Time x          -> stringMetaDoc x
  Quote x         -> stringMetaDoc x
#endif
