{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module HaskellWorks.Data.MQuery.Micro where

import Prettyprinter
import Prettyprinter.Render.Text


import qualified Data.DList as DL

newtype Micro a = Micro a

prettyVs :: Pretty a => [a] -> Doc ann
prettyVs (kv:kvs) = pretty kv <> foldl (<>) mempty ((\jv -> ", " <> pretty jv) `map` kvs)
prettyVs []       = mempty

putPretty :: Pretty a => a -> IO ()
putPretty a = putDoc (pretty a <> hardline)

prettyKvs :: Pretty (Micro a) => [a] -> Doc ann
prettyKvs (kv:kvs) = pretty (Micro kv) <> foldl (<>) mempty ((\jv -> ", " <> pretty (Micro jv)) `map` kvs)
prettyKvs []       = mempty

instance Pretty a => Pretty (Micro [a]) where
  pretty (Micro xs) = case length xs of
    xsLen | xsLen == 0  -> "[]"
    xsLen | xsLen <= 10 -> "[" <> prettyVs xs <> "]"
    _                   -> "[" <> prettyVs (take 10 xs) <> ", ..]"

instance Pretty a => Pretty (Micro (DL.DList a)) where
  pretty (Micro dxs) = case DL.toList dxs of
    xs@(_:_:_:_:_:_:_:_:_:_:_:_:_) -> "[" <> prettyVs (take 50 xs) <> ", ..]"
    []                             -> "[]"
    xs                             -> "[" <> prettyVs          xs  <> "]"
