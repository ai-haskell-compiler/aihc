{-# LANGUAGE TemplateHaskell #-}

module Utils where

import           Language.Haskell.TH

qBoolToExp :: Q Bool -> Q Exp
qBoolToExp q = do
  b <- q
  if b then [e|True|] else [e|False|]

apps :: [Type] -> Type
apps ts = foldl1 AppT ts
