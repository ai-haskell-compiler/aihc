{- ORACLE_TEST pass -}
{-# LANGUAGE Arrows #-}
module ArrowCaseNestedDoTupleBind where

import Control.Arrow

f arrM g h j = proc x -> do
  requestMaybe <- arrM $ g -< x
  case requestMaybe of
    Just request -> do
      (a, b) <- g -< request
      arrM $ h -< (a, b)
      returnA -< Just a
    Nothing -> do
      arrM $ j -< ()
      returnA -< Nothing
