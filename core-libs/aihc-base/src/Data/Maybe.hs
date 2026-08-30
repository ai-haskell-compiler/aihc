module Data.Maybe
  ( Maybe (..),
    isJust,
  )
where

import Prelude (Bool (..), Maybe (..))

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True
