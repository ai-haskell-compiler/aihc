{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Defines core functions that augment the prelude.
module Zenacy.HTML.Internal.Core
  ( updateSTRef
  , rref
  , wref
  , uref
  , findSucc
  , insertBefore
  , removeFirst
  , textExtract
  , textBlank
  , textReadDec
  ) where

import Data.Text
  ( Text
  )
import qualified Data.Text as T
  ( breakOn
  , drop
  , length
  , null
  , replicate
  , singleton
  , unpack
  )
import Control.Monad.ST
  ( ST
  )
import Data.STRef
  ( STRef
  , readSTRef
  , writeSTRef
  )
import qualified Numeric as N
  ( readDec
  )

-- | Updates an STRef by applying a function to its value.
updateSTRef :: STRef s a -> (a -> a) -> ST s ()
updateSTRef r f = readSTRef r >>= writeSTRef r . f
{-# INLINE updateSTRef #-}

-- | Abbreviation for reading an STRef.
rref :: STRef s a -> ST s a
rref = readSTRef
{-# INLINE rref #-}

-- | Abbreviation for writing an STRef.
wref :: STRef s a -> a -> ST s ()
wref = writeSTRef
{-# INLINE wref #-}

-- | Abbreviation for updating an STRef.
uref :: STRef s a -> (a -> a) -> ST s ()
uref = updateSTRef
{-# INLINE uref #-}

-- | Finds the element in a list that is the succeessor of the element
--   matching a predicate.
findSucc :: (a -> Bool) -> [a] -> Maybe a
findSucc p [] = Nothing
findSucc p (y:x:xs) = if p y then Just x else findSucc p (x:xs)
findSucc p (x:xs) = findSucc p xs

-- | Inserts an element in a list before the element satisfied by a predicate.
insertBefore :: (a -> Bool) -> a -> [a] -> [a]
insertBefore f _ [] = []
insertBefore f x (y:ys) = if f y then x : y : ys else y : insertBefore f x ys

-- | Removes the first item from a list that satisfies a predicate.
removeFirst :: (a -> Bool) -> [a] -> [a]
removeFirst p [] = []
removeFirst p (x:xs) = if p x then xs else x : removeFirst p xs

-- | Extracts a range of text bewteen two delimiters.
textExtract :: Text -> Text -> Text -> Maybe Text
textExtract d0 d1 t
  | T.null b = Nothing
  | T.null y = Nothing
  | T.null x = Nothing
  | otherwise = Just x
  where
    (a,b) = T.breakOn d0 t
    (x,y) = T.breakOn d1 $ T.drop (T.length d0) b

-- | Returns a blank text of the specified length.
textBlank :: Int -> Text
textBlank x = T.replicate x (T.singleton ' ')

-- | Converts a decimal string to a integer.
textReadDec :: Text -> Maybe Int
textReadDec x =
  case N.readDec (T.unpack x) of
    [(a,_)]    -> Just a
    _otherwise -> Nothing
