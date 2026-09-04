module Data.Maybe
  ( Maybe (..),
    maybe,
    isJust,
    isNothing,
    fromJust,
    fromMaybe,
    listToMaybe,
    maybeToList,
    catMaybes,
    mapMaybe,
  )
where

import GHC.Base (Maybe (..))
import GHC.Err (errorWithoutStackTrace)
import GHC.Types (Bool (..))

maybe :: b -> (a -> b) -> Maybe a -> b
maybe initial _ Nothing = initial
maybe _ function (Just value) = function value

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just _) = False

fromJust :: Maybe a -> a
fromJust Nothing = errorWithoutStackTrace "Maybe.fromJust: Nothing"
fromJust (Just value) = value

fromMaybe :: a -> Maybe a -> a
fromMaybe fallback Nothing = fallback
fromMaybe _ (Just value) = value

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (value : _) = Just value

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just value) = [value]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing : rest) = catMaybes rest
catMaybes (Just value : rest) = value : catMaybes rest

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe function (value : rest) =
  case function value of
    Nothing -> mapMaybe function rest
    Just result -> result : mapMaybe function rest
