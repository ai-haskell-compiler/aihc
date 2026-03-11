-- |
-- Bidirectional version of "Data.List" and other operations over lists.
{-# LANGUAGE Safe, QuasiQuotes, TypeOperators #-}
module Data.Invertible.List
  ( cons
  , uncons
  , consMaybe
  , repLen
  , map
  , reverse
  , transpose
  , lookup
  , index
  , zip
  , zip3
  , zip4
  , zip5
  , zip6
  , zip7
  , zipWith
  , interleave
  , lines
  , words
  ) where

import Prelude hiding (map, reverse, lookup, zip, zip3, unzip, zipWith, lines, words)
import Control.Arrow ((***))
import qualified Data.List as L
import Data.Tuple (swap)

import Data.Invertible.Bijection
import Data.Invertible.TH
import Data.Invertible.Internal

-- |Convert between @'Just' (head, tail)@ and the non-empty list @head:tail@.
cons :: Maybe (a, [a]) <-> [a]
cons =
  [biCase|
    Just (a, l) <-> a:l
    Nothing <-> []
  |]

-- |Convert between the non-empty list @head:tail@ and @'Just' (head, tail)@. (@'Control.Invertible.BiArrow.invert' 'cons'@)
uncons :: [a] <-> Maybe (a, [a])
uncons = invert cons

-- |Convert between @('Just' head, tail)@ and the non-empty list @head:tail@, or @('Nothing', list)@ and @list@.
consMaybe :: (Maybe a, [a]) <-> [a]
consMaybe =
  [biCase|
    (Just a, l) <-> a:l
    (Nothing, l) <-> l
  |]

-- |Combine 'L.replicate' and 'L.length' for unit lists.
repLen :: Int <-> [()]
repLen = (`L.replicate` ()) :<->: L.length

-- |Apply a bijection over a list using 'L.map'.
map :: (a <-> b) -> [a] <-> [b]
map (f :<->: g) = L.map f :<->: L.map g

-- |'L.reverse' the order of a (finite) list.
reverse :: [a] <-> [a]
reverse = involution L.reverse

-- |'L.transpose' the rows and columns of its argument.
transpose :: [[a]] <-> [[a]]
transpose = involution L.transpose

-- |Bi-directional 'L.lookup'.
lookup :: (Eq a, Eq b) => [(a, b)] -> Maybe a <-> Maybe b
lookup l = (flip L.lookup l =<<) :<->: (flip L.lookup (L.map swap l) =<<)

-- |Combine 'L.elemIndex' and safe 'L.!!'.
index :: Eq a => [a] -> Maybe a <-> Maybe Int
index l = (flip L.elemIndex l =<<) :<->: (idx l =<<) where
  idx _ i | i < 0 = Nothing
  idx [] _ = Nothing
  idx (x:_) 0 = Just x
  idx (_:r) i = idx r $ pred i

-- |'L.zip' two lists together.
zip :: ([a], [b]) <-> [(a, b)]
zip = uncurry L.zip :<->: L.unzip

-- |'L.zip3' three lists together.
zip3 :: ([a], [b], [c]) <-> [(a, b, c)]
zip3 = (\(a,b,c) -> L.zip3 a b c) :<->: L.unzip3

-- |'L.zip4' four lists together.
zip4 :: ([a], [b], [c], [d]) <-> [(a, b, c, d)]
zip4 = (\(a,b,c,d) -> L.zip4 a b c d) :<->: L.unzip4

-- |'L.zip5' five lists together.
zip5 :: ([a], [b], [c], [d], [e]) <-> [(a, b, c, d, e)]
zip5 = (\(a,b,c,d,e) -> L.zip5 a b c d e) :<->: L.unzip5

-- |'L.zip6' six lists together.
zip6 :: ([a], [b], [c], [d], [e], [f]) <-> [(a, b, c, d, e, f)]
zip6 = (\(a,b,c,d,e,f) -> L.zip6 a b c d e f) :<->: L.unzip6

-- |'L.zip7' seven lists together.
zip7 :: ([a], [b], [c], [d], [e], [f], [g]) <-> [(a, b, c, d, e, f, g)]
zip7 = (\(a,b,c,d,e,f,g) -> L.zip7 a b c d e f g) :<->: L.unzip7

-- |'L.zipWith' two lists together using a bijection.
zipWith :: (a, b) <-> c -> ([a], [b]) <-> [(c)]
zipWith (f :<->: g) = uncurry (L.zipWith (curry f)) :<->: L.unzip . L.map g

-- |(Un)interleave two lists, e.g., between @([2,5,11],[3,7])@ and @[2,3,5,7,11]@.
interleave :: ([a], [a]) <-> [a]
interleave = uncurry f :<->: g where
  f (x:xl) (y:yl) = x:y:f xl yl
  f [] l = l
  f l [] = l
  g (x:y:l) = (x:) *** (y:) $ g l 
  g l = (l, [])

-- |Split a string into 'L.lines'.
lines :: String <-> [String]
lines = L.lines :<->: L.unlines

-- |Split a string into 'L.words'.
words :: String <-> [String]
words = L.words :<->: L.unwords
