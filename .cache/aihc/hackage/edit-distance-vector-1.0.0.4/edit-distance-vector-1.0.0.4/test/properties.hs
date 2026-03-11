{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Monad
import Data.Monoid
import qualified Data.Vector as V
import System.Exit
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Data.Vector.Distance

-- | Changes to a 'String' (or other sequence, really).
data C a
    = I Int a
    | D Int a
    | S Int a a
  deriving (Show, Eq)

-- | Apply a list of changes.
runC :: [C v] -> [v] -> [v]
runC [] l = l
runC (D i _ : r) l =
    let (h,t) = splitAt i l
        l' = h <> tail t
    in runC r l'
runC (I i a : r) l =
    let (h,t) = splitAt i l
        l' = h <> [a] <> t
    in runC r l'
runC (S i a a' : r) l =
    let (h,t) = splitAt i l
        l' = h <> [a'] <> tail t
    in runC r l'

-- | Edit parameters for 'String'.
str :: Params Char (C Char) (Sum Int)
str = Params{..}
  where
    equivalent = (==)
    delete = D
    insert = I
    substitute = S
    cost op = Sum 1
    positionOffset op = case op of
        D{} -> 0
        _   -> 1

-- | Patch extracted from identical documents should be mempty.
prop_distance_id :: String -> Bool
prop_distance_id s =
    let s' = V.fromList s
    in leastChanges str s' s' == (Sum 0, [])

-- | Delete everything!
prop_distance_delete :: NonEmptyList Char -> Bool
prop_distance_delete (NonEmpty s) =
    leastChanges str (V.fromList s) V.empty == (Sum $ length s, [ D 0 c | c <- s ])

-- | Insert everything!
prop_distance_insert :: NonEmptyList Char -> Bool
prop_distance_insert (NonEmpty s) =
    leastChanges str V.empty (V.fromList s) == (Sum $ length s, [ I i c | (i,c) <- zip [0..] s ])

-- | The examples from wikipedia.
prop_distance_canned :: Bool
prop_distance_canned =
    let sitting = V.fromList ("sitting" :: String)
        kitten = V.fromList ("kitten" :: String)
        saturday = V.fromList ("Saturday" :: String)
        sunday = V.fromList ("Sunday" :: String)
    in leastChanges str sitting kitten == (Sum 3, [S 0 's' 'k',S 4 'i' 'e',D 6 'g'])
    && leastChanges str kitten sitting == (Sum 3, [S 0 'k' 's',S 4 'e' 'i',I 6 'g'])
    && leastChanges str saturday sunday == (Sum 3, [D 1 'a',D 1 't',S 2 'r' 'n'])
    && leastChanges str sunday saturday == (Sum 3, [I 1 'a',I 2 't',S 4 'n' 'r'])

-- | Apply the found changes works.
--
-- @apply . leastChanges === id@
prop_distance_apply :: String -> String -> Bool
prop_distance_apply ss tt =
    tt == runC (snd $ leastChanges str (V.fromList ss) (V.fromList tt)) ss

--
-- Use Template Haskell to automatically run all of the properties above.
--

return []
runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = do
    result <- runTests
    unless result exitFailure
