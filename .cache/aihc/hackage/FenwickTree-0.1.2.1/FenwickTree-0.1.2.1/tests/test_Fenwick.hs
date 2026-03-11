{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Tree.Fenwick
import Data.List(sort)

import Test.QuickCheck
import Test.QuickCheck.All

tol = 0.001

infix 4 ==~

class AEq a where
  (==~) :: a -> a -> Bool

instance AEq Double where
  (==~) a b = abs (a - b) <= tol

instance (AEq a, AEq b) => AEq (a, b) where
  (a, b) ==~ (c, d) = (a ==~ c) && (b ==~ d)

instance (AEq a) => AEq [a] where
  []     ==~ []     = True
  (b:bs) ==~ (c:cs) = (b ==~ c) && (bs ==~ cs)
  _      ==~ _      = False

instance (AEq a) => AEq (Maybe a) where
  Nothing  ==~ Nothing  = True
  (Just f) ==~ (Just g) = f ==~ g
  _        ==~ _        = False

emptyFT :: FTree (Double, Double)
emptyFT = empty getFreq cmpFst

getFreq (pos, freq)         = freq

cmpFst  (pos1, _) (pos2, _) = pos1 `compare` pos2

absFreq (a, freq) = if aFreq == 0.0
                      then (a, 0.001)
                      else (a, aFreq)
  where
   aFreq = abs freq

-- Prepare a list of unique values

uniq []     = []
uniq (e:es) = (e:) . uniq . filter (/= e) $ es

mkTree = foldr insert emptyFT

prop_insert_toList ls = toList (mkTree uls) == sort uls
  where
    uls = uniq ls

prop_insert_query_non_zero l ls = query l (insert l ft) ==~ snd l + query l ft
  where
    ft = mkTree $ filter (/=l) ls

prop_freqList ls = toFreqList (mkTree uls) ==~ zip (tail $ scanl (\a b -> snd b + a) 0.0 uls) uls
  where
    uls = uniq $ sort ls

prop_freqList_query l ls = query l ft ==~ lookupFL l (toFreqList ft)
  where
    uls = uniq ls
    ft  = insert l (mkTree uls)

lookupFL a ((f, b):_ ) | a == b = f
lookupFL a ((f, b):cs)          = lookupFL a cs
lookupFL a  []                  = 0.0
-- prop_insert_freqList

prop_toList_fromList ls = toList (fromList cmpFst getFreq uls) == uls
  where
    uls = sort $ uniq ls

prop_size_fromList ls = size (mkTree uls) == length uls
  where
    uls = uniq ls

prop_depth_fromList ls = (d <= l) && ((floor . logBase 2 . fromIntegral) l <= d)
  where
    d = depth (mkTree uls)
    l = length uls
    uls = uniq ls

prop_freqList_invQuery q ls = ((jf /= Nothing) && (sumFreq > 0)) ==> jf ==~ lookupFreq q (toFreqList ft)
  where
    jf      = invQuery q ft
    uls     = uniq $ map absFreq ls
    ft      = mkTree uls
    sumFreq = sum $ map snd ls

lookupFreq :: Double -> [(Double, (Double, Double))] -> Maybe (Double, Double)
lookupFreq q ((f, b):_ ) | q <= f = Just b
lookupFreq q ((f, b):cs) | q >  f = lookupFreq q cs
lookupFreq q  []                  = Nothing

main = $quickCheckAll

