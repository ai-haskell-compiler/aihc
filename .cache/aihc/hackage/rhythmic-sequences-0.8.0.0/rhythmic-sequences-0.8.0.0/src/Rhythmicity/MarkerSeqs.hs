{-# LANGUAGE NoImplicitPrelude, BangPatterns #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Rhythmicity.MarkerSeqs
-- Copyright   :  (c) Oleksandr Zhabenko 2022-2023
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
-- 
-- Data and algorithmic basics to evaluate rhythmicity of the lists of 'Ord' instance data type.
-- Similar to @phonetic-languages-rhythmicity@ on Hackage.

module Rhythmicity.MarkerSeqs where

import GHC.Num
import GHC.Real
import GHC.Base
import GHC.Word
import Data.List hiding (foldr)
import Data.Ord (Down(..)) 
import GHC.Show
import Data.Bits
import Numeric (showIntAtBase,showInt)
import Data.Foldable (Foldable)
import GHC.Int
import Data.Char (isDigit)
import Data.Maybe (mapMaybe, catMaybes)
import Rhythmicity.BasicF
import Text.Read
import GHC.Enum (fromEnum)
import GHC.Arr (listArray,unsafeAt)
import Data.Tuple (fst,snd)

-- | The similar function is since @base-4.16.0.0@ in the 'Numeric' module. Is not used 
-- further, is provided here mostly for testing purposes.
showBin :: Int -> [Char]
showBin x = '0':'b':showIntAtBase 2 (head . flip showInt "") x ""
{-# INLINE showBin #-}

-- | Basic counting of the same bits in the 'Bits' arguments.
unionCount :: (Bits a) => a -> a -> Integer
unionCount x = toInteger . popCount . (.&.) x
{-# INLINE unionCount #-}

-- | Some idea function to evaluate the rhythmicity data. Is not used further in the package,
-- can be thought of as an alternative way of computation.
countWeightsQs :: (Foldable t) => [t a -> Int] -> [t a] -> [[Int]]
countWeightsQs fs xs = map (flip map xs) fs

-- | Data type used to provide somewhat \'array sorting with its indices\'.
data Sort2 a = S2 { 
 id :: Int8,
 val :: a
}

instance Eq a => Eq (Sort2 a) where
  S2 _ x == S2 _ y = x == y

instance Ord a => Ord (Sort2 a) where
  compare (S2 _ x) (S2 _ y) = compare y x
  S2 _ x > S2 _ y = x < y
  S2 _ x < S2 _ y = x > y
  S2 _ x >= S2 _ y = x <= y
  S2 _ x <= S2 _ y = x >= y

instance Functor Sort2 where
  fmap f (S2 k x) = S2 k . f $ x

instance Show a => Show (Sort2 a) where
  show (S2 k x) = show k ++ '~':show x

-- | Data type to contain the needed for hashing algorithm information about the sorted 
-- \'array sorting with its indices\'.
data ASort3 a = As3 { 
 id3 :: Int8,
 orD :: Int8,
 val3 :: a
}

instance Eq a => Eq (ASort3 a) where
  As3 _ _ x == As3 _ _ y = x == y

instance Show a => Show (ASort3 a) where
  show (As3 n k x) = show n ++ '&':show k ++ '~':show x

-- | Split the list into lists of @n@ elements where @n@ is the first parameter. Can be used 
-- efficiently just for the finite lists. Contains the modified code of the 'Data.List.unfoldr'
-- function from the @base@ package.
splitF :: Int -> [a] -> [[a]]
splitF n ys = let (q,r) = length ys `quotRem` n in take (if r > 0 then q + 1 else q) . g (splitAt n) $ ys
  where {-# INLINE g #-} -- Is a modified 'Data.List.unfoldr' code.
        g f b0 = build (\c n ->
          let go b = case f b of
                      (a, new_b) -> a `c` go new_b in go b0)

-- | Function to get basic data for hash-based evaluation of the rhythmicity of the list data. Is
-- used internally in the 'countHashesG'.
-- Provided here mostly for testing purposes.
getHashes2 
  :: Ord a => Int8 -- ^ A period of the groups (the length of the lists into which the general sequence is splitted at first).
  -> [Int8] -- ^ The list must be sorted in the descending order, the elements must be greater or equal to 0 and less than the first argument of 'getHashes2' here; besides, there must not be repetitions (any duplicates) in the list so all the elements must be pairwise not equal.  
  -> [a] -- ^ A list of 'Ord' data values that is evaluated for its rhythmic properties.
  -> [[Integer]]
getHashes2 selmarkNum ks xs = map (map toNum . filter (not . null) . map (idList ks) .  -- before this mapping the smallest element can potentially have 'orD' equal to 0 or greater than 0. The greatest element has 'orD' equal to @selmarkNum - 1@ (@= periodLength - 1@).
 g [selmarkNum-1,selmarkNum-2..] . sortOn GHC.Base.id . 
   zipWith S2 [selmarkNum-1,selmarkNum-2..]) . splitF (fromIntegral selmarkNum) $ xs
     where g (q:qs) xs@(x:ys) = let (js,rs) = span (== x) ys in map (\(S2 k y) -> As3 k q y) (x:js) : g qs rs
           g _ _ = []

-- | Convert hashes into basic simler data to evaluate rhythmicity of the list data.
countHashesPrioritized tss@(ts:vs:xss) = zipWith unionCount ts vs : countHashesPrioritized (vs:xss)
countHashesPrioritized _ = []

-- | Mostly for testing.
count1Hashes 
  :: Ord a => Int8
  -> [Int8]
  -> [a]
  -> Integer
count1Hashes groupLength ks = sum . map createNewHash . countHashesPrioritized . getHashes2 groupLength ws 
    where !ws = sortOn Down . filter (>= 0) $ ks
{-# INLINE count1Hashes #-}

{-| Data type to encode the changes  that are introduced by the position  of the group 
 of values in general sequence to the general result of the 'createHashesG' function. If the second parameter  in the 'HashCorrections' is 1 then the result is more 
 sensitive to beginning of the line; if it is set to 2 then the result is more sensitive
 to ending of the line; if it is greater than 2 then the result is sensitive to some
user weights provided as the first parameter to 'HashCorrections' and otherwise 
the computation result does not depend on the first parameter to 'HashCorrections' (this
one can be considered  the basic option for the computation).
-}
data HashCorrections = H [Int8] Int8 deriving (Eq, Show)

hashCorrections2F :: HashCorrections -> (Int8 -> [Integer] -> Integer)
hashCorrections2F (H _ k) 
 | k > 0  = hashPosLF2
 | k == 0 = hashBalancingLF2
 | otherwise = hashBasicLF2
{-# INLINE hashCorrections2F #-}

{-| If the second parameter  in the 'HashCorrections' is 1 then the result is more 
 sensitive to beginning of the line; if it is set to 2 then the result is more sensitive
 to ending of the line; if it is greater than 2 then the result is sensitive to some
user weights provided as the first parameter to 'HashCorrections' and otherwise 
the computation result does not depend on the first parameter to 'HashCorrections' (this
one can be considered  the basic option for the computation).
-}
hashList :: HashCorrections -> [Int8]
hashList (H _ 1) = [24,23..]
hashList (H _ 2) = [1..21] `mappend` cycle [0]
hashList (H xs _) = xs `mappend` cycle [0]
{-# INLINE hashList #-}

-- | If you would like to specify just your own values then specify the 'String' \"a...\" where
-- \'a\' here means the minus sign \'-\' or some not equal to 1 or 2 digit, 
-- instead of dots specify some digits that are the beginning of the ['Int8'] list in
-- 'HashCorrections'. If \'a\' is \'-\', then the next not equal to \'a\' symbol should be
-- some digit not equal to 1 or 2 if you want to specify your own list of @[Int8]@ for 
-- 'HashCorrections'.
--
-- Caution: 
-- 
-- > readHashCorrections . show $ xs /= xs
--
-- > show . readHashCorrections $ xs /= xs
--
-- in general case. The default value is @H [0,0..] 0@. This one corresponds to usage of the
-- 'hashBalancingLF2' without any corrections (equi-sensitive to all the parts of the line except 
-- probably the last syllables if the number of syllables is not wholely divisible without remainder
-- to the groupLength parameter in the 'countHashesG' function). And this is equivalent to just
-- use the 'hashBasicLF2'.
-- 
readHashCorrections :: String -> HashCorrections
readHashCorrections xs = if length ys > 1 then let (ts,us) = splitAt 1 ys in H (map (\x -> read [x]::Int8) us) (if sgn then (-(read ts::Int8)) else (read ts::Int8)) else H [0,0..] 0
   where ys = filter (\x -> isDigit x) xs
         sgn = any (== '-') xs

-- | This is used to provide the second and the third arguments to 'countHashesG' function. The
-- default value is @(4,[3,2])@. This means that the line is divided into groups of 4-syllables 
-- then there are searched for rhythmic repetitions of the positions of the most maximum values
-- and the less maximum values. This scheme should is related to disyllables metrical feet for SaaW 
-- (syllables-as-a-whole) mode of operation for PhLADiPreLiO (see: 
-- https://oleksandrzhabenko.github.io/uk/rhythmicity/PhLADiPreLiO.Eng.21.html#SaaW).
-- For more information on the metrical feet you can see e. g. 
--
-- > @article{hyde2002restrictive,
-- >   title={A restrictive theory of metrical stress},
-- >   author={Hyde, Brett},
-- >   journal={Phonology},
-- >   volume={19},
-- >   number={3},
-- >   pages={313--359},
-- >   year={2002},
-- >   publisher={Cambridge University Press}
-- > }
--
grouppingR :: String -> (Int8, [Int8])
grouppingR xs = if length ys > 1 then let (ts,us) = splitAt 1 ys in (read ts::Int8, map (\x -> read [x]::Int8) us) else (4,[3,2])
   where ys = take 8 . filter (\x -> isDigit x) $ xs

-- | General implementation of  the hash-based algorithm to evaluate the level of rhythmicity 
-- of the list data. The relatively greater result (for PhLADiPreLiO) corresponds to greater detected periodicity.
countHashesG 
  :: Ord a => HashCorrections -- ^ Data that specifies how the arguments influence the result. Somewhat the kernel of the 'countHashesG' computation.
  -> Int8 -- ^ The period of the length of the initial list.
  -> [Int8] -- ^ List of ordinary positions of the maximum-minimum levels for values of the list in the group. The length of the unique elements together in the list is expected to be
  -- in the list [1..7]. 
  -> [a]
  -> [Integer]
countHashesG hc groupLength ks  = -- sum . 
  zipWith (createHashG f) positions . countHashesPrioritized . getHashes2 groupLength ws
   where f = hashCorrections2F hc
         positions = hashList hc
         !ws = sortOn Down . filter (>= 0) $ ks
{-# INLINE countHashesG #-}

-- | General implementation of  the hash-based algorithm to evaluate the level of rhythmicity 
-- of the list data. The relatively greater result (for PhLADiPreLiO) corresponds to greater detected periodicity.
countHashes2G 
  :: Ord a => Int -- ^ The first parameter for 'createHash2G' function — the step of hashing shift. For 'countHashesG' it is equal to 20 (the default sensible value). Is expected to be greater than 2.
  -> HashCorrections -- ^ Data that specifies how the arguments influence the result. Somewhat the kernel of the 'countHashesG' computation.
  -> Int8 -- ^ The period of the length of the initial list.
  -> [Int8] -- ^ List of ordinary positions of the maximum-minimum levels for values of the list in the group. The length of the unique elements together in the list is expected to be
  -- in the list [1..7]. 
  -> [a]
  -> [Integer]
countHashes2G k hc groupLength ks  = -- sum . 
  zipWith (createHash2G k hc) positions . countHashesPrioritized . getHashes2 groupLength ws
   where positions = hashList hc
         !ws = sortOn Down . filter (>= 0) $ ks
{-# INLINE countHashes2G #-}

-- | Provided for testing.
createNewHash :: [Integer] -> Integer
createNewHash (x1:x2:x3:x4:x5:x6:x7:_) = sum [shiftL x1 120, shiftL x2 100, shiftL x3 80, shiftL x4 60, shiftL x5 40, shiftL x6 20, x7]
createNewHash (x1:x2:x3:x4:x5:x6:_) = sum [shiftL x1 120, shiftL x2 100, shiftL x3 80, shiftL x4 60, shiftL x5 40, shiftL x6 20]
createNewHash (x1:x2:x3:x4:x5:_) = sum [shiftL x1 120, shiftL x2 100, shiftL x3 80, shiftL x4 60, shiftL x5 40]
createNewHash (x1:x2:x3:x4:_) = sum [shiftL x1 120, shiftL x2 100, shiftL x3 80, shiftL x4 60]
createNewHash (x1:x2:x3:_) = sum [shiftL x1 120, shiftL x2 100, shiftL x3 80]
createNewHash (x1:x2:_) = sum [shiftL x1 120, shiftL x2 100]
createNewHash (x1:_) = shiftL x1 120
createNewHash _ = 0

-- | General implementation of the second hashing of the data for the algorithm.
createHashG :: (Int8 -> [Integer] -> Integer) -> Int8 -> [Integer] -> Integer
createHashG f pos = f pos . zipWith (\n x -> shift x (n*20)) [6,5..0]
{-# INLINE createHashG #-}

-- | General implementation of the second hashing of the data for the algorithm with the additional
-- parameter that specifies the step of hashing (by default, e. g. in 'createHashG' it is equal to
-- 20, but here you can provide your own value). Therefore, is more flexible than 'createHashG', but
-- can lead to not well coordinated evaluations in general case that wipe by hashing some
-- information in the data. Is intended that the first argument is greater than 2 though it is not
-- checked.
createHash2G :: Int -> HashCorrections -> Int8 -> [Integer] -> Integer
createHash2G k hc@(H _ 0) pos = (hashBalancingLF2G k) pos . zipWith (\n x -> shift x (n*k)) [6,5..0]
createHash2G k (H _ m) pos = (if m > 0 then hashPosLF2 else hashBasicLF2) pos . zipWith (\n x -> shift x (n*k)) [6,5..0]
{-# INLINE createHash2G #-}

-- | A variant of the 'createHashG' that actually must be equal to the 'createNewHash' for the
-- second argument lists 
-- with less than 8 elements. For greater values is not correctly defined, so do not use it for 
-- the lists with 8 or more elements in them. Actually should be equal to 'createNewHash' for the
-- second argument.
createNHash :: [Int8] -> [Integer] -> Integer
createNHash _ = createNewHash . take 7
{-# INLINE createNHash #-}

-- | Function to filter the elements by the second parameter of the 'ASort3' data 
-- and then to get the first ones.
idList :: Eq a => [Int8] -> [ASort3 a] -> [Int8]
idList orDs ys = map (\(As3 k _ _) -> k) . filter (\(As3 _ n _) -> n `elem` orDs) $ ys

-- | Function to create bitwise representation of the intermediate data for the algorithm.
-- Should be very optimized to run fast.
toNum :: [Int8] -> Integer
toNum xs = foldl' setBit 0 . map fromEnum $ xs

-- | The alternative implementation of the 'toNum' (on the Linux x86_64 for some CPU is 
-- slower than the former one).
toNum2 :: [Int8] -> Integer
toNum2 xs = (sum . map (shiftL 1 . fromEnum) $ xs)::Integer

------------------------------------------------------

-- | Function for generating the information to be used for evaluation of the points of the uncongruencies' influences on the pauses in the case of the period of the line is equal to 2 (two-syllable meter).
--  See for the theoretical idea the paper by the link:
--  https://www.academia.edu/105067761/Why_some_lines_are_easy_to_pronounce_and_others_are_not_or_prosodic_unpredictability_as_a_characteristic_of_text (English text)
--  https://www.academia.edu/105067723/%D0%A7%D0%BE%D0%BC%D1%83_%D0%B4%D0%B5%D1%8F%D0%BA%D1%96_%D1%80%D1%8F%D0%B4%D0%BA%D0%B8_%D0%BB%D0%B5%D0%B3%D0%BA%D0%BE_%D0%B2%D0%B8%D0%BC%D0%BE%D0%B2%D0%BB%D1%8F%D1%82%D0%B8_%D0%B0_%D1%96%D0%BD%D1%88%D1%96_%D0%BD%D1%96_%D0%B0%D0%B1%D0%BE_%D0%BF%D1%80%D0%BE%D1%81%D0%BE%D0%B4%D0%B8%D1%87%D0%BD%D0%B0_%D0%BD%D0%B5%D1%81%D0%BF%D1%80%D0%BE%D0%B3%D0%BD%D0%BE%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D1%96%D1%81%D1%82%D1%8C_%D1%8F%D0%BA_%D1%85%D0%B0%D1%80%D0%B0%D0%BA%D1%82%D0%B5%D1%80%D0%B8%D1%81%D1%82%D0%B8%D0%BA%D0%B0_%D1%82%D0%B5%D0%BA%D1%81%D1%82%D1%83 (Ukrainian text)
showZerosFor2Period 
  :: (Ord a) => [[a]] -- The representation of the data to be analysed.
  -> Int 
  -> (b -> String)
  -> [[[b]]]
  -> (String, [Integer])
showZerosFor2Period structData syllN f sylls = (breaks, rs)
  where rs = concat . countHashesPrioritized . getHashes2 2 [1] . mconcat $ structData
        indeces = findIndices (== 0) rs
        resSylls = listArray (0,syllN - 1) . map (concatMap f) . concat $ sylls
        addlist 
           | syllN `rem` 2 == 0 = [0,1,2] : ((splitF 2 . take (syllN - 6) $ [3,4..]) `mappend` [[syllN - 3, syllN - 2, syllN - 1]])
           | otherwise = [0,1,2] : (splitF 2 . take (syllN - 3) $ [3,4..])
        arrinds = listArray (0,syllN `quot` 2) addlist
        neededinds = map (unsafeAt arrinds) indeces
        neededsylls = map (map (unsafeAt resSylls)) neededinds
        breaks = intercalate " ... " . map unwords $ neededsylls

-- | Function for generating the information to be used for evaluation of the points of the uncongruencies' influences on the pauses in the case of the period of the line is equal to 2 (two-syllable meter). Anologue of the 'swohZerosFor2Period' but is intended to be used for the \"music\" mode of PhLADiPreLiO.
--  See for the theoretical idea the paper by the link:
--  https://www.academia.edu/105067761/Why_some_lines_are_easy_to_pronounce_and_others_are_not_or_prosodic_unpredictability_as_a_characteristic_of_text (English text)
--  https://www.academia.edu/105067723/%D0%A7%D0%BE%D0%BC%D1%83_%D0%B4%D0%B5%D1%8F%D0%BA%D1%96_%D1%80%D1%8F%D0%B4%D0%BA%D0%B8_%D0%BB%D0%B5%D0%B3%D0%BA%D0%BE_%D0%B2%D0%B8%D0%BC%D0%BE%D0%B2%D0%BB%D1%8F%D1%82%D0%B8_%D0%B0_%D1%96%D0%BD%D1%88%D1%96_%D0%BD%D1%96_%D0%B0%D0%B1%D0%BE_%D0%BF%D1%80%D0%BE%D1%81%D0%BE%D0%B4%D0%B8%D1%87%D0%BD%D0%B0_%D0%BD%D0%B5%D1%81%D0%BF%D1%80%D0%BE%D0%B3%D0%BD%D0%BE%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D1%96%D1%81%D1%82%D1%8C_%D1%8F%D0%BA_%D1%85%D0%B0%D1%80%D0%B0%D0%BA%D1%82%D0%B5%D1%80%D0%B8%D1%81%D1%82%D0%B8%D0%BA%D0%B0_%D1%82%D0%B5%D0%BA%D1%81%D1%82%D1%83 (Ukrainian text)
showZerosFor2PeriodMusic 
  :: [(String, Word8)] -- The representation of the data to be analysed.
  -> (String, [Integer])
showZerosFor2PeriodMusic qqs = (breaks, rs)
  where rs = concat . countHashesPrioritized . getHashes2 2 [1] . map snd $ qqs
        syllN = length qqs
        indeces = findIndices (== 0) rs
        resSylls = listArray (0,syllN - 1) . map fst $ qqs
        addlist
          | syllN `rem` 2 == 0 = [0,1,2] : ((splitF 2 . take (syllN - 6) $ [3,4..]) `mappend` [[syllN - 3, syllN - 2, syllN - 1]])
          | otherwise = [0,1,2] : (splitF 2 . take (syllN - 3) $ [3,4..])
        arrinds = listArray (0,syllN `quot` 2) addlist
        neededinds = map (unsafeAt arrinds) indeces
        neededsylls = map (map (unsafeAt resSylls)) neededinds
        breaks = intercalate " ... " . map unwords $ neededsylls
