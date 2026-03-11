module Test.Strict where

import qualified Data.StorableVector as V
import qualified Data.ByteString as P
import qualified Data.List.HT as ListHT

import qualified Test.QuickCheck as QC
import Test.QuickCheck.Modifiers (Positive(Positive), )
import Test.QuickCheck (Property, quickCheck, )
import Test.Utility
          (V, W, X, P, applyId, applyModel,
           eq0, eq1, eq2, eqnotnull1, eqnotnull2, eqnotnull3, )


-- * compare Data.StorableVector <=> ByteString

newtype Size = Size {getSize :: Int}
   deriving (Show)

instance QC.Arbitrary Size where
   arbitrary = fmap Size $ QC.choose (0,10000)

prop_concatVP :: [V] -> Bool
prop_nullVP :: V -> Bool
prop_reverseVP :: V -> Bool
prop_transposeVP :: [V] -> Bool
prop_groupVP :: V -> Bool
prop_initsVP :: V -> Bool
prop_tailsVP :: V -> Bool
prop_allVP :: (W -> Bool) -> V -> Bool
prop_anyVP :: (W -> Bool) -> V -> Bool
prop_appendVP :: V -> V -> Bool
prop_breakVP :: (W -> Bool) -> V -> Bool
prop_concatMapVP :: (W -> V) -> V -> Bool
prop_consVP :: W -> V -> Bool
prop_countVP :: W -> V -> Bool
prop_dropVP :: X -> V -> Bool
prop_dropWhileVP :: (W -> Bool) -> V -> Bool
prop_filterVP :: (W -> Bool) -> V -> Bool
prop_findVP :: (W -> Bool) -> V -> Bool
prop_findIndexVP :: (W -> Bool) -> V -> Bool
prop_findIndicesVP :: (W -> Bool) -> V -> Bool
prop_isPrefixOfVP :: V -> V -> Bool
prop_mapVP :: (W -> W) -> V -> Bool
prop_replicateVP :: Size -> W -> Bool
prop_iterateVP :: Size -> (W -> W) -> W -> Bool
prop_snocVP :: V -> W -> Bool
prop_spanVP :: (W -> Bool) -> V -> Bool
prop_splitVP :: W -> V -> Bool
prop_splitAtVP :: X -> V -> Bool
prop_sieveVP :: Positive X -> V -> Bool
prop_sliceVerticalVP :: Positive X -> V -> Bool
prop_deinterleaveVP :: Positive X -> V -> Bool
prop_interleaveVP :: Positive X -> V -> Bool
prop_takeVP :: X -> V -> Bool
prop_takeWhileVP :: (W -> Bool) -> V -> Bool
prop_elemVP :: W -> V -> Bool
prop_notElemVP :: W -> V -> Bool
prop_elemIndexVP :: W -> V -> Bool
prop_elemIndicesVP :: W -> V -> Bool
prop_lengthVP :: V -> Bool
prop_headVP :: V -> Property
prop_initVP :: V -> Property
prop_lastVP :: V -> Property
prop_maximumVP :: V -> Property
prop_minimumVP :: V -> Property
prop_tailVP :: V -> Property
prop_foldl1VP :: (W -> W -> W) -> V -> Property
prop_foldl1VP' :: (W -> W -> W) -> V -> Property
prop_foldr1VP :: (W -> W -> W) -> V -> Property
prop_scanlVP :: (W -> W -> W) -> W -> V -> Property
prop_scanrVP :: (W -> W -> W) -> W -> V -> Property
prop_eqVP :: V -> V -> Bool
prop_foldlVP :: (X -> W -> X) -> X -> V -> Bool
prop_foldlVP' :: (X -> W -> X) -> X -> V -> Bool
prop_foldrVP :: (W -> X -> X) -> X -> V -> Bool
prop_mapAccumLVP :: (X -> W -> (X, W)) -> X -> V -> Bool
prop_mapAccumRVP :: (X -> W -> (X, W)) -> X -> V -> Bool
prop_zipWithVP :: (W -> W -> W) -> V -> V -> Bool

prop_concatVP       = V.concat  `eq1`  P.concat
prop_nullVP         = V.null  `eq1`  P.null
prop_reverseVP      = V.reverse  `eq1`  P.reverse
prop_transposeVP    = V.transpose  `eq1`  P.transpose
prop_groupVP        = V.group  `eq1`  P.group
prop_initsVP        = V.inits  `eq1`  P.inits
prop_tailsVP        = V.tails  `eq1`  P.tails
prop_allVP          = V.all  `eq2`  P.all
prop_anyVP          = V.any  `eq2`  P.any
prop_appendVP       = V.append  `eq2`  P.append
prop_breakVP        = V.break  `eq2`  P.break
prop_concatMapVP    = V.concatMap  `eq2`  P.concatMap
prop_consVP         = V.cons  `eq2`  P.cons
prop_countVP        = V.count  `eq2`  P.count
prop_dropVP         = V.drop  `eq2`  P.drop
prop_dropWhileVP    = V.dropWhile  `eq2`  P.dropWhile
prop_filterVP       = V.filter  `eq2`  P.filter
prop_findVP         = V.find  `eq2`  P.find
prop_findIndexVP    = V.findIndex  `eq2`  P.findIndex
prop_findIndicesVP  = V.findIndices  `eq2`  P.findIndices
prop_isPrefixOfVP   = V.isPrefixOf  `eq2`  P.isPrefixOf
prop_mapVP          = V.map  `eq2`  P.map
prop_replicateVP    = (\n -> V.replicate n  `eq1`  P.replicate n) . getSize
prop_iterateVP      = (\n f -> V.iterateN n f  `eq1`  P.pack . take n . iterate f) . getSize
prop_snocVP         = V.snoc  `eq2`  P.snoc
prop_spanVP         = V.span  `eq2`  P.span
prop_splitVP        = V.split  `eq2`  P.split
prop_splitAtVP      = V.splitAt  `eq2`  P.splitAt
prop_takeVP         = V.take  `eq2`  P.take
prop_takeWhileVP    = V.takeWhile  `eq2`  P.takeWhile
prop_elemVP         = V.elem  `eq2`  P.elem
prop_notElemVP      = V.notElem  `eq2`  P.notElem
prop_elemIndexVP    = V.elemIndex  `eq2`  P.elemIndex
prop_elemIndicesVP  = V.elemIndices  `eq2`  P.elemIndices
prop_lengthVP       = V.length  `eq1`  P.length

prop_headVP         = V.head  `eqnotnull1`  P.head
prop_initVP         = V.init  `eqnotnull1`  P.init
prop_lastVP         = V.last  `eqnotnull1`  P.last
prop_maximumVP      = V.maximum  `eqnotnull1`  P.maximum
prop_minimumVP      = V.minimum  `eqnotnull1`  P.minimum
prop_tailVP         = V.tail  `eqnotnull1`  P.tail
prop_foldl1VP       = V.foldl1  `eqnotnull2`  P.foldl1
prop_foldl1VP'      = V.foldl1'  `eqnotnull2`  P.foldl1'
prop_foldr1VP       = V.foldr1  `eqnotnull2`  P.foldr1
prop_scanlVP        = V.scanl  `eqnotnull3`  P.scanl
prop_scanrVP        = V.scanr  `eqnotnull3`  P.scanr

prop_sliceVerticalVP (Positive n) =
   V.sliceVertical n  `eq1`  (ListHT.sliceVertical n :: [W] -> [[W]])

prop_sieveVP (Positive n) =
   V.sieve n  `eq1`  (ListHT.sieve n :: [W] -> [W])

prop_deinterleaveVP (Positive n) =
   V.deinterleave n  `eq1`  (ListHT.sliceHorizontal n :: [W] -> [[W]])

prop_interleaveVP (Positive n) xs =
   let xss = ListHT.switchR [] const $ V.sliceVertical n xs
   in  V.interleave xss  ==  V.concat (V.transpose xss)

prop_eqVP =
   eq2
      ((==) :: V -> V -> Bool)
      ((==) :: P -> P -> Bool)
prop_foldlVP f b as =
   uncurry eq0
      ((V.foldl, P.foldl) `applyId` f `applyId` b `applyModel` as)
prop_foldlVP' f b as =
   uncurry eq0
      ((V.foldl', P.foldl') `applyId` f `applyId` b `applyModel` as)
prop_foldrVP f b as =
   uncurry eq0
      ((V.foldr, P.foldr) `applyId` f `applyId` b `applyModel` as)
prop_mapAccumLVP f b as =
   uncurry eq0
      ((V.mapAccumL, P.mapAccumL) `applyId` f `applyId` b `applyModel` as)
prop_mapAccumRVP f b as =
   uncurry eq0
      ((V.mapAccumR, P.mapAccumR) `applyId` f `applyId` b `applyModel` as)
prop_zipWithVP f xs ys =
   uncurry eq0
      ((V.zipWith f, \x y -> P.pack (P.zipWith f x y)) `applyModel` xs `applyModel` ys)

prop_unfoldrVP :: Size -> (X -> Maybe (W, X)) -> X -> Bool
prop_unfoldrVP n f =
   eq1
      (V.unfoldrN (getSize n) f)
      (P.unfoldrN (getSize n) f)


vp_tests :: [(String, IO ())]
vp_tests =
   ("all",         quickCheck prop_allVP) :
   ("any",         quickCheck prop_anyVP) :
   ("append",      quickCheck prop_appendVP) :
   ("concat",      quickCheck prop_concatVP) :
   ("cons",        quickCheck prop_consVP) :
   ("eq",          quickCheck prop_eqVP) :
   ("filter",      quickCheck prop_filterVP) :
   ("find",        quickCheck prop_findVP) :
   ("findIndex",   quickCheck prop_findIndexVP) :
   ("findIndices", quickCheck prop_findIndicesVP) :
   ("foldl",       quickCheck prop_foldlVP) :
   ("foldl'",      quickCheck prop_foldlVP') :
   ("foldl1",      quickCheck prop_foldl1VP) :
   ("foldl1'",     quickCheck prop_foldl1VP') :
   ("foldr",       quickCheck prop_foldrVP) :
   ("foldr1",      quickCheck prop_foldr1VP) :
   ("mapAccumL",   quickCheck prop_mapAccumLVP) :
   ("mapAccumR",   quickCheck prop_mapAccumRVP) :
   ("zipWith",     quickCheck prop_zipWithVP) :
   ("unfoldr",     quickCheck prop_unfoldrVP) :
   ("head",        quickCheck prop_headVP) :
   ("init",        quickCheck prop_initVP) :
   ("isPrefixOf",  quickCheck prop_isPrefixOfVP) :
   ("last",        quickCheck prop_lastVP) :
   ("length",      quickCheck prop_lengthVP) :
   ("map",         quickCheck prop_mapVP) :
   ("maximum",     quickCheck prop_maximumVP) :
   ("minimum",     quickCheck prop_minimumVP) :
   ("null",        quickCheck prop_nullVP) :
   ("reverse",     quickCheck prop_reverseVP) :
   ("snoc",        quickCheck prop_snocVP) :
   ("tail",        quickCheck prop_tailVP) :
   ("scanl",       quickCheck prop_scanlVP) :
   ("scanr",       quickCheck prop_scanrVP) :
   ("transpose",   quickCheck prop_transposeVP) :
   ("replicate",   quickCheck prop_replicateVP) :
   ("iterateN",    quickCheck prop_iterateVP) :
   ("take",        quickCheck prop_takeVP) :
   ("drop",        quickCheck prop_dropVP) :
   ("splitAt",     quickCheck prop_splitAtVP) :
   ("takeWhile",   quickCheck prop_takeWhileVP) :
   ("dropWhile",   quickCheck prop_dropWhileVP) :
   ("break",       quickCheck prop_breakVP) :
   ("span",        quickCheck prop_spanVP) :
   ("split",       quickCheck prop_splitVP) :
   ("count",       quickCheck prop_countVP) :
   ("group",       quickCheck prop_groupVP) :
   ("inits",       quickCheck prop_initsVP) :
   ("tails",       quickCheck prop_tailsVP) :
   ("elem",        quickCheck prop_elemVP) :
   ("notElem",     quickCheck prop_notElemVP) :
   ("elemIndex",   quickCheck prop_elemIndexVP) :
   ("elemIndices", quickCheck prop_elemIndicesVP) :
   ("concatMap",   quickCheck prop_concatMapVP) :
   ("sieve",       quickCheck prop_sieveVP) :
   ("sliceVertical", quickCheck prop_sliceVerticalVP) :
   ("deinterleave",  quickCheck prop_deinterleaveVP) :
   ("interleave",  quickCheck prop_interleaveVP) :
   []
