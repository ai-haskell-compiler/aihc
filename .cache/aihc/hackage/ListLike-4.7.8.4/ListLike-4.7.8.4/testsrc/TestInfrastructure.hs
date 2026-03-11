{-# LANGUAGE CPP
            ,ScopedTypeVariables
            ,RankNTypes
            ,ExistentialQuantification
            ,MultiParamTypeClasses
            ,FunctionalDependencies
            ,FlexibleInstances
            ,UndecidableInstances
            ,TypeFamilies
            ,FlexibleContexts #-}
{-# OPTIONS -Wno-orphans #-}

{-
Copyright (C) 2007 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT

-}

-- FIXME -- better code is in offlineimap v7 branch
module TestInfrastructure where

import Test.QuickCheck
import Test.QuickCheck.Test
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ListLike as LL
import qualified Data.ListLike.Chars as Chars
import qualified Data.Array as A
import qualified Data.DList as DL
import qualified Data.FMList as FM
import qualified Data.Semigroup as Sem
import qualified Data.Sequence as S
--import qualified Data.Foldable as F
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.String.UTF8 as UTF8
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
--import System.Random
import System.IO
import qualified Test.HUnit as HU
import Text.Printf
import Data.Function (on)
import Data.String (IsString(fromString))
import Data.Word
import Data.List
import Data.Monoid (Monoid(..))
import GHC.Exts (IsList(Item))

simpleArb :: (LL.ListLike f i, Arbitrary i) => Gen f
simpleArb = sized (\n -> choose (0, n) >>= myVector)
    where myVector n = do
            arblist <- vector n
            return (LL.fromList arblist)

instance (Arbitrary i) => Arbitrary (MyList i) where
    arbitrary = sized (\n -> choose (0, n) >>= myVector)
        where myVector n =
                  do arblist <- vector n
                     return (LL.fromList arblist)
    shrink (MyList l) = map MyList $ shrink l

instance (CoArbitrary i) => CoArbitrary (MyList i) where
    coarbitrary l = coarbitrary (LL.toList l)

instance (Arbitrary i) => Arbitrary (DL.DList i) where
    arbitrary = simpleArb
    shrink = map LL.fromList . shrink . LL.toList

instance (CoArbitrary i) => CoArbitrary (DL.DList i) where
    coarbitrary l = coarbitrary (LL.toList l)

instance (Arbitrary i) => Arbitrary (FM.FMList i) where
    arbitrary = simpleArb
    shrink = map LL.fromList . shrink . LL.toList

instance (CoArbitrary i) => CoArbitrary (FM.FMList i) where
    coarbitrary l = coarbitrary (LL.toList l)

instance Arbitrary (BSL.ByteString) where
    arbitrary = sized (\n -> choose (0, n) >>= myVector)
        where myVector n =
                  do arblist <- vector n
                     return (LL.fromList arblist)
    shrink = map LL.fromList . shrink . LL.toList

instance CoArbitrary (BSL.ByteString) where
    coarbitrary l = coarbitrary (LL.toList l)

instance Arbitrary (BS.ByteString) where
    arbitrary = sized (\n -> choose (0, n) >>= myVector)
        where myVector n =
                  do arblist <- vector n
                     return (LL.fromList arblist)
    shrink = map LL.fromList . shrink . LL.toList

instance CoArbitrary (BS.ByteString) where
    coarbitrary l = coarbitrary (LL.toList l)

instance Arbitrary Chars.Chars where
    arbitrary = sized (\n -> choose (0, n) >>= myVector)
        where myVector n =
                  do arblist <- vector n
                     return (LL.fromList arblist)
    shrink = map LL.fromList . shrink . LL.toList

instance CoArbitrary Chars.Chars where
    coarbitrary l = coarbitrary (LL.toList l)

instance Arbitrary i => Arbitrary (A.Array Int i) where
    arbitrary = sized (\n -> choose (0, n) >>= myVector)
        where myVector n =
                  do arblist <- vector n
                     return (LL.fromList arblist)
    shrink = map LL.fromList . shrink . LL.toList

instance (CoArbitrary i) => CoArbitrary (A.Array Int i) where
    coarbitrary l = coarbitrary (LL.toList l)

instance Arbitrary (T.Text) where
    arbitrary = sized (\n -> choose (0, n) >>= myVector)
        where myVector n =
                  do arblist <- vector n
                     return (LL.fromList arblist)
    shrink = map LL.fromList . shrink . LL.toList

instance CoArbitrary (T.Text) where
    coarbitrary l = coarbitrary (LL.toList l)

instance Arbitrary (TL.Text) where
    arbitrary = sized (\n -> choose (0, n) >>= myVector)
        where myVector n =
                  do arblist <- vector n
                     return (LL.fromList arblist)
    shrink = map LL.fromList . shrink . LL.toList

instance CoArbitrary (TL.Text) where
    coarbitrary l = coarbitrary (LL.toList l)

instance Arbitrary (TB.Builder) where
    arbitrary = sized (\n -> choose (0, n) >>= myVector)
        where myVector n =
                  do arblist <- vector n
                     return (LL.fromList arblist)
    shrink = map LL.fromList . shrink . LL.toList

instance CoArbitrary (TB.Builder) where
    coarbitrary l = coarbitrary (LL.toList l)

instance Arbitrary (UTF8.UTF8 BS.ByteString) where
    arbitrary = sized (\n -> choose (0, n) >>= myVector)
        where myVector n =
                  do arblist <- vector n
                     return (LL.fromList arblist)
    shrink = map LL.fromList . shrink . LL.toList

instance CoArbitrary (UTF8.UTF8 BS.ByteString) where
    coarbitrary l = coarbitrary (LL.toList l)

instance Arbitrary (UTF8.UTF8 BSL.ByteString) where
    arbitrary = sized (\n -> choose (0, n) >>= myVector)
        where myVector n =
                  do arblist <- vector n
                     return (LL.fromList arblist)
    shrink = map LL.fromList . shrink . LL.toList

instance CoArbitrary (UTF8.UTF8 BSL.ByteString) where
    coarbitrary l = coarbitrary (LL.toList l)

instance Arbitrary i => Arbitrary (V.Vector i) where
    arbitrary = sized (\n -> choose (0, n) >>= myVector)
        where myVector n =
                  do arblist <- vector n
                     return (LL.fromList arblist)
    shrink = map LL.fromList . shrink . LL.toList

instance (CoArbitrary i) => CoArbitrary (V.Vector i) where
    coarbitrary l = coarbitrary (LL.toList l)

instance (Arbitrary i, VS.Storable i) => Arbitrary (VS.Vector i) where
    arbitrary = sized (\n -> choose (0, n) >>= myVector)
        where myVector n =
                  do arblist <- vector n
                     return (LL.fromList arblist)
    shrink = map LL.fromList . shrink . LL.toList

instance (CoArbitrary i, VS.Storable i) => CoArbitrary (VS.Vector i) where
    coarbitrary l = coarbitrary (LL.toList l)

instance (Arbitrary i, VU.Unbox i) => Arbitrary (VU.Vector i) where
    arbitrary = sized (\n -> choose (0, n) >>= myVector)
        where myVector n =
                  do arblist <- vector n
                     return (LL.fromList arblist)
    shrink = map LL.fromList . shrink . LL.toList

instance (CoArbitrary i, VU.Unbox i) => CoArbitrary (VU.Vector i) where
    coarbitrary l = coarbitrary (LL.toList l)

class (Show b, Arbitrary a, Show a, Eq a, Eq b, LL.ListLike a b) => TestLL a b where
  llcmp :: a -> [b] -> Property
  llcmp f l =  (putStrLn ("Expected: " ++ show l ++ "\nGot: " ++ show f))
               `whenFail` (l == (LL.toList f))
  checkLengths :: a -> [b] -> Bool
  checkLengths f l = (LL.length f) == length l

instance (Arbitrary a, Show a, Eq a) => TestLL [a] a where

instance (Arbitrary a, Show a, Eq a) => TestLL (MyList a) a

instance (Arbitrary a, Show a, Eq a) => TestLL (DL.DList a) a

instance (Arbitrary a, Show a, Eq a) => TestLL (FM.FMList a) a

instance TestLL BS.ByteString Word8 where

instance TestLL BSL.ByteString Word8 where

instance TestLL Chars.Chars Char where

instance (Arbitrary a, Show a, Eq a) => TestLL (S.Seq a) a where

instance (Arbitrary a, Show a, Eq a) => TestLL (A.Array Int a) a where

instance TestLL T.Text Char where

instance TestLL TL.Text Char where

instance TestLL TB.Builder Char where

instance TestLL (UTF8.UTF8 BS.ByteString) Char where

instance TestLL (UTF8.UTF8 BSL.ByteString) Char where

instance (Arbitrary a, Show a, Eq a) => TestLL (V.Vector a) a where

instance (Arbitrary a, Show a, Eq a, VS.Storable a) => TestLL (VS.Vector a) a where

instance (Arbitrary a, Show a, Eq a, VU.Unbox a) => TestLL (VU.Vector a) a where

instance Eq a => Eq (FM.FMList a) where
    (==) = (==) `on` FM.toList

mapRemoveDups :: (Eq k1) => [(k1, v1)] -> [(k1, v1)]
mapRemoveDups = nubBy (\(k1, _) (k2, _) -> k1 == k2)

data MyList a = MyList [a]
    deriving (Ord, Eq, Show)

instance LL.FoldableLL (MyList a) a where
    foldr f i (MyList x) = foldr f i x
    foldl f i (MyList x) = foldl f i x
    foldr1 f (MyList x) = foldr1 f x
    foldl1 f (MyList x) = foldl1 f x

instance Sem.Semigroup (MyList a) where
    MyList x <> MyList y = MyList (x ++ y)

instance Monoid (MyList a) where
    mempty = MyList []
    mappend = (Sem.<>)

instance IsList (MyList a) where
    type Item (MyList a) = a
    toList (MyList xs) = xs
    fromList = MyList

instance LL.ListLike (MyList a) a where
    singleton x = MyList [x]
    -- Andreas Abel, 2023-10-10, issue #32:
    -- Avoid 'head' and 'tail' from 'Prelude' and thus the x-partial warning of GHC 9.8.
    head (MyList (x:xs)) = x
    head (MyList [])     = undefined
    tail (MyList (x:xs)) = MyList xs
    tail (MyList []    ) = undefined
    null (MyList x) = null x

instance IsString (MyList Char) where
    fromString = MyList

instance LL.StringLike (MyList Char) where
    toString (MyList x) = x

mkTest :: Testable prop => String -> prop -> HU.Test
mkTest msg test = HU.TestLabel msg $ HU.TestCase (quickCheckResult test >>= HU.assertBool msg . isSuccess)

-- Modified from HUnit
runVerbTestText :: HU.PutText st -> HU.Test -> IO (HU.Counts, st)
runVerbTestText (HU.PutText put us) test = do
  (counts, us') <- HU.performTest reportStart reportError reportFailure us test
  us'' <- put (HU.showCounts counts) True us'
  return (counts, us'')
 where
  reportStart ss us' = do
    hPrintf stderr "\rTesting %-68s\n" (HU.showPath (HU.path ss))
    put (HU.showCounts (HU.counts ss)) False us'
  reportError   = reportProblem "Error:"   "Error in:   "
  reportFailure = reportProblem "Failure:" "Failure in: "
  reportProblem p0 p1 _mloc msg ss us' = put line True us'
   where line  = "### " ++ kind ++ path' ++ '\n' : msg
         kind  = if null path' then p0 else p1
         path' = HU.showPath (HU.path ss)


-- | So we can test map and friends
instance Show (a -> b) where
    show _ = "(a -> b)"

data LLTest f i =
    forall t. Testable t => LLTest (f -> t)

data LLWrap f' f i =
         forall t. Testable t => LLWrap (f' -> t)

w :: TestLL f i => String -> LLTest f i -> HU.Test
w msg (LLTest theTest) = mkTest msg theTest

ws :: (LL.StringLike f, TestLL f i) => String -> LLTest f i -> HU.Test
ws = w

wwrap :: (TestLL f i, TestLL f' f) => String -> LLWrap f' f i -> HU.Test
wwrap msg f = case f of
                   LLWrap theTest -> mkTest msg theTest

t :: forall f t i. (TestLL f i, Arbitrary f, Arbitrary i, Show f, Eq f, Testable t) => (f -> t) -> LLTest f i
t = LLTest

-- | all props, wrapped list
apw :: String -> (forall f' f i. (TestLL f i, Show i, Eq i, LL.ListLike f i, Eq f, Show f, Arbitrary f, Arbitrary i, LL.ListLike f' f, Show f', TestLL f' f, Arbitrary f', Eq f') => LLWrap f' f i) -> HU.Test
apw msg x = HU.TestLabel msg $ HU.TestList $
    [wwrap "wrap [[Int]]" (x::LLWrap [[Int]] [Int] Int),
     wwrap "wrap MyList (MyList Int)" (x::LLWrap (MyList (MyList Int)) (MyList Int) Int),
     wwrap "wrap S.Seq (S.Seq Int)" (x::LLWrap (S.Seq (S.Seq Int)) (S.Seq Int) Int),
     wwrap "wrap Array (Array Int)" (x::LLWrap (A.Array Int (A.Array Int Int)) (A.Array Int Int) Int),
     wwrap "wrap Array [Int]" (x::LLWrap (A.Array Int [Int]) [Int] Int)
    ,wwrap "wrap (Vector (Vector Int))" (x::LLWrap (V.Vector (V.Vector Int)) (V.Vector Int) Int)
     ]

-- | all props, 1 args: full
apf :: String -> (forall f i. (Ord i, TestLL f i, Show i, Eq i, LL.ListLike f i, Eq f, Show f, Arbitrary f, Arbitrary i, CoArbitrary f, CoArbitrary i) => LLTest f i) -> HU.Test
apf msg x = HU.TestLabel msg $ HU.TestList $
    [w "[Int]" (x::LLTest [Int] Int),
     w "MyList Int" (x::LLTest (MyList Int) Int),
     w "String" (x::LLTest String Char),
     w "[Bool]" (x::LLTest [Bool] Bool),
     w "MyList Bool" (x::LLTest (MyList Bool) Bool),
     w "ByteString" (x::LLTest BS.ByteString Word8),
     w "ByteString.Lazy" (x::LLTest BSL.ByteString Word8),
     w "Chars" (x::LLTest Chars.Chars Char),
     w "Sequence Int" (x::LLTest (S.Seq Int) Int),
     w "Sequence Bool" (x::LLTest (S.Seq Bool) Bool),
     w "Sequence Char" (x::LLTest (S.Seq Char) Char),
     w "Array Int Int" (x::LLTest (A.Array Int Int) Int),
     w "Array Int Bool" (x::LLTest (A.Array Int Bool) Bool),
     w "Array (Just Int)" (x::LLTest (A.Array Int (Maybe Int)) (Maybe Int)),
     w "DList Int" (x::LLTest (DL.DList Int) Int),
     -- w "FMList Int" (x::LLTest (FM.FMList Int) Int),
     w "Vector Int" (x::LLTest (V.Vector Int) Int),
     w "StorableVector Int" (x::LLTest (VS.Vector Int) Int),
     w "UnboxVector Int" (x::LLTest (VU.Vector Int) Int),
     w "Vector Bool" (x::LLTest (V.Vector Bool) Bool),
     w "StorableVector Bool" (x::LLTest (VS.Vector Bool) Bool),
     w "UnboxVector Bool" (x::LLTest (VU.Vector Bool) Bool),
     w "Text" (x::LLTest T.Text Char),
     w "Text.Lazy" (x::LLTest TL.Text Char),
     w "Text.Builder" (x::LLTest TB.Builder Char),
     w "UTF8 ByteString" (x::LLTest (UTF8.UTF8 BS.ByteString) Char),
     w "UTF8 ByteString.Lazy" (x::LLTest (UTF8.UTF8 BSL.ByteString) Char)
    ]

-- | all props, 1 args: full
aps :: String -> (forall f i. (Ord i, TestLL f i, Show i, Eq i, LL.StringLike f, LL.ListLike f i, Eq f, Show f, Arbitrary f, Arbitrary i) => LLTest f i) -> HU.Test
aps msg x = HU.TestLabel msg $ HU.TestList $
    [w "String" (x::LLTest String Char),
     w "MyList Char" (x::LLTest (MyList Char) Char),
     w "Sequence Char" (x::LLTest (S.Seq Char) Char),
     w "DList Char" (x::LLTest (DL.DList Char) Char),
     -- w "FMList Char" (x::LLTest (FM.FMList Char) Char),
     -- w "ByteString" (x::LLTest BS.ByteString Word8),
     -- w "ByteString.Lazy" (x::LLTest BSL.ByteString Word8),
     w "Chars" (x::LLTest Chars.Chars Char),
     w "Array Int Char" (x::LLTest (A.Array Int Char) Char),
     w "Text" (x::LLTest T.Text Char),
     w "Text.Lazy" (x::LLTest TL.Text Char),
     w "Text.Builder" (x::LLTest TB.Builder Char),
     w "UTF8 ByteString" (x::LLTest (UTF8.UTF8 BS.ByteString) Char),
     w "UTF8 ByteString.Lazy" (x::LLTest (UTF8.UTF8 BSL.ByteString) Char),
     w "Vector Char" (x::LLTest (V.Vector Char) Char),
     w "Vector.Unbox Char" (x::LLTest (VU.Vector Char) Char)
    ]
