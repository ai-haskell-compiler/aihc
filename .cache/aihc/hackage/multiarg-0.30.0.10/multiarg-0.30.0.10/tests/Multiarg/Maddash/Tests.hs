{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE TemplateHaskell #-}
module Multiarg.Maddash.Tests where

import Control.Applicative
import Multiarg.Types
import Multiarg.Maddash
import Makeopt
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.QuickCheck
import Multiarg.Types.Instances ()
import Multiarg.Maddash.Instances ()
import Multiarg.Types.Instances ()

tests :: TestTree
tests = $(testGroupGenerator)

genInt :: Gen Int
genInt = arbitrary

singleDash :: Multiarg.Types.Word
singleDash = Multiarg.Types.Word "-"

stopper :: Multiarg.Types.Word
stopper = Multiarg.Types.Word "--"

genNonOptWord :: Gen Multiarg.Types.Word
genNonOptWord = oneof
  [ return singleDash
  , return stopper
  , do
      c1 <- arbitrary `suchThat` (/= '-')
      cs <- listOf arbitrary
      return $ Multiarg.Types.Word (c1 : cs)
  ]

genPending :: Arbitrary a => Gen (State a)
genPending = Pending <$> arbitrary <*> arbitrary

-- * Properties

-- | Non-option token always returns NotAnOption if State is Ready
prop_nonOptWordNotAnOptionIfStateIsReady :: Property
prop_nonOptWordNotAnOptionIfStateIsReady =
  forAll arbitrary $ \shorts ->
  forAll arbitrary $ \longs ->
  forAll genNonOptWord $ \token ->
  let (pallet, _) = processWord shorts longs Ready token
      _types = shorts :: [(ShortName, ArgSpec Int)]
  in pallet == NotAnOption

-- | Stopper always returns NotAnOption if State is Ready
prop_stopperNotAnOptionIfStateIsReady :: Property
prop_stopperNotAnOptionIfStateIsReady =
  forAll arbitrary $ \shorts ->
  forAll arbitrary $ \longs ->
  let (pallet, _) = processWord shorts longs Ready stopper
      _types = shorts :: [(ShortName, ArgSpec Int)]
  in pallet == NotAnOption

-- | Single dash always returns NotAnOption if State is Ready
prop_singleDashNotAnOptionIfStateIsReady :: Property
prop_singleDashNotAnOptionIfStateIsReady =
  forAll arbitrary $ \shorts ->
  forAll arbitrary $ \longs ->
  let (pallet, _) = processWord shorts longs Ready singleDash
      _types = shorts :: [(ShortName, ArgSpec Int)]
  in pallet == NotAnOption

-- | processWord never returns NotAnOption when input is Pending
prop_processWordNeverReturnsNotAnOptionOnPending =
  forAll arbitrary $ \shorts ->
  forAll arbitrary $ \longs ->
  forAll genPending $ \state ->
  forAll arbitrary $ \token ->
  let (pallet, _) = processWord shorts longs state token
      _types = shorts :: [(ShortName, ArgSpec Int)]
  in pallet /= NotAnOption

-- | NotAnOption is always returned with Ready
prop_processWordNotAnOptionWithReady =
  forAll arbitrary $ \shorts ->
  forAll arbitrary $ \longs ->
  forAll arbitrary $ \state ->
  forAll arbitrary $ \token ->
  let (pallet, state') = processWord shorts longs state token
      _types = shorts :: [(ShortName, ArgSpec Int)]
  in pallet == NotAnOption ==> isReady state'

pickOne :: [a] -> Gen a
pickOne ls
  | null ls = error "pickOne: null list"
  | otherwise = fmap (\ix -> ls !! ix) (choose (0, length ls - 1))

data OptionWithToks = OptionWithToks
  { owtOptName :: OptName
  , owtArgSpec :: ArgSpec Int
  , owtArgs :: [String]
  , owtWords :: [Multiarg.Types.Word]
  , owtResultOuts :: [[Output Int]]
  , owtResultToks :: Maybe [Multiarg.Types.Word]
  , owtExpected :: Int
  } deriving Show

instance Arbitrary OptionWithToks where
  arbitrary = do
    OptName on <- arbitrary
    as <- arbitrary
    (args, expected) <- case as of
      ZeroArg a -> return ([], a)
      OneArg f -> do
        s <- arbitrary
        return ([s], f s)
      TwoArg f -> do
        s1:s2:[] <- vectorOf 2 arbitrary
        return ([s1,s2], f s1 s2)
      ThreeArg f -> do
        s1:s2:s3:[] <- vectorOf 3 arbitrary
        return ([s1,s2,s3], f s1 s2 s3)
    let strings = case on of
          Left shrt -> processShortOptions [] (shrt, args)
          Right lng -> processLongOption lng args
    toks <- fmap (map Multiarg.Types.Word) $ pickOne strings
    let (shrts, lngs) = case on of
          Left shrt -> ([(shrt, as)], [])
          Right lng -> ([], [(lng, as)])
        (procRslts, procEi) = processWords shrts lngs toks
        mayToks = either (const Nothing) Just procEi
    return $ OptionWithToks (OptName on) as args toks procRslts
      mayToks expected

prop_optionWithToksResultToksEmpty = (== Just []) . owtResultToks

prop_optionWithToksResultIsExpected owt
  = concat (owtResultOuts owt) == [Good . owtExpected $ owt]

