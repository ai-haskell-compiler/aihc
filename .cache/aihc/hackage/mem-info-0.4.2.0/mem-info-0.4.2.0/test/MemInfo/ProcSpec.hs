{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : MemInfo.ProcSpec
Copyright   : (c) 2023 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module MemInfo.ProcSpec (
  spec,
) where

import Data.Hashable (hash)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word16)
import Fmt (blockMapF, build, fmt, (+|), (|+))
import MemInfo.Files.Smap (genBaseSmap, genSmapLine, genWithPss)
import MemInfo.OrphanInstances ()
import Numeric.Natural (Natural)
import System.MemInfo.Proc
import Test.Hspec
import Test.QuickCheck
import Test.Validity (GenValid (..), forAllValid)
import Test.Validity.GenValidity (genValidSpec)


spec :: Spec
spec = describe "module System.MemInfo.Proc" $ do
  genValidSpec @ExeInfo
  exeInfoSpec
  statusInfoSpec
  fromStatmSpec
  fromSmapSpec


exeInfoSpec :: Spec
exeInfoSpec = describe "parseExeInfo" $ do
  it "should parse all valid values successfully" $ do
    forAllValid $ \ei -> ei == parseExeInfo (eiTarget ei)


genOthers :: Gen [(Text, Word16)]
genOthers = do
  keys <- sublistOf otherStatusFields `suchThat` (not . null)
  vals <- vectorOf (length keys) arbitrary
  pure $ zip keys vals


statusInfoSpec :: Spec
statusInfoSpec = describe "parseStatusInfo" $ do
  it "should parse all valid values successfully" $ do
    forAll genStatusInfoContent $ \(si, txt) -> Right si == parseStatusInfo txt


genProcStatus :: StatusInfo -> Gen [(Text, Text)]
genProcStatus status = do
  others <- fmap (fmap (\(x, y) -> (x, fmt $ build y))) genOthers
  pure $ others <> asFields status


genStatusInfoContent :: Gen (StatusInfo, Text)
genStatusInfoContent = do
  si <- genValid
  txt <- fmt . blockMapF <$> genProcStatus si
  pure (si, txt)


asFields :: StatusInfo -> [(Text, Text)]
asFields si =
  [ ("Name", siName si)
  , ("PPid", fmt $ build $ toInteger $ siParent si)
  ]


otherStatusFields :: [Text]
otherStatusFields = ["Uid", "Gid", "FDSize", "Ngid", "Threads", "Cpus_allowed"]


fromStatmSpec :: Spec
fromStatmSpec = describe "parseFromStatm" $ do
  describe "when using a kernel version with unknown sharing" $ do
    it "should parse values to ProcUsage successfully" prop_roundtripStatmNotShared
  describe "when using a kernel version with known sharing" $ do
    it "should parse values to ProcUsage successfully" prop_roundtripStatmShared

  describe "When the statm content is invalid" $ do
    it "should not parse values to ProcUsage successfully" prop_roundtripInvalidStatm


fromSmapSpec :: Spec
fromSmapSpec = describe "parseFromSmap" $ do
  it "should parse values to ProcUsage successfully" prop_roundtripSmap


prop_roundtripStatmShared :: Property
prop_roundtripStatmShared =
  discardAfter 5000000 $
    forAll genSharedStatm $
      \(pp, txt) -> Just pp == parseFromStatm sharedKernel txt


prop_roundtripStatmNotShared :: Property
prop_roundtripStatmNotShared =
  forAll genNoSharedStatm $
    \(pp, txt) -> Just pp == parseFromStatm badSharedKernel txt


prop_roundtripInvalidStatm :: Property
prop_roundtripInvalidStatm =
  forAll genNoSharedStatm $
    \(_, txt) -> isNothing $ parseFromStatm badSharedKernel $ invalidateStatm txt


prop_roundtripSmap :: Property
prop_roundtripSmap = forAll genSmap $ \(pp, txt) -> pp == parseFromSmap txt


badSharedKernel :: (Natural, Natural, Natural)
badSharedKernel = (2, 6, 1)


sharedKernel :: (Natural, Natural, Natural)
sharedKernel = (2, 7, 1)


statmNoShared :: Word16 -> Text
statmNoShared rss = "0 " +| toInteger rss |+ " 1 2 3 4"


genNoSharedStatm :: Gen (ProcUsage, Text)
genNoSharedStatm = do
  rssKb <- genValid
  let content = statmNoShared rssKb
      pp =
        ppZero
          { puPrivate = fromIntegral rssKb * pageSizeKiB
          , puMemId = hash content
          }
  pure (pp, content)


invalidateStatm :: Text -> Text
invalidateStatm = Text.replace " " "-"


statmShared :: Word16 -> Word16 -> Text
statmShared rss shared = "0 " +| toInteger rss |+ " " +| toInteger shared |+ " 1 2 3"


genSharedStatm :: Gen (ProcUsage, Text)
genSharedStatm = do
  rssKb <- genValid `suchThat` (> 1) :: Gen Word16
  sharedKb <- genValid `suchThat` (< rssKb) :: Gen Word16
  let content = statmShared rssKb sharedKb
      pp =
        ppZero
          { puPrivate = fromIntegral (rssKb - sharedKb) * pageSizeKiB
          , puMemId = hash content
          , puShared = fromIntegral sharedKb * pageSizeKiB
          }
  pure (pp, content)


pageSizeKiB :: Int
pageSizeKiB = 4


ppZero :: ProcUsage
ppZero =
  ProcUsage
    { puPrivate = 0
    , puShared = 0
    , puSharedHuge = 0
    , puSwap = 0
    , puMemId = 0
    }


genSmap :: Gen (ProcUsage, Text)
genSmap = oneof [genBaseSmap, genWithSwapPss, genWithPss, genWithPss >>= genAppendSwapPss]


genWithSwapPss :: Gen (ProcUsage, Text)
genWithSwapPss = genBaseSmap >>= genAppendSwapPss


genAppendSwapPss :: (ProcUsage, Text) -> Gen (ProcUsage, Text)
genAppendSwapPss (pp, initial) = do
  (swapPss, txt) <- genSmapLine "SwapPss"
  let content = initial <> "\n" <> txt
  pure (pp {puSwap = swapPss, puMemId = hash content}, content)
