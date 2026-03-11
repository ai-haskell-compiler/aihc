{-# LANGUAGE OverloadedStrings #-}

module MemInfo.Files.Smap (
  -- * data types

  -- * functions
  genSmapLine,
  genBaseSmap',
  genBaseSmap,
  genWithPss,
) where

import Data.GenValidity (GenValid (..))
import Data.Hashable (hash)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word16)
import Fmt ((+|), (|+))
import MemInfo.OrphanInstances ()
import System.MemInfo.Proc (ProcUsage (..))
import Test.QuickCheck


genSmapLine :: Text -> Gen (Int, Text)
genSmapLine prefix = do
  x <- genValid :: Gen Word16
  let txt = "" +| prefix |+ ": " +| x |+ " kB"
  pure (fromIntegral x, txt)


genBaseSmap :: Gen (ProcUsage, Text)
genBaseSmap = do
  (pp, txt, _) <- genBaseSmap'
  pure (pp, txt)


ppZero :: ProcUsage
ppZero =
  ProcUsage
    { puPrivate = 0
    , puShared = 0
    , puSharedHuge = 0
    , puSwap = 0
    , puMemId = 0
    }


genBaseSmap' :: Gen (ProcUsage, Text, Int)
genBaseSmap' = do
  (clean, cleanTxt) <- genSmapLine "Private_Clean"
  (dirty, dirtyTxt) <- genSmapLine "Private_Dirty"
  (sharedClean, shCleanTxt) <- genSmapLine "Shared_Clean"
  (sharedDirty, shDirtyTxt) <- genSmapLine "Shared_Dirty"
  (privateHuge, phTxt) <- genSmapLine "Private_Hugetlb"
  (sharedHuge, shTxt) <- genSmapLine "Shared_Hugetlb"
  (swap, swapTxt) <- genSmapLine "Swap"
  let pp =
        ppZero
          { puPrivate = clean + dirty + privateHuge
          , puMemId = hash content
          , puSwap = swap
          , puSharedHuge = sharedHuge
          , puShared = sharedClean + sharedDirty
          }
      content =
        Text.unlines
          [ phTxt
          , dirtyTxt
          , cleanTxt
          , swapTxt
          , shTxt
          , shCleanTxt
          , shDirtyTxt
          ]
  pure (pp, content, privateHuge)


genWithPss :: Gen (ProcUsage, Text)
genWithPss = genBaseSmap' >>= genAppendPss


genAppendPss :: (ProcUsage, Text, Int) -> Gen (ProcUsage, Text)
genAppendPss (pp, initial, puPrivateHuge) = do
  (pss, txt) <- genSmapLine "Pss"
  let content = initial <> "\n" <> txt
      newShared = pss - (puPrivate pp - puPrivateHuge)
  pure (pp {puShared = newShared, puMemId = hash content}, content)
