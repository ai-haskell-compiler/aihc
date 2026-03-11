{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : Test.KeyedVals.CheckHandle
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetcorrectlyunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module Test.KeyedVals.CheckTypedHandle (
  -- * a test fixture
  spec,

  -- * setup/teardown hspec tests
  setupFixture,
  closeFixture,
) where

import qualified Data.Map.Strict as Map
import KeyedVals.Handle.Typed
import Test.KeyedVals.Prelude


spec :: SpecWith (Handle IO)
spec = do
  checkFixedPathed
  checkVarPathed


checkFixedPathed :: SpecWith (Handle IO)
checkFixedPathed = do
  context "with typed key-values stored in fixed paths" $ do
    it "should load correctly" $ \h -> do
      loadKVs h Fixed `endsRight` fixedKVs

    checkLength @FixedDemo Fixed 2

    it "should update an indexed value correctly" $ \h -> do
      let k = key fixedK1
          want = FixedDemo (1, "changed")
      endsRight_ $ saveTo h k want
      loadFrom h k `endsRight` want
      mayLoadFrom h k `endsRight` Just want

    checkLength @FixedDemo Fixed 2

    it "should add an indexed value correctly" $ \h -> do
      let added = key fixedK3
          want = FixedDemo (3, "added")
      mayLoadFrom h added `endsRight` Nothing
      endsRight_ $ saveTo h added want
      mayLoadFrom h added `endsRight` Just want

    checkLength @FixedDemo Fixed 3

    it "should update the key-values correctly" $ \h -> do
      endsRight_ $ updateKVs h Fixed moreFixedKVs
      mayLoadFrom h (key fixedK1) `endsRight` Just fixedV3
      mayLoadFrom h (key fixedK4) `endsRight` Just fixedV4

    it "should fetch a subset of the key-values as a dict correctly" $ \h -> do
      let selection = fixedK1 :| [fixedK4]
      loadSlice h Fixed selection `endsRight` moreFixedKVs


checkVarPathed :: SpecWith (Handle IO)
checkVarPathed = do
  context "with typed key-values stored in variable paths" $ do
    it "should load correctly" $ \h -> do
      loadKVs h path1 `endsRight` varKVs
      loadKVs h path2 `endsRight` varKVs

    checkLength path1 2
    checkLength path2 2

    it "should update an indexed value correctly" $ \h -> do
      let k = id1 // varK1
          want = VarDemo $ Right False
      endsRight_ $ saveTo h k want
      loadFrom h k `endsRight` want
      mayLoadFrom h k `endsRight` Just want

    it "should add an indexed value correctly" $ \h -> do
      let added = id2 // varK3
          want = VarDemo $ Left "added"
      mayLoadFrom h added `endsRight` Nothing
      endsRight_ $ saveTo h added want
      mayLoadFrom h added `endsRight` Just want

    checkLength path1 2
    checkLength path2 3

    it "should update the key-values correctly" $ \h -> do
      endsRight_ $ updateKVs h path1 moreVarKVs
      mayLoadFrom h (id1 // varK1) `endsRight` Just varV3
      mayLoadFrom h (id1 // varK4) `endsRight` Just varV4

    it "should fetch a subset of the key-values correctly" $ \h -> do
      let selection = varK1 :| [varK4]
      loadSlice h path1 selection `endsRight` moreVarKVs


setupFixture :: Handle IO -> IO (Handle IO)
setupFixture h = do
  orThrowHandleErr $ saveKVs h Fixed fixedKVs
  orThrowHandleErr $ saveKVs h path1 varKVs
  orThrowHandleErr $ saveKVs h path2 varKVs
  pure h


closeFixture :: Handle IO -> IO ()
closeFixture = close


fixedK1, fixedK2, fixedK3, fixedK4 :: FixedDemoKey
fixedK1 = 25
fixedK2 = 49
fixedK3 = 81
fixedK4 = 121


fixedV1, fixedV2, fixedV3, fixedV4 :: FixedDemo
fixedV1 = FixedDemo (1, "one")
fixedV2 = FixedDemo (2, "two")
fixedV3 = FixedDemo (1, "un")
fixedV4 = FixedDemo (4, "quatre")


fixedKVs, moreFixedKVs :: TypedKVs FixedDemo
fixedKVs =
  Map.fromList
    [ (fixedK1, fixedV1)
    , (fixedK2, fixedV2)
    ]
moreFixedKVs =
  Map.fromList
    [ (fixedK1, fixedV3)
    , (fixedK4, fixedV4)
    ]


varK1, varK2, varK3, varK4 :: VarDemoKey
varK1 = 36
varK2 = 64
varK3 = 100
varK4 = 144


path1, path2 :: TypedPath VarDemo
path1 = Variable id1
path2 = Variable id2


id1, id2 :: VarDemoID
id1 = "id1"
id2 = "id2"


varV1, varV2, varV3, varV4 :: VarDemo
varV1 = VarDemo $ Left "one"
varV2 = VarDemo $ Right False
varV3 = VarDemo $ Left "three"
varV4 = VarDemo $ Left "four"


varKVs, moreVarKVs :: TypedKVs VarDemo
varKVs =
  Map.fromList
    [ (varK1, varV1)
    , (varK2, varV2)
    ]
moreVarKVs =
  Map.fromList
    [ (varK1, varV3)
    , (varK4, varV4)
    ]


checkLength ::
  (Ord (KeyType v)) =>
  TypedPath v ->
  Natural ->
  SpecWith (Handle IO)
checkLength path n = context "and the reported size" $ do
  it "should be correct " $ \h -> do
    countKVs h path `endsRight` n
