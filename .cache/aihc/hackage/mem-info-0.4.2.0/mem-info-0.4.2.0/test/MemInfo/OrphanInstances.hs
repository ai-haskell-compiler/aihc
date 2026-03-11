{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : MemInfo.OrphanInstances
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module MemInfo.OrphanInstances where

import Data.Fixed (Deci)
import Data.GenValidity (GenValid (..))
import Data.GenValidity.Text ()
import Data.List.NonEmpty (nonEmpty)
import System.MemInfo.Choices (
  Choices (..),
  Mem (..),
  Power,
  PrintOrder,
  Style,
  defaultRoot,
 )
import System.MemInfo.Proc (ExeInfo (..), StatusInfo)
import System.Posix.Types (CPid (..), ProcessID)
import Test.QuickCheck (Gen, chooseInt, elements, frequency, suchThat)
import Test.QuickCheck.Gen (listOf)
import Test.Validity (Validity)


instance GenValid ExeInfo where
  genValid = do
    eiDeleted <- genValid
    eiOriginal <- genValid
    let eiTarget = if eiDeleted then eiOriginal <> " (deleted)" else eiOriginal
    pure $ ExeInfo {eiDeleted, eiOriginal, eiTarget}


genPositive :: (GenValid a, Num a, Ord a) => Gen a
genPositive = genValid `suchThat` (> 0)


gen2Dp :: Gen Deci
gen2Dp = do
  dps <- fromIntegral <$> chooseInt (10, 99)
  strength <- elements [1.0, 10.0, 100.0]
  pure $ (dps / 10.0) * strength


deriving anyclass instance GenValid StatusInfo


deriving newtype instance Validity ProcessID


deriving newtype instance GenValid ProcessID


deriving instance Validity Choices


deriving instance Validity PrintOrder


deriving anyclass instance GenValid PrintOrder


deriving instance Validity Style


deriving instance GenValid Style


deriving instance Validity Power


deriving instance GenValid Power


deriving instance Validity Mem


deriving instance GenValid Mem


instance GenValid Choices where
  genValid =
    let genPositiveMb = frequency [(1, pure Nothing), (5, Just <$> genPositive)]
        genPids = nonEmpty <$> listOf genPositive
        genValidMem = Mem <$> genValid <*> gen2Dp
        genOptionalMem =
          frequency
            [ (1, pure Nothing)
            , (5, Just <$> genValidMem)
            ]
     in Choices
          <$> genValid
          <*> genValid
          <*> genValid
          <*> genValid
          <*> genValid
          <*> pure defaultRoot
          <*> genPositiveMb
          <*> genPids
          <*> genValid
          <*> genValid
          <*> genOptionalMem
