{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : Test.KeyedVals.Type
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

This module provides types that demonstrate how to use @KeyVals.Handle.Typed@

The declared types are used in hspec tests used to validate implementations of 'Handle'
-}
module Test.KeyedVals.Types (
  -- * data types
  VarDemo (VarDemo),
  VarDemoKey,
  VarDemoID,
  FixedDemo (FixedDemo),
  FixedDemoKey,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.String (IsString)
import Data.Text (Text)
import KeyedVals.Handle.Codec.Aeson (AesonOf (..))
import KeyedVals.Handle.Codec.HttpApiData (HttpApiDataOf (..))
import KeyedVals.Handle.Typed
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))


{- | A simple type to illustrate storing key-values at varying storage paths.

it's just a simple type (Either) wrapped in newtype to avoid orphan
instances.
-}
newtype VarDemo = VarDemo (Either Text Bool)
  deriving (Eq, Show)
  deriving (FromJSON, ToJSON) via (Either Text Bool)


deriving via (AesonOf (Either Text Bool)) instance DecodeKV VarDemo


deriving via (AesonOf (Either Text Bool)) instance EncodeKV VarDemo


-- | The keys for each 'VarDemo' are @Int@s.
newtype VarDemoKey = VarDemoKey Int
  deriving stock (Eq, Show)
  deriving (ToHttpApiData, FromHttpApiData, Num, Ord) via Int
  deriving (DecodeKV, EncodeKV) via HttpApiDataOf Int


-- | Groups of 'VarDemo' are stored for different 'VarDemoID'.
newtype VarDemoID = VarDemoId Text
  deriving stock (Eq, Show)
  deriving (IsString, ToHttpApiData, FromHttpApiData) via Text
  deriving (DecodeKV, EncodeKV) via HttpApiDataOf Text


-- | Describe how @'VarDemo's@ are stored in the key-value store
instance PathOf VarDemo where
  type KVPath VarDemo = "/testing/{}/var"
  type KeyType VarDemo = VarDemoKey


{- | Specify how to derive the path to store @'VarDemo's@ in the key-value store

This instance uses 'expand' to replace the @{}@ in the 'KVPath' with the
variable portion of the key.
-}
instance VaryingPathOf VarDemo where
  type PathVar VarDemo = VarDemoID
  modifyPath _ = expand


{- | A simple type to illustrate storing key-values at a fixed storage path

it's just a simple type (tuple) wrapped in newtype to avoid orphan instances.
-}
newtype FixedDemo = FixedDemo (Int, Text)
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via (Int, Text)
  deriving (DecodeKV, EncodeKV) via AesonOf (Int, Text)


-- | Specify how @'FixedDemo's@ are stored in the key-value store
instance PathOf FixedDemo where
  type KVPath FixedDemo = "/testing/fixed"
  type KeyType FixedDemo = FixedDemoKey


-- | The keys for each 'FixedDemo' are @Int@s.
newtype FixedDemoKey = FixedDemoKey Int
  deriving stock (Eq, Show)
  deriving (ToHttpApiData, FromHttpApiData, Num, Ord) via Int
  deriving (DecodeKV, EncodeKV) via HttpApiDataOf Int
