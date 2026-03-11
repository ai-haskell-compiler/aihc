{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds        #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-imports #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Binary.Orphans
-- Copyright   :  (C) 2015-2019 Oleg Grenrus
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Provides orphan 'Binary' instances for types in various packages:
--
--   * aeson
--   * case-insensitive
--   * hashable
--   * scientific (prior to scientific-0.3.4.0)
--   * tagged
--   * text (through text-binary, or text >= 1.2.1)
--   * time
--   * unordered-containers
--   * vector (through vector-binary-instances)
--
module Data.Binary.Instances () where

import Data.Binary.Orphans ()

import Data.Binary.Instances.Aeson ()
import Data.Binary.Instances.CaseInsensitive ()
import Data.Binary.Instances.Hashable ()
import Data.Binary.Instances.Primitive ()
import Data.Binary.Instances.Scientific ()
import Data.Binary.Instances.Tagged ()
import Data.Binary.Instances.Text ()
import Data.Binary.Instances.Time ()
import Data.Binary.Instances.UnorderedContainers ()
import Data.Binary.Instances.Vector ()
