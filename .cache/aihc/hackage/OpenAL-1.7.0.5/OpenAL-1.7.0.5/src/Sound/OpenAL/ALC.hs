--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.ALC
-- Copyright   :  (c) Sven Panne 2003-2016
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to chapter 6 (AL Contexts and the ALC API) of the
-- OpenAL Specification and Reference (version 1.1).
--
-- ALC is a portable API for managing OpenAL contexts, including resource
-- sharing, locking, and unlocking. Within the core AL API the existence of a
-- context is implied, but the context is not exposed. The context encapsulates
-- the state of a given instance of the AL state machine.
--
--------------------------------------------------------------------------------

module Sound.OpenAL.ALC (
   module Sound.OpenAL.ALC.BasicTypes,
   module Sound.OpenAL.ALC.Device,
   module Sound.OpenAL.ALC.Context,
   module Sound.OpenAL.ALC.Extensions,
   module Sound.OpenAL.ALC.Errors,
   module Sound.OpenAL.ALC.Capture
) where

import Sound.OpenAL.ALC.BasicTypes
import Sound.OpenAL.ALC.Device
import Sound.OpenAL.ALC.Context
import Sound.OpenAL.ALC.Extensions
import Sound.OpenAL.ALC.Errors
import Sound.OpenAL.ALC.Capture
