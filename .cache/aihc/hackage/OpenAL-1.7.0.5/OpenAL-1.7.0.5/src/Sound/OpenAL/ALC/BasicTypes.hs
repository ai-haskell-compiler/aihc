--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.ALC.BasicTypes
-- Copyright   :  (c) Sven Panne 2003-2016
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to the introductory parts of chapter 6 (AL Contexts
-- and the ALC API) of the OpenAL Specification and Reference (version 1.1).
--
-- The context API makes use of ALC types which are defined separately from the
-- AL types - there is an 'ALCboolean', 'ALCchar', etc.
--
--------------------------------------------------------------------------------

module Sound.OpenAL.ALC.BasicTypes (
   ALCboolean, ALCchar, ALCbyte, ALCubyte, ALCshort, ALCushort, ALCint, ALCuint,
   ALCsizei, ALCenum, ALCfloat, ALCdouble
) where

import Sound.OpenAL.Config
