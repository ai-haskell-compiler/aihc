{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.AL.ALboolean
-- Copyright   :  (c) Sven Panne 2003-2016
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling ALboolean.
--
--------------------------------------------------------------------------------

module Sound.OpenAL.AL.ALboolean (
   marshalALboolean, unmarshalALboolean
) where

import Sound.OpenAL.AL.BasicTypes
import Sound.OpenAL.Constants

--------------------------------------------------------------------------------

marshalALboolean :: Bool -> ALboolean
marshalALboolean False = al_FALSE
marshalALboolean True  = al_TRUE

unmarshalALboolean :: ALboolean -> Bool
unmarshalALboolean = (/= al_FALSE)
