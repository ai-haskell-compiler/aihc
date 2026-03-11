{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  MapInteger
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2012 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  OverloadedStrings
--
--  This module defines the datatytpe mapping and relation values
--  used for RDF dataype @xsd:integer@.
--
--------------------------------------------------------------------------------

module Swish.RDF.Datatype.XSD.MapInteger (mapXsdInteger) where

import Swish.Datatype (DatatypeMap(..))

import qualified Data.Text as T
import qualified Data.Text.Read as T

------------------------------------------------------------
--  Implementation of DatatypeMap for xsd:integer
------------------------------------------------------------

-- | Functions that perform lexical-to-value
--  and value-to-canonical-lexical mappings for @xsd:integer@ values.
--
mapXsdInteger :: DatatypeMap Integer
mapXsdInteger = DatatypeMap
    { -- mapL2V :: T.Text -> Maybe Integer
      mapL2V = \txt -> case T.signed T.decimal txt of
         Right (val, "") -> Just val
         _ -> Nothing
         
      -- mapV2L :: Integer -> Maybe T.Text
      -- TODO: for now convert via String as issues with text-format
      --       (inability to use with ghci)   
    , mapV2L = Just . T.pack . show
    }

--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--    2011, 2012 Douglas Burke
--  All rights reserved.
--
--  This file is part of Swish.
--
--  Swish is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  Swish is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with Swish; if not, write to:
--    The Free Software Foundation, Inc.,
--    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--
--------------------------------------------------------------------------------
