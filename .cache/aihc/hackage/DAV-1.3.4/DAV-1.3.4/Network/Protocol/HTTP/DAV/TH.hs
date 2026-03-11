-- TH.hs: WebDAV client library - bits using TH
-- Copyright © 2012-2013  Clint Adams
--
-- vim: softtabstop=4:shiftwidth=4:expandtab
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE TemplateHaskell #-}

module Network.Protocol.HTTP.DAV.TH where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B
import Network.HTTP.Client (Manager, Request)

data Depth = Depth0 | Depth1 | DepthInfinity
instance Read Depth where
    readsPrec _ x
        | x == "0" = [(Depth0, "")]
        | x == "1" = [(Depth1, "")]
        | x == "infinity" = [(DepthInfinity, "")]
        | otherwise = fail "invalid"
instance Show Depth where
    show Depth0 = "0"
    show Depth1 = "1"
    show DepthInfinity = "infinity"

data DAVContext = DAVContext {
    _allowedMethods :: [B.ByteString]
  , _baseRequest :: Request
  , _basicusername :: B.ByteString
  , _basicpassword :: B.ByteString
  , _complianceClasses :: [B.ByteString]
  , _depth :: Maybe Depth
  , _httpManager :: Maybe Manager
  , _lockToken :: Maybe B.ByteString
  , _userAgent :: B.ByteString
}
makeLenses ''DAVContext
