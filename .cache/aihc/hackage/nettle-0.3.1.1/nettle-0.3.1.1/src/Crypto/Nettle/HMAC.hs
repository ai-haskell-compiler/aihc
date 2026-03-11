-----------------------------------------------------------------------------
-- |
-- Module      :  Crypto.Nettle.HMAC
-- Copyright   :  (c) 2013 Stefan Bühler
-- License     :  MIT-style (see the file COPYING)
-- 
-- Maintainer  :  stbuehler@web.de
-- Stability   :  experimental
-- Portability :  portable
--
-- Generic HMAC implementation based on the 'HashAlgorithm' class,
-- implementing the 'KeyedHashAlgorithm' class.
--
-----------------------------------------------------------------------------

module Crypto.Nettle.HMAC
	( HMAC
	, hmacInit
	, hmacInit'
	, hmac
	, hmac'
	, hmacLazy
	, hmacLazy'
	) where

import Crypto.Nettle.Hash.Types
