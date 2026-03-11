-----------------------------------------------------------------------------
-- |
-- Module      :  Crypto.Nettle.KeyedHash
-- Copyright   :  (c) 2013 Stefan Bühler
-- License     :  MIT-style (see the file COPYING)
-- 
-- Maintainer  :  stbuehler@web.de
-- Stability   :  experimental
-- Portability :  portable
--
-- Generic interface to calculate key based hashes.
--
-----------------------------------------------------------------------------

module Crypto.Nettle.KeyedHash
	( KeyedHashAlgorithm(..)
	, KeyedHash(..)

	, keyedHashDigestSize
	, keyedHashDigestSize'
	, keyedHashName
	, keyedHashName'
	, keyedHashInit
	, keyedHashInit'
	, keyedHashUpdate
	, keyedHashUpdateLazy
	, keyedHashFinalize
	, keyedHash
	, keyedHash'
	, keyedHashLazy
	, keyedHashLazy'
	) where

import Crypto.Nettle.Hash.Types
