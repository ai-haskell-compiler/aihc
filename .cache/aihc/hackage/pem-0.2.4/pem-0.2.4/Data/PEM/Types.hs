-- |
-- Module      : Data.PEM.Types
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
module Data.PEM.Types where

import Data.ByteString (ByteString)
import Basement.NormalForm

-- | Represent one PEM section
--
-- for now headers are not serialized at all.
-- this is just available here as a placeholder for a later implementation.
data PEM = PEM
    { pemName    :: String                 -- ^ the name of the section, found after the dash BEGIN tag.
    , pemHeader  :: [(String, ByteString)] -- ^ optionals key value pair header
    , pemContent :: ByteString             -- ^ binary content of the section
    } deriving (Show,Eq)

instance NormalForm PEM where
    toNormalForm pem =
        toNormalForm (pemName pem) `seq` nfLbs (pemHeader pem) `seq` pemContent pem `seq` ()
      where
        nfLbs []         = ()
        nfLbs ((s,bs):l) = toNormalForm s `seq` bs `seq` nfLbs l
