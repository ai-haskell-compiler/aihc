{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module      : Crypto.Types.PubKey.DH
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : Stable
-- Portability : Excellent
--
module Crypto.Types.PubKey.DH
    ( Params(..)
    , PublicNumber(..)
    , PrivateNumber(..)
    , SharedKey(..)
    ) where

import Data.Data
import Data.ASN1.Types

-- | Represent Diffie Hellman parameters namely P (prime), and G (generator).
data Params = Params
    { params_p :: Integer
    , params_g :: Integer
    } deriving (Show,Read,Eq,Data,Typeable)

instance ASN1Object Params where
    toASN1 params = \xs -> Start Sequence
                           : IntVal (params_p params)
                           : IntVal (params_g params)
                           : End Sequence
                           : xs 

    fromASN1 (Start Sequence:IntVal p:IntVal g:End Sequence:xs) =
        Right (Params { params_p = p, params_g = g }, xs)
    fromASN1 _ = Left "fromASN1: DH.Params: unexpected format"

-- | Represent Diffie Hellman public number Y.
newtype PublicNumber = PublicNumber Integer
    deriving (Show,Read,Eq,Enum,Real,Num,Ord)

-- | Represent Diffie Hellman private number X.
newtype PrivateNumber = PrivateNumber Integer
    deriving (Show,Read,Eq,Enum,Real,Num,Ord)

-- | Represent Diffie Hellman shared secret.
newtype SharedKey = SharedKey Integer
    deriving (Show,Read,Eq,Enum,Real,Num,Ord)
