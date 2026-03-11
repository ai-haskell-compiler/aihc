{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Crypto.HPKE.KDF (
    KDF (..),
    HashAlgorithm,
    SHA256 (..),
    SHA384 (..),
    SHA512 (..),
    PRK,
    extractAndExpand,
)
where

import Crypto.Hash.Algorithms (
    HashAlgorithm,
    SHA256 (..),
    SHA384 (..),
    SHA512 (..),
 )
import Crypto.KDF.HKDF (PRK)
import qualified Crypto.KDF.HKDF as HKDF

import Crypto.HPKE.Types

----------------------------------------------------------------

class KDF h where
    labeledExtract :: Suite -> Salt -> Label -> IKM -> PRK h
    labeledExpand :: Suite -> PRK h -> Label -> Info -> Int -> Key

instance KDF SHA256 where
    labeledExtract = labeledExtract_
    labeledExpand = labeledExpand_

instance KDF SHA384 where
    labeledExtract = labeledExtract_
    labeledExpand = labeledExpand_

instance KDF SHA512 where
    labeledExtract = labeledExtract_
    labeledExpand = labeledExpand_

----------------------------------------------------------------

labeledExtract_
    :: HashAlgorithm a => Suite -> Salt -> Label -> IKM -> PRK a
labeledExtract_ suite salt label ikm = HKDF.extract salt labeled_ikm
  where
    labeled_ikm = "HPKE-v1" <> suite <> label <> ikm

labeledExpand_
    :: HashAlgorithm a => Suite -> PRK a -> Label -> Info -> Int -> Key
labeledExpand_ suite prk label info len = HKDF.expand prk labeled_info len
  where
    labeled_info =
        i2ospOf_ 2 (fromIntegral len) <> "HPKE-v1" <> suite <> label <> info

----------------------------------------------------------------

extractAndExpand
    :: forall h
     . (HashAlgorithm h, KDF h)
    => h -> Suite -> KeyDeriveFunction
extractAndExpand h suite dh kem_context = shared_secret
  where
    eae_prk :: PRK h
    eae_prk = labeledExtract suite "" "eae_prk" $ convert dh
    siz = hashDigestSize h
    shared_secret =
        labeledExpand suite eae_prk "shared_secret" kem_context siz
