{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Data.Radius.Packet (
  codeToWord, codeFromWord,

  Code (..), Header (..), Packet (..),
  ) where

import Data.Word (Word8, Word16)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Data.Radius.Scalar (Bin128)


data Code
  = AccessRequest
  | AccessAccept
  | AccessReject

  | AccessChallenge

  | Other Word8
  deriving (Eq, Ord, Show)

codeToWord :: Code -> Word8
codeToWord =  d  where
  d AccessRequest    =   1
  d AccessAccept     =   2
  d AccessReject     =   3
  d AccessChallenge  =  11
  d (Other w) = w

codeFromWord :: Word8 -> Code
codeFromWord = d  where
  d  1  =  AccessRequest
  d  2  =  AccessAccept
  d  3  =  AccessReject
  d 11  =  AccessChallenge
  d  w  =  Other w


data Header =
  Header
  { code           ::  !Code
  , pktId          ::  !Word8
  , pktLength      ::  !Word16
  , authenticator  ::  !Bin128
  } deriving (Eq, Show)

data Packet a =
  Packet
  { header      ::  !Header
  , attributes  ::  !a
  } deriving (Eq, Show, Functor, Foldable, Traversable)
