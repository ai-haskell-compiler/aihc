-- |
--
module Data.Radius.Attribute.Number (
  Number (..),

  toWord, fromWord,
  ) where

import Data.Word (Word8)


data Number
  = UserName
  | ProxyState
  | State
  | MessageAuthenticator
  | ReplyMessage
  | VendorSpecific
  | Other !Word8
  deriving (Eq, Ord, Show, Read)

toWord :: Number -> Word8
toWord = d  where
  d UserName              =   1
  d ReplyMessage          =  18
  d State                 =  24
  d VendorSpecific        =  26
  d ProxyState            =  33
  d MessageAuthenticator  =  80
  d (Other w8)            =  w8

fromWord :: Word8 -> Number
fromWord = d  where
  d  1  =  UserName
  d 18  =  ReplyMessage
  d 24  =  State
  d 26  =  VendorSpecific
  d 33  =  ProxyState
  d 80  =  MessageAuthenticator
  d w8  =  Other w8
