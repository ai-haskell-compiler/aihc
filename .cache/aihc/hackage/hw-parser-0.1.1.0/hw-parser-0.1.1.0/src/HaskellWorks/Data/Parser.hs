{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module HaskellWorks.Data.Parser
  ( Parser(..)
  ) where

import Data.ByteString               (ByteString)
import Data.Text                     (Text)
import Data.Word
import HaskellWorks.Data.Char.IsChar

import qualified Data.Attoparsec.ByteString       as ABS
import qualified Data.Attoparsec.ByteString.Char8 as BC
import qualified Data.Attoparsec.Text             as AT
import qualified Data.Attoparsec.Types            as T

class Parser t e | t -> e where
  satisfy :: (e -> Bool) -> T.Parser t e
  satisfyWith :: (e -> a) -> (a -> Bool) -> T.Parser t a
  satisfyChar :: (Char -> Bool) -> T.Parser t Char
  string :: t -> T.Parser t t
  try :: T.Parser t a -> T.Parser t a
  char :: Char -> T.Parser t Char
  (<?>) :: T.Parser t Char -> String -> T.Parser t Char
  rational :: Fractional f => T.Parser t f

instance Parser ByteString Word8 where
  satisfy = ABS.satisfy
  satisfyWith = ABS.satisfyWith
  satisfyChar = ABS.satisfyWith toChar
  string = ABS.string
  try = ABS.try
  char = BC.char
  (<?>) = (BC.<?>)
  rational = BC.rational
  {-# INLINE satisfy     #-}
  {-# INLINE satisfyWith #-}
  {-# INLINE satisfyChar #-}
  {-# INLINE string      #-}
  {-# INLINE try         #-}
  {-# INLINE char        #-}
  {-# INLINE (<?>)       #-}
  {-# INLINE rational    #-}

instance Parser Text Char where
  satisfy = AT.satisfy
  satisfyWith = AT.satisfyWith
  satisfyChar = AT.satisfyWith toChar
  string = AT.string
  try = AT.try
  char = AT.char
  (<?>) = (AT.<?>)
  rational = AT.rational
  {-# INLINE satisfy     #-}
  {-# INLINE satisfyWith #-}
  {-# INLINE satisfyChar #-}
  {-# INLINE string      #-}
  {-# INLINE try         #-}
  {-# INLINE char        #-}
  {-# INLINE (<?>)       #-}
  {-# INLINE rational    #-}
