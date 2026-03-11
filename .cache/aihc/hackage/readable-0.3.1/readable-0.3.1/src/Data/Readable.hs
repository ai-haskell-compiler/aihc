{-# LANGUAGE OverloadedStrings #-}

{-|

The Read type class is very useful for building data types from String
representations.  But String has high overhead, so sometimes it isn't suitable
for applications where space usage and performance are important.  This
library provides a simpler version of Read's functionality for Text and
ByteStrings.

-}

module Data.Readable
  ( Readable(..)
  ) where

------------------------------------------------------------------------------
import           Control.Monad
import           Data.ByteString.Char8 (ByteString)
import           Data.Int
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.Text.Read
import           Data.Word


------------------------------------------------------------------------------
-- | ByteString and Text reading using MonadPlus to handle parse failure.  On
-- error, fromText and fromBS will return mzero.  You can use mplus to provide
-- fallback defaults.
class Readable a where
    -- | Reads data from a Text representation.
    fromText :: MonadPlus m => Text -> m a
    -- | Reads data from a UTF8 encoded ByteString.  The default
    -- implementation of this function simply decodes with UTF-8 and then
    -- calls the fromText function.  If decoding fails, mzero will be
    -- returned.  You can provide your own implementation if you need
    -- different behavior such as not decoding to UTF8.
    fromBS   :: MonadPlus m => ByteString -> m a
    fromBS = fromText <=< hushPlus . decodeUtf8'


hushPlus :: MonadPlus m => Either a b -> m b
hushPlus (Left _) = mzero
hushPlus (Right b) = return b


------------------------------------------------------------------------------
-- | Fails if the input wasn't parsed completely.
checkComplete :: MonadPlus m => (t, Text) -> m t
checkComplete (a,rest)
  | T.null rest = return a
  | otherwise   = mzero


-- Leaving out these instances breaks users who depend on having a unified
-- constraint for parsing, so we need to keep them around.
instance Readable ByteString where
    fromText = return . encodeUtf8
    fromBS = return
instance Readable Text where
    fromText = return

instance Readable Int where
    fromText = either (const mzero) checkComplete . signed decimal
instance Readable Integer where
    fromText = either (const mzero) checkComplete . signed decimal
instance Readable Float where
    fromText = either (const mzero) checkComplete . rational
instance Readable Double where
    fromText = either (const mzero) checkComplete . double
instance Readable Bool where
    fromText t = case T.toLower t of
                   "1" -> return True
                   "0" -> return False
                   "t" -> return True
                   "f" -> return False
                   "true" -> return True
                   "false" -> return False
                   "y" -> return True
                   "n" -> return False
                   "yes" -> return True
                   "no" -> return False
                   _ -> mzero

instance Readable Int8 where
    fromText = either (const mzero) checkComplete . signed decimal
instance Readable Int16 where
    fromText = either (const mzero) checkComplete . signed decimal
instance Readable Int32 where
    fromText = either (const mzero) checkComplete . signed decimal
instance Readable Int64 where
    fromText = either (const mzero) checkComplete . signed decimal

instance Readable Word8 where
    fromText = either (const mzero) checkComplete . decimal
instance Readable Word16 where
    fromText = either (const mzero) checkComplete . decimal
instance Readable Word32 where
    fromText = either (const mzero) checkComplete . decimal
instance Readable Word64 where
    fromText = either (const mzero) checkComplete . decimal
