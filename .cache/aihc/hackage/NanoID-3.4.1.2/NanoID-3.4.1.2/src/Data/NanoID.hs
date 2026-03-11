{-# LANGUAGE CPP           #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.NanoID where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Char8 as C
import           Data.Maybe

#if !MIN_VERSION_base(4,11,0)
import           Data.Monoid           ((<>))
#endif

import           Data.Serialize        (Serialize)
import           Data.Text.Encoding
import           GHC.Generics
import           Numeric.Natural
import           System.Random.MWC

newtype NanoID =
  NanoID { unNanoID :: C.ByteString }
  deriving (Eq, Generic)

newtype Alphabet =
  Alphabet { unAlphabet :: C.ByteString }
  deriving (Eq)

type Length = Natural

instance Show NanoID where
  show = C.unpack . unNanoID

instance Show Alphabet where
  show = C.unpack . unAlphabet

instance ToJSON NanoID where
  toJSON n = String (decodeUtf8 $ unNanoID n)

instance FromJSON NanoID where
  parseJSON (String s) = pure (NanoID $ encodeUtf8 s)
  parseJSON _          = fail "A JSON String is expected to convert to NanoID"

instance Serialize NanoID

-- | Create a new 'Alphabet' from a string of symbols of your choice
toAlphabet :: String -> Alphabet
toAlphabet = Alphabet . C.pack

-- | Standard 'NanoID' generator function
--
-- >λ: createSystemRandom >>= nanoID
-- >x2f8yFadIm-Vp14ByJ8R3
--
nanoID :: GenIO -> IO NanoID
nanoID = customNanoID defaultAlphabet 21

-- | Customable 'NanoID' generator function
customNanoID
  :: Alphabet  -- ^ An 'Alphabet' of your choice
  -> Length    -- ^ A 'NanoID' length (the standard length is 21 chars)
  -> GenIO     -- ^ The pseudo-random number generator state
  -> IO NanoID
customNanoID a l g =
  let
    ua = unAlphabet a
    al = C.length ua
    l' = fromEnum l
  in
    NanoID . C.pack <$> replicateM l' ((\r -> C.index ua (r-1)) <$> uniformR (1,al) g)

-- | The default 'Alphabet', made of URL-friendly symbols.
defaultAlphabet :: Alphabet
defaultAlphabet = toAlphabet "ABCDEFGHIJKLMNOPKRSTUVWXYZ_1234567890-abcdefghijklmnopqrstuvwxyz"

-- * Some predefined 'Alphabet's, borrowed from https://github.com/CyberAP/nanoid-dictionary

numbers :: Alphabet
numbers = toAlphabet "1234567890"

hexadecimalLowercase :: Alphabet
hexadecimalLowercase = toAlphabet "0123456789abcdef"

hexadecimalUppercase :: Alphabet
hexadecimalUppercase = toAlphabet "0123456789ABCDEF"

lowercase :: Alphabet
lowercase = toAlphabet "abcdefghijklmnopqrstuvwxyz"

uppercase :: Alphabet
uppercase = toAlphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

alphanumeric :: Alphabet
alphanumeric = toAlphabet "ABCDEFGHIJKLMNOPKRSTUVWXYZ1234567890abcdefghijklmnopqrstuvwxyz"

nolookalikes :: Alphabet
nolookalikes = toAlphabet "346789ABCDEFGHJKLMNPQRTUVWXYabcdefghijkmnpqrtwxyz"

nolookalikesSafe :: Alphabet
nolookalikesSafe = toAlphabet "6789ABCDEFGHJKLMNPQRTUWYabcdefghijkmnpqrtwyz"

-- * Special password

specialPassword :: Alphabet
specialPassword = toAlphabet "67{8_9A!B>CDEF)GH=JKL(MNPQ%RTU]W.Ya@bc%def&g[hij}k<m#-npq:r+twyz"

