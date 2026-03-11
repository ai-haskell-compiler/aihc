module Attoparsec.Data.Implicit where

import qualified Attoparsec.Data.Explicit as A
import Attoparsec.Data.Prelude
import qualified Data.Attoparsec.Text as B

-- |
-- Provides the default lenient parser for a type.
--
-- By convention, the parser should not check for the end of input.
class LenientParser a where
  lenientParser :: B.Parser a

-- | Consumes all the remaining input.
instance LenientParser Text where lenientParser = A.text

-- | Consumes all the remaining input, encoding it using UTF8.
instance LenientParser ByteString where lenientParser = A.utf8Bytes

instance LenientParser Char where lenientParser = A.char

instance LenientParser Bool where lenientParser = A.bool

instance LenientParser Integer where lenientParser = A.signedIntegral

instance LenientParser Int where lenientParser = A.signedIntegral

instance LenientParser Int8 where lenientParser = A.signedIntegral

instance LenientParser Int16 where lenientParser = A.signedIntegral

instance LenientParser Int32 where lenientParser = A.signedIntegral

instance LenientParser Int64 where lenientParser = A.signedIntegral

instance LenientParser Word where lenientParser = A.unsignedIntegral

instance LenientParser Word8 where lenientParser = A.unsignedIntegral

instance LenientParser Word16 where lenientParser = A.unsignedIntegral

instance LenientParser Word32 where lenientParser = A.unsignedIntegral

instance LenientParser Word64 where lenientParser = A.unsignedIntegral

instance LenientParser Double where lenientParser = A.double

instance LenientParser Scientific where lenientParser = A.scientific

instance LenientParser TimeOfDay where lenientParser = A.timeOfDayInISO8601

instance LenientParser Day where lenientParser = A.dayInISO8601

instance LenientParser TimeZone where lenientParser = A.timeZoneInISO8601

instance LenientParser UTCTime where lenientParser = A.utcTimeInISO8601

instance LenientParser DiffTime where lenientParser = A.diffTime

instance LenientParser NominalDiffTime where lenientParser = A.nominalDiffTime

instance LenientParser String where lenientParser = A.string

instance LenientParser UUID where lenientParser = A.uuid
