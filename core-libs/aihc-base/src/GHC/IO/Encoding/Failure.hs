-- | How a text codec reacts to a byte sequence or character it cannot
-- translate. Handles carry the name of their encoding only, so the mode
-- reaches the runtime as the suffix of that name.
module GHC.IO.Encoding.Failure
  ( CodingFailureMode (..),
    codingFailureModeSuffix,
    isSurrogate,
    recoverDecode,
    recoverEncode,
  )
where

import Data.Bool (Bool (..), (&&))
import Data.Maybe (Maybe (..))
import GHC.Base (Char, Monad (..), String, ord, unsafeChr)
import GHC.IO (IO, throwIO)
import GHC.IO.Buffer (Buffer (..), CharBuffer, readCharBuf, readWord8Buf, writeCharBuf, writeWord8Buf)
import GHC.Internal.Classes (Eq (..), Ord (..))
import GHC.Internal.IO.Types (IOErrorType (..), IOException (..))
import GHC.Num (Num (..))
import GHC.Real (fromIntegral)
import GHC.Show (Show (..))
import GHC.Word (Word8)

data CodingFailureMode
  = -- | Throw an exception.
    ErrorOnCodingFailure
  | -- | Drop the offending input.
    IgnoreCodingFailure
  | -- | Replace the offending input with a substitute character.
    TransliterateCodingFailure
  | -- | Map undecodable bytes to the surrogate code points that GHC uses
    -- to carry them back out unchanged.
    RoundtripFailure
  deriving (Show)

-- | The suffix that names an encoding with this failure mode.
codingFailureModeSuffix :: CodingFailureMode -> String
codingFailureModeSuffix ErrorOnCodingFailure = ""
codingFailureModeSuffix IgnoreCodingFailure = "//IGNORE"
codingFailureModeSuffix TransliterateCodingFailure = "//TRANSLIT"
codingFailureModeSuffix RoundtripFailure = "//ROUNDTRIP"

-- | The code points reserved for UTF-16 surrogate pairs. 'RoundtripFailure'
-- parks undecodable bytes in the low half of that range.
isSurrogate :: Char -> Bool
isSurrogate character = '\xD800' <= character && character <= '\xDFFF'

-- | The character a transliterating codec emits for input it cannot
-- decode.
unrepresentableChar :: Char
unrepresentableChar = '\xFFFD'

decodingError :: IO a
decodingError = throwIO (codingError "recoverDecode" "invalid byte sequence")

encodingError :: IO a
encodingError = throwIO (codingError "recoverEncode" "character is not in the target encoding")

codingError :: String -> String -> IOException
codingError location description =
  IOError
    { ioe_handle = Nothing,
      ioe_type = InvalidArgument,
      ioe_location = location,
      ioe_description = description,
      ioe_errno = Nothing,
      ioe_filename = Nothing
    }

-- | Step a decoder over one input byte it could not translate.
recoverDecode :: CodingFailureMode -> Buffer Word8 -> CharBuffer -> IO (Buffer Word8, CharBuffer)
recoverDecode mode input@Buffer {bufRaw = iraw, bufL = ir} output@Buffer {bufRaw = oraw, bufR = ow} =
  case mode of
    ErrorOnCodingFailure -> decodingError
    IgnoreCodingFailure -> return (input {bufL = ir + 1}, output)
    TransliterateCodingFailure -> do
      ow' <- writeCharBuf oraw ow unrepresentableChar
      return (input {bufL = ir + 1}, output {bufR = ow'})
    RoundtripFailure -> do
      byte <- readWord8Buf iraw ir
      ow' <- writeCharBuf oraw ow (unsafeChr (0xDC00 + fromIntegral byte))
      return (input {bufL = ir + 1}, output {bufR = ow'})

-- | Step an encoder over one character it could not translate.
recoverEncode :: CodingFailureMode -> CharBuffer -> Buffer Word8 -> IO (CharBuffer, Buffer Word8)
recoverEncode mode input@Buffer {bufRaw = iraw, bufL = ir} output@Buffer {bufRaw = oraw, bufR = ow} = do
  (character, ir') <- readCharBuf iraw ir
  case mode of
    IgnoreCodingFailure -> return (input {bufL = ir'}, output)
    TransliterateCodingFailure ->
      case character == '?' of
        True -> return (input {bufL = ir'}, output)
        False -> do
          -- Replace the character in place and let the codec run again, so
          -- that the substitute goes through the encoding proper.
          _ <- writeCharBuf iraw ir '?'
          return (input, output)
    RoundtripFailure ->
      let codePoint = ord character
       in case 0xDC80 <= codePoint && codePoint <= 0xDCFF of
            True -> do
              writeWord8Buf oraw ow (fromIntegral (codePoint - 0xDC00))
              return (input {bufL = ir'}, output {bufR = ow + 1})
            False -> encodingError
    ErrorOnCodingFailure -> encodingError
