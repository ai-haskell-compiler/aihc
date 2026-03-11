{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2006-2007 Duncan Coutts
-- License     :  BSD-style
--
-- Maintainer  :  duncan@haskell.org
-- Portability :  portable (H98 + FFI)
--
-- String encoding conversion
--
-----------------------------------------------------------------------------
module Codec.Text.IConv (

  -- | This module provides pure functions for converting the string encoding
  -- of strings represented by lazy 'ByteString's. This makes it easy to use
  -- either in memory or with disk or network IO.
  --
  -- For example, a simple Latin1 to UTF-8 conversion program is just:
  --
  -- > import Codec.Text.IConv as IConv
  -- > import Data.ByteString.Lazy as ByteString
  -- >
  -- > main = ByteString.interact (convert "LATIN1" "UTF-8")
  --
  -- Or you could lazily read in and convert a UTF-8 file to UTF-32 using:
  --
  -- > content <- fmap (IConv.convert "UTF-8" "UTF-32") (readFile file)
  --
  -- This module uses the POSIX @iconv()@ library function. The primary
  -- advantage of using iconv is that it is widely available, most systems
  -- have a wide range of supported string encodings and the conversion speed
  -- it typically good. The iconv library is available on all unix systems
  -- (since it is required by the POSIX.1 standard) and GNU libiconv is
  -- available as a standalone library for other systems, including Windows.

  -- * Simple conversion API
  convert,
  EncodingName,

  -- * Variant that is lax about conversion errors
  convertFuzzy,
  Fuzzy(..),

  -- * Variants that are pedantic about conversion errors
  convertStrictly,
  convertLazily,
  ConversionError(..),
  reportConversionError,
  Span(..),

  ) where

import Prelude hiding (length, span)

import Control.Exception (assert)
import qualified Control.Exception as Exception
import Foreign.C.Error as C.Error (Errno, errnoToIOError)

import qualified Data.ByteString.Lazy as L (ByteString, toChunks, fromChunks)
import qualified Data.ByteString.Lazy.Internal as L (defaultChunkSize)
import qualified Data.ByteString as S

import qualified Codec.Text.IConv.Internal as IConv
import Codec.Text.IConv.Internal (IConv)


-- | A string encoding name, eg @\"UTF-8\"@ or @\"LATIN1\"@.
--
-- The range of string encodings available is determined by the capabilities
-- of the underlying iconv implementation.
--
-- When using the GNU C or libiconv libraries, the permitted values are listed
-- by the @iconv --list@ command, and all combinations of the listed values
-- are supported.
--
type EncodingName = String

-- | Output spans from encoding conversion. When nothing goes wrong we
-- expect just a bunch of 'Span's. If there are conversion errors we get other
-- span types.
--
data Span =

    -- | An ordinary output span in the target encoding
    Span !S.ByteString

    -- | An error in the conversion process. If this occurs it will be the
    -- last span.
  | ConversionError !ConversionError

data ConversionError =
    -- | The conversion from the input to output string encoding is not
    -- supported by the underlying iconv implementation. This is usually
    -- because a named encoding is not recognised or support for it
    -- was not enabled on this system.
    --
    -- The POSIX standard does not guarantee that all possible combinations
    -- of recognised string encoding are supported, however most common
    -- implementations do support all possible combinations.
    --
    UnsuportedConversion EncodingName EncodingName

    -- | This covers two possible conversion errors:
    --
    -- * There is a byte sequence in the input that is not valid in the input
    -- encoding.
    --
    -- * There is a valid character in the input that has no corresponding
    -- character in the output encoding.
    --
    -- Unfortunately iconv does not let us distinguish these two cases. In
    -- either case, the Int parameter gives the byte offset in the input of
    -- the unrecognised bytes or unconvertable character.
    --
  | InvalidChar Int

    -- | This error covers the case where the end of the input has trailing
    -- bytes that are the initial bytes of a valid character in the input
    -- encoding. In other words, it looks like the input ended in the middle of
    -- a multi-byte character. This would often be an indication that the input
    -- was somehow truncated. Again, the Int parameter is the byte offset in
    -- the input where the incomplete character starts.
    --
  | IncompleteChar Int

    -- | An unexpected iconv error. The iconv spec lists a number of possible
    -- expected errors but does not guarantee that there might not be other
    -- errors.
    --
    -- This error can occur either immediately, which might indicate that the
    -- iconv installation is messed up somehow, or it could occur later which
    -- might indicate resource exhaustion or some other internal iconv error.
    --
    -- Use 'Foreign.C.Error.errnoToIOError' to get slightly more information
    -- on what the error could possibly be.
    --
  | UnexpectedError C.Error.Errno

reportConversionError :: ConversionError -> IOError
reportConversionError conversionError = case conversionError of
  UnsuportedConversion fromEncoding toEncoding
                          -> err $ "cannot convert from string encoding "
                             ++ show fromEncoding ++ " to string encoding "
                             ++ show toEncoding
  InvalidChar    inputPos -> err $ "invalid input sequence at byte offset "
                               ++ show inputPos
  IncompleteChar inputPos -> err $ "incomplete input sequence at byte offset "
                               ++ show inputPos
  UnexpectedError errno   -> C.Error.errnoToIOError
                               "Codec.Text.IConv: unexpected error" errno
                               Nothing Nothing
  where err msg = userError $ "Codec.Text.IConv: " ++ msg

{-# NOINLINE convert #-}
-- | Convert text from one named string encoding to another.
--
-- * The conversion is done lazily.
--
-- * An exception is thrown if conversion between the two encodings is not
-- supported.
--
-- * An exception is thrown if there are any encoding conversion errors.
--
convert :: EncodingName      -- ^ Name of input string encoding
        -> EncodingName      -- ^ Name of output string encoding
        -> L.ByteString      -- ^ Input text
        -> L.ByteString      -- ^ Output text
convert fromEncoding toEncoding =

    -- lazily convert the list of spans into an ordinary lazy ByteString:
    L.fromChunks
  . foldr span []
  . convertLazily fromEncoding toEncoding

  where
    span (Span c)            cs = c : cs
    span (ConversionError e) _  =
#if MIN_VERSION_base(4,0,0)
      Exception.throw (reportConversionError e)
#else
      Exception.throw (Exception.IOException (reportConversionError e))
#endif


data Fuzzy = Transliterate | Discard

-- | Convert text ignoring encoding conversion problems.
--
-- If invalid byte sequences are found in the input they are ignored and
-- conversion continues if possible. This is not always possible especially
-- with stateful encodings. No placeholder character is inserted into the
-- output so there will be no indication that invalid byte sequences were
-- encountered.
--
-- If there are characters in the input that have no direct corresponding
-- character in the output encoding then they are dealt in one of two ways,
-- depending on the 'Fuzzy' argument. We can try and 'Transliterate' them into
-- the nearest corresponding character(s) or use a replacement character
-- (typically @\'?\'@ or the Unicode replacement character). Alternatively they
-- can simply be 'Discard'ed.
--
-- In either case, no exceptions will occur. In the case of unrecoverable
-- errors, the output will simply be truncated. This includes the case of
-- unrecognised or unsupported encoding names; the output will be empty.
--
-- * This function only works with the GNU iconv implementation which provides
-- this feature beyond what is required by the iconv specification.
--
convertFuzzy :: Fuzzy           -- ^ Whether to try and transliterate or
                                -- discard characters with no direct conversion
             -> EncodingName    -- ^ Name of input string encoding
             -> EncodingName    -- ^ Name of output string encoding
             -> L.ByteString    -- ^ Input text
             -> L.ByteString    -- ^ Output text
convertFuzzy fuzzy fromEncoding toEncoding =

    -- lazily convert the list of spans into an ordinary lazy ByteString:
    L.fromChunks
  . foldr span []
  . convertInternal IgnoreInvalidChar fromEncoding (toEncoding ++ mode)
  where
    mode = case fuzzy of
             Transliterate -> "//IGNORE,TRANSLIT"
             Discard       -> "//IGNORE"
    span (Span c)            cs = c : cs
    span (ConversionError _) cs = cs

{-# NOINLINE convertStrictly #-}
-- | This variant does the conversion all in one go, so it is able to report
-- any conversion errors up front. It exposes all the possible error conditions
-- and never throws exceptions
--
-- The disadvantage is that no output can be produced before the whole input
-- is consumed. This might be problematic for very large inputs.
--
convertStrictly :: EncodingName      -- ^ Name of input string encoding
                -> EncodingName      -- ^ Name of output string encoding
                -> L.ByteString      -- ^ Input text
                -> Either L.ByteString
                          ConversionError -- ^ Output text or conversion error
convertStrictly fromEncoding toEncoding =
    -- strictly convert the list of spans into an ordinary lazy ByteString
    -- or an error
    strictify []
  . convertLazily fromEncoding toEncoding

  where
    strictify :: [S.ByteString] -> [Span] -> Either L.ByteString ConversionError
    strictify cs []                    = Left (L.fromChunks (reverse cs))
    strictify cs (Span c : ss)         = strictify (c:cs) ss
    strictify _  (ConversionError e:_) = Right e


{-# NOINLINE convertLazily #-}
-- | This version provides a more complete but less convenient conversion
-- interface. It exposes all the possible error conditions and never throws
-- exceptions.
--
-- The conversion is still lazy. It returns a list of spans, where a span may
-- be an ordinary span of output text or a conversion error. This somewhat
-- complex interface allows both for lazy conversion and for precise reporting
-- of conversion problems. The other functions 'convert' and 'convertStrictly'
-- are actually simple wrappers on this function.
--
convertLazily :: EncodingName  -- ^ Name of input string encoding
              -> EncodingName  -- ^ Name of output string encoding
              -> L.ByteString  -- ^ Input text
              -> [Span]        -- ^ Output text spans
convertLazily = convertInternal StopOnInvalidChar


data InvalidCharBehaviour = StopOnInvalidChar | IgnoreInvalidChar

convertInternal :: InvalidCharBehaviour
                -> EncodingName -> EncodingName
                -> L.ByteString -> [Span]
convertInternal ignore fromEncoding toEncoding input =
  IConv.run fromEncoding toEncoding $ \status -> case status of
    IConv.InitOk -> do IConv.newOutputBuffer outChunkSize
                       fillInputBuffer ignore (L.toChunks input)

    IConv.UnsupportedConversion     -> failConversion (UnsuportedConversion
                                                         fromEncoding
                                                         toEncoding)
    IConv.UnexpectedInitError errno -> failConversion (UnexpectedError errno)


fillInputBuffer :: InvalidCharBehaviour -> [S.ByteString] -> IConv [Span]
fillInputBuffer ignore (inChunk : inChunks) = do
  IConv.pushInputBuffer inChunk
  drainBuffers ignore inChunks

fillInputBuffer _ignore [] = do
  outputBufferBytesAvailable <- IConv.outputBufferBytesAvailable
  IConv.finalise
  if outputBufferBytesAvailable > 0
    then do outChunk <- IConv.popOutputBuffer
            return [Span outChunk]
    else return []


drainBuffers :: InvalidCharBehaviour -> [S.ByteString] -> IConv [Span]
drainBuffers ignore inChunks = do
  inputBufferEmpty_ <- IConv.inputBufferEmpty
  outputBufferFull <- IConv.outputBufferFull
  assert (not outputBufferFull && not inputBufferEmpty_) $ return ()
  -- this invariant guarantees we can always make forward progress

  status <- IConv.iconv

  case status of
    IConv.InputEmpty -> do
      inputBufferEmpty <- IConv.inputBufferEmpty
      assert inputBufferEmpty $ fillInputBuffer ignore inChunks

    IConv.OutputFull -> do
      outChunk <- IConv.popOutputBuffer
      outChunks <- IConv.unsafeInterleave $ do
        IConv.newOutputBuffer outChunkSize
        drainBuffers ignore inChunks
      return (Span outChunk : outChunks)

    IConv.InvalidChar -> invalidChar ignore inChunks

    IConv.IncompleteChar -> fixupBoundary ignore inChunks

    IConv.UnexpectedError errno -> failConversion (UnexpectedError errno)

-- | The posix iconv api looks like it's designed specifically for streaming
-- and it is, except for one really really annoying corner case...
--
-- Suppose you're converting a stream, say by reading a file in 4k chunks. This
-- would seem to be the canonical use case for iconv, reading and converting an
-- input file. However suppose the 4k read chunk happens to split a multi-byte
-- character. Then iconv will stop just before that char and tell us that its
-- an incomplete char. So far so good. Now what we'd like to do is have iconv
-- remember those last few bytes in its conversion state so we can carry on
-- with the next 4k block. Sadly it does not. It requires us to fix things up
-- so that it can carry on with the next block starting with a complete multi-
-- byte character. Do do that we have to somehow copy those few trailing bytes
-- to the beginning of the next block. That's perhaps not too bad in an
-- imperitive context using a mutable input buffer - we'd just copy the few
-- trailing bytes to the beginning of the buffer and do a short read (ie 4k-n
-- the number of trailing bytes). That's not terribly nice since it means the
-- OS has to do IO on non-page aligned buffers which tends to be slower. It's
-- worse for us though since we're not using a mutable input buffer, we're
-- using a lazy bytestring which is a sequence of immutable buffers.
--
-- So we have to do more cunning things. We could just prepend the trailing
-- bytes to the next block, but that would mean alocating and copying the whole
-- next block just to prepend a couple bytes. This probably happens quite
-- frequently so would be pretty slow. So we have to be even more cunning.
--
-- The solution is to create a very small buffer to cover the few bytes making
-- up the character spanning the block boundary. So we copy the trailing bytes
-- plus a few from the beginning of the next block. Then we run iconv again on
-- that small buffer. How many bytes from the next block to copy is a slightly
-- tricky issue. If we copy too few there's no guarantee that we have enough to
-- give a complete character. We opt for a maximum size of 16, 'tmpChunkSize'
-- on the theory that no encoding in existance uses that many bytes to encode a
-- single character, so it ought to be enough. Yeah, it's a tad dodgey.
--
-- Having papered over the block boundary, we still have to cross the boundary
-- of this small buffer. It looks like we've still got the same problem,
-- however this time we should have crossed over into bytes that are wholly
-- part of the large following block so we can abandon our small temp buffer
-- an continue with the following block, with a slight offset for the few bytes
-- taken up by the chars that fit into the small buffer.
--
-- So yeah, pretty complex. Check out the proof below of the tricky case.
--
fixupBoundary :: InvalidCharBehaviour -> [S.ByteString] -> IConv [Span]
fixupBoundary _ignore [] = do
  inputPos <- IConv.inputPosition
  failConversion (IncompleteChar inputPos)
fixupBoundary ignore inChunks@(inChunk : inChunks') = do
  inSize <- IConv.inputBufferSize
  assert (inSize < tmpChunkSize) $ return ()
  let extraBytes = tmpChunkSize - inSize

  if S.length inChunk <= extraBytes
    then do
      IConv.replaceInputBuffer (`S.append` inChunk)
      drainBuffers ignore inChunks'
    else do
      IConv.replaceInputBuffer (`S.append` S.take extraBytes inChunk)

      before <- IConv.inputBufferSize
      assert (before == tmpChunkSize) $ return ()

      status <- IConv.iconv
      after <- IConv.inputBufferSize
      let consumed = before - after

      case status of
        IConv.InputEmpty ->
          assert (consumed == tmpChunkSize) $
          fillInputBuffer ignore (S.drop extraBytes inChunk : inChunks')

        IConv.OutputFull -> do
          outChunk <- IConv.popOutputBuffer
          outChunks <- IConv.unsafeInterleave $ do
            IConv.newOutputBuffer outChunkSize
            drainBuffers ignore inChunks
          return (Span outChunk : outChunks)

        IConv.InvalidChar -> invalidChar ignore inChunks

        IConv.IncompleteChar ->
          assert (inSize < consumed && consumed < tmpChunkSize) $
          --    inSize < consumed < tmpChunkSize
          -- => { subtract inSize from each side }
          --    0 < consumed - inSize < tmpChunkSize - inSize
          -- => { by definition that extraBytes = tmpChunkSize - inSize }
          --    0 < consumed - inSize < extraBytes
          -- => { since we're in the False case of the if, we know:
          --        not (S.length inChunk <= extraBytes)
          --      =      S.length inChunk > extraBytes
          --      =      extraBytes < S.length inChunk }
          --    0 < consumed - inSize < extraBytes < S.length inChunk
          --
          -- And we're done! We know it's safe to drop (consumed - inSize) from
          -- inChunk since it's more than 0 and less than the inChunk size, so
          -- we're not being left with an empty chunk (which is not allowed).

          drainBuffers ignore (S.drop (consumed - inSize) inChunk : inChunks')

        IConv.UnexpectedError errno -> failConversion (UnexpectedError errno)


invalidChar :: InvalidCharBehaviour -> [S.ByteString] -> IConv [Span]
invalidChar StopOnInvalidChar _ = do
  inputPos <- IConv.inputPosition
  failConversion (InvalidChar inputPos)

invalidChar IgnoreInvalidChar inChunks = do
  inputPos  <- IConv.inputPosition
  let invalidCharError = ConversionError (InvalidChar inputPos)
  outputBufferBytesAvailable <- IConv.outputBufferBytesAvailable
  if outputBufferBytesAvailable > 0
    then do outChunk  <- IConv.popOutputBuffer
            outChunks <- IConv.unsafeInterleave $ do
              IConv.newOutputBuffer outChunkSize
              inputBufferEmpty <- IConv.inputBufferEmpty
              if inputBufferEmpty
                then fillInputBuffer IgnoreInvalidChar inChunks
                else drainBuffers    IgnoreInvalidChar inChunks
            return (Span outChunk : invalidCharError : outChunks)
    else do outChunks <- IConv.unsafeInterleave $ do
              IConv.newOutputBuffer outChunkSize
              inputBufferEmpty <- IConv.inputBufferEmpty
              if inputBufferEmpty
                then fillInputBuffer IgnoreInvalidChar inChunks
                else drainBuffers    IgnoreInvalidChar inChunks
            return (invalidCharError : outChunks)

failConversion :: ConversionError -> IConv [Span]
failConversion err = do
  outputBufferBytesAvailable <- IConv.outputBufferBytesAvailable
  IConv.finalise
  if outputBufferBytesAvailable > 0
    then do outChunk <- IConv.popOutputBuffer
            return [Span outChunk, ConversionError err]
    else    return [               ConversionError err]

outChunkSize :: Int
outChunkSize = L.defaultChunkSize

tmpChunkSize :: Int
tmpChunkSize = 16
