{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module : Data.Attoparsec.Framer
Copyright : (c) 2022 Tim Emiola
Maintainer : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Provides the 'Framer' data type that combines an @Attoparsec 'A.Parser'@ with a
a few additional combinators that allow the parser to be used to process frames
of the framed byte streams commonly used in network protocol implementations.

A @'Framer'@ specifies how the processing function @'runFramer'@ should
parse a byte stream.

Minimally, a @Framer@ specifies

* a @'A.Parser'@, used to extract frames from the byte stream
* a @'FrameHandler'@ responsible for using the parsed frames
* the byte stream source, represented by a 'ByteSource'

@'runFramer'@ reads chunks from the @ByteSource@, parses these into frames and
invokes the 'FrameHandler'. Each invocation returns a 'Progression', which
indicates if processing should continue. This allows the 'FrameHandler' to
trigger termination of 'runFramer'.
-}
module Data.Attoparsec.Framer (
  -- * Framer
  ByteSource,
  Framer,
  FrameHandler,
  Progression (..),
  mkFramer,
  mkFramer',

  -- * query/update a  @'Framer'@
  setChunkSize,
  setOnBadParse,
  setOnClosed,
  setOnFrame,
  chunkSize,

  -- * Run the @Framer@
  runFramer,
  runOneFrame,

  -- * Exception handling
  -- $exceptions

  -- * exceptions
  BrokenFrame (..),
  NoMoreInput (..),
) where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow (..))
import qualified Data.Attoparsec.ByteString as A
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word32)


-- | Handles a parsed @frame@, returning a @Progression@ that indicates if further @frames@ should be parsed.
type FrameHandler m frame = frame -> m Progression


-- | A byte stream from which chunks are to be retrieved.
type ByteSource m = Word32 -> m ByteString


-- | Used by 'FrameHandler' to indicate if additional frames should be parsed.
data Progression
  = Stop
  | StopUnlessExtra
  | Continue
  deriving (Eq, Show)


-- | Uses a 'A.Parser' to parse a stream of @frames@ from a byte stream
data Framer m frame = Framer
  { framerChunkSize :: !Word32
  , frameByteSource :: !(ByteSource m)
  , framerOnFrame :: !(FrameHandler m frame)
  , framerParser :: !(A.Parser frame)
  , framerOnClosed :: !(m ())
  , framerOnBadParse :: !(Text -> m ())
  }


{- | Construct a @'Framer'@ that will handle @frames@ repeatedly until the
@FrameHandler@ returns a @'Progression'@ that stops it.
-}
mkFramer' ::
  MonadThrow m =>
  A.Parser frame ->
  FrameHandler m frame ->
  ByteSource m ->
  Framer m frame
mkFramer' framerParser framerOnFrame frameByteSource =
  Framer
    { framerParser
    , framerOnFrame
    , frameByteSource
    , framerOnBadParse = \_err -> pure ()
    , framerOnClosed = throwM NoMoreInput
    , framerChunkSize = defaultChunkSize
    }


-- | Construct a @'Framer'@ that loops continuously.
mkFramer ::
  MonadThrow m =>
  -- | parses frames from the byte stream
  A.Parser frame ->
  -- | handles parsed frames
  (frame -> m ()) ->
  -- | obtains the next chunk from the byte stream
  ByteSource m ->
  Framer m frame
mkFramer parser onFrame fetchBytes =
  let onFrameContinue x = do
        onFrame x
        pure Continue
   in mkFramer' parser onFrameContinue fetchBytes


-- | Repeatedly parse and handle frames until the configured @FrameHandler@ ends handling.
runFramer ::
  MonadThrow m =>
  Framer m frame ->
  m ()
runFramer f =
  let Framer
        { framerChunkSize = fetchSize
        , framerOnBadParse = onErr
        , frameByteSource = fetchBytes
        , framerOnFrame = onFrame
        , framerParser = parser
        , framerOnClosed = onClosed
        } = f
   in runFramer' fetchSize parser fetchBytes onFrame onErr onClosed


{- | Parse and handle a single frame.

The result is a tuple: (Maybe @unparsed@, @terminated@)

where

@unparsed@ are outstanding bytes fetched from the @ByteSource@ and
@terminated@ is @True@ if the @ByteSource@ has no further input.
-}
runOneFrame ::
  MonadThrow m =>
  -- | the unparsed bytes from an earlier invocation, if any
  Maybe ByteString ->
  -- | the 'Framer' used to parse the @frame@
  Framer m frame ->
  m ((Maybe ByteString), Bool)
runOneFrame restMb f =
  let Framer
        { framerChunkSize = fetchSize
        , framerOnBadParse = onErr
        , frameByteSource = fetchBytes
        , framerOnFrame = onFrame
        , framerParser = parser
        , framerOnClosed = onClose
        } = f
   in runOneFrame' restMb fetchSize parser fetchBytes onFrame onErr onClose


-- | The chunk size of a @Framer@.
chunkSize :: Framer m a -> Word32
chunkSize = framerChunkSize


-- | Update the chunk size of a @Framer@.
setChunkSize :: Word32 -> Framer m a -> Framer m a
setChunkSize size f = f {framerChunkSize = size}


-- | Update the parse error handler of a @Framer@.
setOnBadParse :: (Text -> m ()) -> Framer m a -> Framer m a
setOnBadParse onErr f = f {framerOnBadParse = onErr}


-- | Update the @FrameHandler@ of a @Framer@.
setOnFrame :: FrameHandler m frame -> Framer m frame -> Framer m frame
setOnFrame onFrame f = f {framerOnFrame = onFrame}


-- | Update the end-of-input handler of a @Framer@.
setOnClosed :: (m ()) -> Framer m a -> Framer m a
setOnClosed onClose f = f {framerOnClosed = onClose}


runFramer' ::
  MonadThrow m =>
  Word32 ->
  A.Parser a ->
  (Word32 -> m ByteString) ->
  (a -> m Progression) ->
  (Text -> m ()) ->
  m () ->
  m ()
runFramer' fetchSize parser fetchBytes handleFrame onErr onClosed = do
  let loop x = do
        (next, closed) <- runOneFrame' x fetchSize parser fetchBytes handleFrame onErr onClosed
        if not closed then loop next else pure ()
  loop Nothing


runOneFrame' ::
  MonadThrow m =>
  Maybe ByteString ->
  Word32 ->
  A.Parser a ->
  (Word32 -> m ByteString) ->
  (a -> m Progression) ->
  (Text -> m ()) ->
  m () ->
  m ((Maybe ByteString), Bool)
runOneFrame' restMb fetchSize parser fetchBytes handleFrame onErr onClose = do
  let pullChunk = fetchBytes fetchSize
      initial = fromMaybe BS.empty restMb
      onParse (A.Fail _ ctxs reason) = do
        let errMessage = parsingFailed ctxs reason
        if reason == closedReason
          then -- WANTED: a typed way of detecting this condition, i.e,
          -- it is possible not to rely on a  specific error message ?
          do
            onClose
            pure (Nothing, True)
          else do
            onErr errMessage
            throwM $ BrokenFrame reason
      onParse (A.Done i r) = do
        let extraMb = if BS.null i then Nothing else Just i
        doMore <- handleFrame r
        case (doMore, extraMb) of
          (Stop, _) -> pure (extraMb, True)
          (StopUnlessExtra, Nothing) -> pure (extraMb, True)
          (_, _) -> pure (extraMb, False)
      onParse (A.Partial continue) = pullChunk >>= onParse . continue
  A.parseWith pullChunk parser initial >>= onParse


parsingFailed :: [String] -> String -> Text
parsingFailed context reason =
  let contexts = Text.intercalate "-" (Text.pack <$> context)
      cause = if null reason then Text.empty else ":" <> Text.pack reason
   in "bad parse:" <> contexts <> cause


{- $exceptions

On failures, @'runFramer'@ throws @'Exception's@ using @'MonadThrow'@ rather
than using an @Either@ or @MonadError@

This is because its intended use is for parsing framed protocol byte streams;
where parsing or connection errors are typically not recoverable. In
haskell non-recoverable failures are better modelled using @Exceptions@.

Although it throws 'NoMoreInput' or 'BrokenFrame' when appropriate, it provides
hooks to override these when constructing a 'Framer'.

By use of 'setOnClosed' and 'setOnBadParse', the caller of @runFramer@ can
completely override the exception type that is raised when @runFramer@ encounters
any failure.
-}


{- | Thrown by 'runFramer' or 'runOneFrame' if parsing fails and there is no
 handler installed using 'setOnBadParse', or it does not throw an exception.
-}
newtype BrokenFrame = BrokenFrame String
  deriving (Eq, Show)


instance Exception BrokenFrame


{- | Thrown by 'runFramer' or 'runOneFrame' when no further input is available and
 no end of input handler is set using 'setOnClosed'.
-}
data NoMoreInput = NoMoreInput
  deriving (Eq, Show)


instance Exception NoMoreInput


closedReason :: String
closedReason = "not enough input"


defaultChunkSize :: Word32
defaultChunkSize = 2048
