{-# LANGUAGE OverloadedStrings #-}

-- | IO effects
module Box.IO
  ( fromStdin,
    toStdout,
    stdBox,
    fromStdinN,
    toStdoutN,
    readStdin,
    showStdout,
    refCommitter,
    refEmitter,
    handleE,
    handleC,
    fileE,
    fileC,
    fileEText,
    fileEBS,
    fileCText,
    fileCBS,
    toLineBox,
    fromLineBox,
    logConsoleC,
    logConsoleE,
    pauser,
    changer,
    quit,
    restart,
  )
where

import Box.Box
import Box.Codensity
import Box.Committer
import Box.Connectors
import Box.Emitter
import Control.Concurrent.Async
import Control.Exception
import Control.Monad.State.Lazy
import Data.Bool
import Data.ByteString.Char8 as Char8
import Data.Foldable
import Data.Function
import Data.Functor.Contravariant
import Data.IORef
import Data.Sequence qualified as Seq
import Data.String
import Data.Text as Text hiding (null, show)
import Data.Text.Encoding
import Data.Text.IO as Text
import System.IO as IO
import Prelude

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Prelude
-- >>> import Box
-- >>> import Data.Bool
-- >>> import Data.Text (Text, pack)
-- >>> import Data.Functor.Contravariant

-- * console

-- | Emit text from stdin
--
-- @
-- λ> emit fromStdin
-- hello
-- Just "hello"
-- @
fromStdin :: Emitter IO Text
fromStdin = Emitter $ Just <$> Text.getLine

-- | Commit to stdout
--
-- >>> commit toStdout ("I'm committed!" :: Text)
-- I'm committed!
-- True
toStdout :: Committer IO Text
toStdout = Committer $ \a -> Text.putStrLn a >> pure True

-- | A 'Box' to and from std, with an escape phrase.
stdBox :: Text -> Box IO Text Text
stdBox q = Box toStdout (takeUntilE (== q) fromStdin)

-- | Finite console emitter
--
-- @
-- λ> toListM /<$|/> fromStdinN 2
-- hello
-- hello again
-- ["hello","hello again"]
-- @
fromStdinN :: Int -> CoEmitter IO Text
fromStdinN n = source n Text.getLine

-- | Finite console committer
--
-- >>> glue <$> contramap (pack . show) <$> (toStdoutN 2) <*|> qList [1..3]
-- 1
-- 2
toStdoutN :: Int -> CoCommitter IO Text
toStdoutN n = sink n Text.putStrLn

-- | Read from console, throwing away read errors
--
-- > λ> glueN 2 showStdout (readStdin :: Emitter IO Int)
-- > 1
-- > 1
-- > hippo
-- > 2
-- > 2
readStdin :: (Read a) => Emitter IO a
readStdin = witherE (pure . either (const Nothing) Just) . readE $ fromStdin

-- | Show to stdout
--
-- >>> glue showStdout <$|> qList [1..3]
-- 1
-- 2
-- 3
showStdout :: (Show a) => Committer IO a
showStdout = contramap (Text.pack . show) toStdout

-- | Emits lines of Text from a handle.
handleE :: (IsString a, Eq a) => (Handle -> IO a) -> Handle -> Emitter IO a
handleE action h = Emitter $ do
  l :: (Either IOException a) <- try (action h)
  pure $ case l of
    Left _ -> Nothing
    Right a -> bool (Just a) Nothing (a == "")

-- | Commit lines of Text to a handle.
handleC :: (Handle -> a -> IO ()) -> Handle -> Committer IO a
handleC action h = Committer $ \a -> do
  action h a
  pure True

-- | Emit from a file.
fileE :: FilePath -> BufferMode -> IOMode -> (Handle -> Emitter IO a) -> CoEmitter IO a
fileE fp b m action = Codensity $ \eio ->
  withFile
    fp
    m
    ( \h -> do
        hSetBuffering h b
        eio (action h)
    )

-- | Emit lines of Text from a file.
fileEText :: FilePath -> BufferMode -> CoEmitter IO Text
fileEText fp b = fileE fp b ReadMode (handleE Text.hGetLine)

-- | Emit lines of ByteString from a file.
fileEBS :: FilePath -> BufferMode -> CoEmitter IO ByteString
fileEBS fp b = fileE fp b ReadMode (handleE Char8.hGetLine)

-- | Commit to a file.
fileC :: FilePath -> IOMode -> BufferMode -> (Handle -> Committer IO a) -> CoCommitter IO a
fileC fp m b action = Codensity $ \cio ->
  withFile
    fp
    m
    ( \h -> do
        hSetBuffering h b
        cio (action h)
    )

-- | Commit Text to a file, as a line.
fileCText :: FilePath -> BufferMode -> IOMode -> CoCommitter IO Text
fileCText fp m b = fileC fp b m (handleC Text.hPutStrLn)

-- | Commit ByteString to a file, as a line.
fileCBS :: FilePath -> BufferMode -> IOMode -> CoCommitter IO ByteString
fileCBS fp m b = fileC fp b m (handleC Char8.hPutStrLn)

-- | Convert a 'Box' from ByteString to lines of Text.
toLineBox :: Text -> Box IO ByteString ByteString -> CoBox IO Text Text
toLineBox end (Box c e) = Box (contramap (encodeUtf8 . (<> end)) c) <$> evalEmitter [] (unlistE $ fmap (Text.lines . decodeUtf8) e)

-- | Convert a 'Box' from lines of Text to ByteStrings.
fromLineBox :: Text -> Box IO Text Text -> Box IO ByteString ByteString
fromLineBox end (Box c e) = Box (contramap (Text.lines . decodeUtf8) (listC c)) (fmap (encodeUtf8 . (<> end)) e)

-- | Commit to an IORef
--
-- >>> (c1,l1) <- refCommitter :: IO (Committer IO Int, IO [Int])
-- >>> glue c1 <$|> qList [1..3]
-- >>> l1
-- [1,2,3]
refCommitter :: IO (Committer IO a, IO [a])
refCommitter = do
  ref <- newIORef Seq.empty
  let c = Committer $ \a -> do
        modifyIORef ref (Seq.:|> a)
        pure True
  let res = toList <$> readIORef ref
  pure (c, res)

-- | Emit from a list IORef
--
-- >>> e <- refEmitter [1..3]
-- >>> toListM e
-- [1,2,3]
refEmitter :: [a] -> IO (Emitter IO a)
refEmitter xs = do
  ref <- newIORef xs
  let e = Emitter $ do
        as <- readIORef ref
        case as of
          [] -> pure Nothing
          (x : xs') -> do
            writeIORef ref xs'
            pure $ Just x
  pure e

-- | simple console logger for rough testing
logConsoleE :: (Show a) => String -> Emitter IO a -> Emitter IO a
logConsoleE label e = Emitter $ do
  a <- emit e
  Prelude.putStrLn (label <> show a)
  pure a

-- | simple console logger for rough testing
logConsoleC :: (Show a) => String -> Committer IO a -> Committer IO a
logConsoleC label c = Committer $ \a -> do
  Prelude.putStrLn (label <> show a)
  commit c a

-- | Pause an emitter based on a Bool emitter
pauser :: Emitter IO Bool -> Emitter IO a -> Emitter IO a
pauser b e = Emitter $ fix $ \rec -> do
  b' <- emit b
  case b' of
    Nothing -> pure Nothing
    Just False -> emit e
    Just True -> rec

-- | Create an emitter that indicates when another emitter has changed.
changer :: (Eq a) => a -> Emitter IO a -> CoEmitter IO Bool
changer a0 e = evalEmitter a0 $ Emitter $ do
  r <- lift $ emit e
  case r of
    Nothing -> pure Nothing
    Just r' -> do
      r'' <- get
      put r'
      pure (Just (r' == r''))

-- | quit a process based on a Bool emitter
--
-- > quit <$> speedEffect (pure 2) <$> (resetGap 5) <*|> pure io
-- > 0
-- > 1
-- > 2
-- > 3
-- > 4
-- > Left True
quit :: Emitter IO Bool -> IO a -> IO (Either Bool a)
quit flag io = race (checkE flag) io

checkE :: Emitter IO Bool -> IO Bool
checkE e = fix $ \rec -> do
  a <- emit e
  -- atomically $ check (a == Just False)
  case a of
    Nothing -> pure False
    Just True -> pure True
    Just False -> rec

-- | restart a process if flagged by a Bool emitter
restart :: Emitter IO Bool -> IO a -> IO (Either Bool a)
restart flag io = fix $ \rec -> do
  res <- quit flag io
  case res of
    Left True -> rec
    Left False -> pure (Left False)
    Right r -> pure (Right r)
