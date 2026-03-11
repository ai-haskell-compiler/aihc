module Sound.MIDI.Parser.File
   (T(..), runFile, runHandle, runIncompleteFile,
    PossiblyIncomplete, UserMessage, ) where

import qualified Sound.MIDI.Parser.Class as Parser
import Sound.MIDI.Parser.Class (UserMessage, PossiblyIncomplete, )

import Control.Monad.Trans.Reader (ReaderT(runReaderT), ask, )
import Control.Monad.Trans.Class (lift, )
import Control.Monad (liftM, ap, )
import Control.Applicative (Applicative, pure, (<*>), )

import qualified System.IO.Error as IOE
import qualified Control.Exception as Exc
import qualified Control.Monad.Exception.Asynchronous as Async
import qualified Control.Monad.Exception.Synchronous  as Sync

import qualified System.IO as IO
import Data.Char (ord)

import qualified Numeric.NonNegative.Wrapper as NonNeg



newtype T a = Cons {decons :: ReaderT IO.Handle IO a}


runFile :: Parser.Fragile T a -> FilePath -> IO a
runFile p name =
   Exc.bracket
      (IO.openBinaryFile name IO.ReadMode)
      IO.hClose
      (runHandle p)

runHandle :: Parser.Fragile T a -> IO.Handle -> IO a
runHandle p h =
   do exc <- runReaderT (decons (Sync.tryT p)) h
      Sync.resolve (IOE.ioError . IOE.userError) (fmap return exc)



{- |
Since in case of an incomplete file read,
we cannot know where the current file position is,
we omit the @runIncompleteHandle@ variant.
-}
runIncompleteFile :: Parser.Partial (Parser.Fragile T) a -> FilePath -> IO a
runIncompleteFile p name =
   Exc.bracket
      (IO.openBinaryFile name IO.ReadMode)
      IO.hClose
      (\h ->
          do (Async.Exceptional me a) <- runHandle p h
             maybe (return ())
                 (\msg -> putStrLn $ "could not parse MIDI file completely: " ++ msg) me
             return a)



instance Functor T where
   fmap = liftM

instance Applicative T where
   pure = Cons . return
   (<*>) = ap

instance Monad T where
   return = pure
   x >>= y = Cons $ decons . y =<< decons x

fromIO :: (IO.Handle -> IO a) -> T a
fromIO act = Cons $ lift . act =<< ask

ioeTry :: IO a -> IO (Either IOError a)
ioeTry = Exc.try

fragileFromIO :: (IO.Handle -> IO a) -> Parser.Fragile T a
fragileFromIO act =
   Sync.ExceptionalT . Cons . lift .
      fmap (Sync.mapException show . Sync.fromEither) . ioeTry . act
          =<< lift (Cons ask)

instance Parser.EndCheck T where
   isEnd   = fromIO IO.hIsEOF

instance Parser.C T where
   getByte = fragileFromIO $ liftM (fromIntegral . ord) . IO.hGetChar
   skip n  = fragileFromIO $ \h -> IO.hSeek h IO.RelativeSeek (NonNeg.toNumber n)
   warn    = Cons . lift . (\msg -> putStrLn ("warning: " ++ msg))
