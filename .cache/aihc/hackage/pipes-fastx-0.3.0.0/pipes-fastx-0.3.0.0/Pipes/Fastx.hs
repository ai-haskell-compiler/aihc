module Pipes.Fastx where

import Data.Attoparsec.Char8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Internal (ByteString(..), memcpy)
import Control.Monad
import Control.Applicative
import Control.Exception
import System.IO
import Data.Monoid
import Pipes
import Pipes.Internal
import qualified Pipes.ByteString as Pb
import qualified Pipes.Prelude as P
import Foreign.Ptr
import Foreign.ForeignPtr (newForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (allocaBytes, mallocBytes, reallocBytes, finalizerFree)
import Pipes.Attoparsec hiding (parse)

takeTill' c = takeTill (== c) <* char c
skipTill c = skipWhile (/= c) *> char c *> pure ()

parseFasta :: Parser (ByteString, ByteString)
parseFasta = (,) <$> (char '>' *> takeTill' '\n') <*> multiline where
    multiline = mconcat <$> (many1 $ do
        Just c <- peekChar
        if c == '>' then fail "next" else takeTill' '\n')

hFastaProd :: Handle -> Producer (ByteString, ByteString) IO ()
hFastaProd h = first where
    start_size = 1024
    first = do
        l <- liftIO $ B.hGetLine h
        case B8.uncons l of
            Just ('>', rest) -> go rest
            _ -> return ()
    go header = do
        let mkPS p nt = do
                p' <- reallocBytes p nt
                fp <- newForeignPtr finalizerFree p'
                return $! (PS fp 0 nt)
            loop p cap nt = do
                eof <- liftIO $ hIsEOF h
                if eof
                    then do sq <- liftIO $ mkPS p nt
                            yield (header, sq)
                    else do
                        l@(PS fpl offl lenl) <- liftIO $ B.hGetLine h
                        let cap2 = cap * 2
                        case B8.uncons l of
                            Just ('>', nexthead) -> do
                                sq <- liftIO $ mkPS p nt
                                yield (header, sq) >> go nexthead
                            Just {} -> do
                                let nt' = lenl + nt
                                (cap', p') <- liftIO $ if nt' > cap then (,) cap2 <$> reallocBytes p cap2 else return (cap, p)
                                liftIO $ withForeignPtr fpl $ \ps -> memcpy (p' `plusPtr` nt) (ps `plusPtr` offl) lenl
                                loop p' cap' nt'
                            Nothing -> return ()
        p <- liftIO $ mallocBytes start_size
        loop p start_size 0

hFastqProd :: Handle -> Producer (ByteString, ByteString, ByteString) IO ()
hFastqProd h = go where
    go = do
        let getline = B.hGetLine h
            doread = do
                line1 <- getline
                head <- case B8.uncons line1 of
                    Just ('@', rest) -> return rest
                    _ -> ioError $ userError "bad format"
                sq <- getline
                blank <- getline
                case B8.uncons blank of
                    Just ('+', _) -> return ()
                    _ -> ioError $ userError "bad format"
                qual <- getline
                when (B.length qual /= B.length sq) (ioError $ userError "bad format")
                return $ Just (head, sq, qual)
            catcher :: IOException -> IO (Maybe (ByteString, ByteString, ByteString))
            catcher _ = return Nothing
        mbread <- liftIO $ catch doread catcher
        case mbread of
            Just j -> yield j >> go
            Nothing -> return ()
