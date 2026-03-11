
-- | Taken from Michael Thompson <http://hackage.haskell.org/package/streaming-utils>

module Data.Attoparsec.ByteString.Streaming where

import qualified Data.Attoparsec.ByteString as A
import Data.ByteString.Streaming
import Data.ByteString.Streaming.Internal
import qualified Data.ByteString as B
import Streaming.Internal (Stream (..)) 
import Streaming hiding (concats, unfold)



type Message = ([String], String)

-- | The parsed function from @streaming-utils@

parsed
  :: Monad m
  => A.Parser a     -- ^ Attoparsec parser
  -> ByteString m r -- ^ Raw input
  -> Stream (Of a) m (Either (Message, ByteString m r) r)
parsed parser = begin
  where
    begin p0 = case p0 of  -- inspect for null chunks before
            Go m        -> lift m >>= begin -- feeding attoparsec 
            Empty r     -> Return (Right r)
            Chunk bs p1 | B.null bs -> begin p1
                        | otherwise -> step (chunk bs >>) (A.parse parser bs) p1
    step diffP res p0 = case res of
      A.Fail _ c m -> Return (Left ((c,m), diffP p0))
      A.Done bs a  | B.null bs -> Step (a :> begin p0) 
                   | otherwise -> Step (a :> begin (chunk bs >> p0))
      A.Partial k  -> do
        x <- lift (nextChunk p0)
        case x of
          Left e -> step diffP (k mempty) (return e)
          Right (bs,p1) | B.null bs -> step diffP res p1
                        | otherwise  -> step (diffP . (chunk bs >>)) (k bs) p1
{-# INLINABLE parsed #-}

