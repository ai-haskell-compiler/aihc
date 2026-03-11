
-- | Splitting functions for @ByteString m r@ into @Stream (ByteString m) m r@.
--
-- TODO These functions need quickcheck tests.

module Data.ByteString.Streaming.Split where

import Data.ByteString.Streaming.Char8 as S8
import Data.ByteString.Streaming.Internal as SI
import Streaming.Internal (Stream(..))



-- | Split a @ByteString m r@ after every @k@ characters.
--
-- Streams in constant memory.
--
-- BUG: Once the stream is exhausted, it will still call @splitAt@, forever
-- creating empty @ByteString@s.

splitsByteStringAt ∷ Monad m ⇒ Int → ByteString m r → Stream (ByteString m) m r
splitsByteStringAt !k = loop where
  loop (Empty r) = return r
  loop p = Step $ fmap loop $ S8.splitAt (fromIntegral k) p
{- -- this version would consume all memory
  loop p = SI.Effect $ do
    e ← nextChunk p
    return $ case e of
      Left r → SI.Return r
      Right (a,p') → SI.Step (fmap loop (S8.splitAt (fromIntegral k) (chunk a >> p')))
      -}
{-# Inlinable splitsByteStringAt #-}



-- | For lists, this would be @sbs (f :: [a] -> ([a],[a])) -> [a] -> [[a]]@.
-- Takes a function that splits the bytestring into two elements repeatedly,
-- where the first is followed by the repeated application of the function.
--
-- cf. <http://hackage.haskell.org/package/streaming-utils-0.1.4.7/docs/src/Streaming-Pipes.html#chunksOf>
--
-- TODO these functions should go into a helper library

separatesByteString
  ∷ Monad m
  ⇒ (ByteString m r → ByteString m (ByteString m r))
  → ByteString m r
  → Stream (ByteString m) m r
separatesByteString f = loop where
  loop (Empty r) = return r
  loop p = Step $ fmap loop $ f p
{-
  loop p = SI.Effect $ do
    e ← nextChunk p
    return $ case e of
      Left r → SI.Return r
      Right (a,p') → SI.Step (fmap loop (f (chunk a >> p')))
-}
{-# Inlinable separatesByteString #-}

