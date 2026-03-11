
-- | Test the performance of splitting and consuming a *very long* bytestring
-- using @streaming@.

module Main where

import           Data.Functor.Of
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.ByteString.Streaming.Char8 as SB8
import           System.TimeIt
import           Text.Printf

import qualified Data.ByteString.Streaming.Split as SBS



genLong
  ∷ ( Monad m )
  ⇒ Int
  → SB8.ByteString m ()
genLong = SB8.fromLazy . BSL8.fromChunks . go
  where
    -- create chunks of strict char8 bytestring to concatenate.
    go !k
      | k <= 0    = []
      | otherwise = chunk : go (k-1)
    -- the chunk to use for concatenation.
    !chunk = BS8.pack $ ['A'..'Z'] ++ "\n" ++ ['a'..'z']
{-# Inlinable genLong #-}

-- | Generate long strings, split them after every 10 characters (which is
-- different from the chunk length), concatenate the resulting strings, and get
-- the total string length.
--
-- This function should run in constant memory.

longLength ∷ ( Monad m ) ⇒ Int → m (Of Int ())
longLength = SB8.length . SB8.concat . SBS.splitsByteStringAt 10 . genLong

otherLength ∷ ( Monad m ) ⇒ Int → m (Of Int ())
otherLength = SB8.length . SB8.concat . SBS.separatesByteString (SB8.splitAt 10) . genLong

benchmark ∷ (Int → IO (Of Int ())) → Int → Int → IO ()
benchmark !f !k !l = do
  (!seconds, !n :> ()) ← timeItT $ f l
  printf "53 * 10^%2d = %10d characters in %9.6f seconds\n" k n seconds

main = do
  putStrLn "longLength"
  mapM_ (uncurry (benchmark longLength))  [(k ∷ Int, 10^k) | k ← [0..7]]
  putStrLn "otherLength"
  mapM_ (uncurry (benchmark otherLength)) [(k ∷ Int, 10^k) | k ← [0..7]]
  return ()

