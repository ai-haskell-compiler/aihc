{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tools to check and prepare data to be parsed as valid unicode.
--
-- The following is an example of converting dubious data to a text.
--
-- > textDecode :: ByteString -> Text
-- > textDecode b =
-- >   case bomStrip b of
-- >     (Nothing, s)           -> T.decodeUtf8 $ unicodeCleanUTF8 s -- Assume UTF8
-- >     (Just BOM_UTF8, s)     -> T.decodeUtf8 $ unicodeCleanUTF8 s
-- >     (Just BOM_UTF16_BE, s) -> T.decodeUtf16BE s
-- >     (Just BOM_UTF16_LE, s) -> T.decodeUtf16LE s
-- >     (Just BOM_UTF32_BE, s) -> T.decodeUtf32BE s
-- >     (Just BOM_UTF32_LE, s) -> T.decodeUtf32LE s
module Zenacy.Unicode
  ( BOM(..)
  , bomStrings
  , bomStrip
  , unicodeCleanUTF8
  ) where

import Foreign
  ( castPtr
  , withForeignPtr
  )
import Control.Monad.ST
  ( ST
  , runST
  )
import Data.STRef
  ( STRef
  , newSTRef
  , readSTRef
  , writeSTRef
  )
import Data.ByteString
  ( ByteString
  )
import qualified Data.ByteString as S
  ( index
  , length
  , null
  , packCStringLen
  , pack
  , stripPrefix
  )
import Data.Vector.Storable.Mutable
  ( MVector(..)
  )
import qualified Data.Vector.Storable.Mutable as U
  ( new
  , length
  , write
  , grow
  , unsafeToForeignPtr0
  )
import Data.Word
  ( Word8
  )
import System.IO.Unsafe
  ( unsafePerformIO
  )

-- | Defines the unicode byte order mark.
data BOM
  = BOM_UTF8
  | BOM_UTF16_BE
  | BOM_UTF16_LE
  | BOM_UTF32_BE
  | BOM_UTF32_LE
    deriving (Eq, Ord, Show)

-- | Defines the byte order mark signatures.
bomStrings :: [(BOM,ByteString)]
bomStrings =
  [ ( BOM_UTF8,     S.pack [ 0xEF, 0xBB, 0xBF ] )
  , ( BOM_UTF32_BE, S.pack [ 0x00, 0x00, 0xFE, 0xFF ] )
  , ( BOM_UTF32_LE, S.pack [ 0xFF, 0xFE, 0x00, 0x00 ] )
  -- The 16 bit codes need to be checked after the 32 bit codes,
  -- because the prefixes are similar.
  , ( BOM_UTF16_BE, S.pack [ 0xFE, 0xFF ] )
  , ( BOM_UTF16_LE, S.pack [ 0xFF, 0xFE ] )
  ]

-- | Remove the BOM from the start of a string.
bomStrip :: ByteString -> (Maybe BOM, ByteString)
bomStrip x =
  go bomStrings
  where
    go [] =
      (Nothing, x)
    go ((b,s):bs) =
      case S.stripPrefix s x of
        Just x' -> (Just b, x')
        Nothing -> go bs

-- | Removes bad characters and nulls from a UTF8 byte string.
unicodeCleanUTF8 :: ByteString -> ByteString
unicodeCleanUTF8 x =
  runST $ do
    v <- U.new 100
    go 0 0 v
  where
    go i j u
      | i == S.length x = do
          dataString u j
      | otherwise = do
          v <- if j + 3 < U.length u
                  then pure u
                  else U.grow u $ U.length u

          let c0 = S.index x (i + 0)
              c1 = S.index x (i + 1)
              c2 = S.index x (i + 2)
              c3 = S.index x (i + 3)

          if | (c0 >= 0x01 && c0 <= 0x7F) -> do
                 U.write v (j + 0) c0
                 go (i + 1) (j + 1) v

             | (c0 >= 0xC0 && c0 <= 0xDF) -> do
                 if | i + 1 < S.length x &&
                      (c1 >= 0x80 && c1 <= 0xBF) -> do
                        U.write v (j + 0) c0
                        U.write v (j + 1) c1
                        go (i + 2) (j + 2) v
                    | otherwise -> do
                        rep (i + 1) j v

             | (c0 >= 0xE0 && c0 <= 0xEF) -> do
                 if | i + 2 < S.length x &&
                      (c1 >= 0x80 && c1 <= 0xBF) &&
                      (c2 >= 0x80 && c2 <= 0xBF) -> do
                        U.write v (j + 0) c0
                        U.write v (j + 1) c1
                        U.write v (j + 2) c2
                        go (i + 3) (j + 3) v
                    | (c1 >= 0x80 && c1 <= 0xBF) -> do
                        rep (i + 2) j v
                    | otherwise ->
                        rep (i + 1) j v

             | (c0 >= 0xF0 && c0 <= 0xF7) -> do
                 if | i + 3 < S.length x &&
                      (c1 >= 0x80 && c1 <= 0xBF) &&
                      (c2 >= 0x80 && c2 <= 0xBF) &&
                      (c3 >= 0x80 && c3 <= 0xBF) -> do
                        U.write v (j + 0) c0
                        U.write v (j + 1) c1
                        U.write v (j + 2) c2
                        U.write v (j + 3) c3
                        go (i + 4) (j + 4) v
                    | (c1 >= 0x80 && c1 <= 0xBF) &&
                      (c2 >= 0x80 && c2 <= 0xBF) -> do
                        rep (i + 3) j v
                    | (c1 >= 0x80 && c1 <= 0xBF) -> do
                        rep (i + 2) j v
                    | otherwise ->
                        rep (i + 1) j v

             | otherwise -> do
                 rep (i + 1) j v

    rep i j v = do
      U.write v (j + 0) 0xEF
      U.write v (j + 1) 0xBF
      U.write v (j + 2) 0xBD
      go i (j + 3) v

-- | Converts a storable vector to a byte string.
dataString :: MVector s Word8 -> Int -> ST s ByteString
dataString v n =
  pure $ unsafePerformIO $ do
    let (f, _) = U.unsafeToForeignPtr0 v
    withForeignPtr f $ \p ->
      S.packCStringLen (castPtr p, n)
