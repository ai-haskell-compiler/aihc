{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Generate JSON from a source mapping.

module SourceMap (generate) where

import           SourceMap.Types
import qualified VLQ

import           Control.Monad hiding (forM_)
import           Control.Monad.ST
import           Data.Aeson hiding ((.=))
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as Map
#else
import qualified Data.HashMap.Lazy as Map
#endif
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as U
import           Data.ByteString.Builder (Builder(), lazyByteString, toLazyByteString)
import           Data.Foldable (forM_)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import           Data.List
import           Data.Maybe
import           Data.STRef
import           Data.Text (Text)
import           Data.Text.Lazy.Encoding (decodeUtf8)

-- | Generate the JSON from a source mapping.
generate :: SourceMapping -> Value
generate SourceMapping{..} = Object (Map.fromList obj) where
  obj = [("version",toJSON version)
        ,("file",toJSON smFile)
        ,("sources",toJSON sources)
        ,("names",toJSON names)
        ,("mappings",toJSON (decodeUtf8 (encodeMappings sources names smMappings)))] ++
        [("sourceRoot",toJSON root) | Just root <- [smSourceRoot]]
  names = nub $ mapMaybe mapName smMappings
  sources = symbols mapSourceFile
  symbols f = sort (nub (mapMaybe f smMappings))

-- | Encode the mappings to the source map format.
encodeMappings :: [FilePath] -> [Text] -> [Mapping] -> ByteString
encodeMappings sources names = go . sortOn mapGenerated where
  go mappings = runST $ do
    -- State.
    prevGenCol   <- newSTRef 0
    prevGenLine  <- newSTRef 1
    prevOrigCol  <- newSTRef 0
    prevOrigLine <- newSTRef 0
    prevName     <- newSTRef 0
    prevSource   <- newSTRef 0
    result       <- newSTRef (mempty :: Builder)
    -- Generate the groupings.
    forM_ (zip [0::Integer ..] mappings) $ \(i,Mapping{..}) -> do
      -- Continuations on the same line are separated by “,”, whereas
      -- new lines are separted by “;”.
      updating prevGenLine $ \previousGeneratedLine ->
        if posLine mapGenerated /= previousGeneratedLine
           then do prevGenCol .= 0
                   result += B.replicate (fromIntegral (posLine mapGenerated - previousGeneratedLine))
                                             (fromIntegral (fromEnum ';'))
                   return (posLine mapGenerated)
           else do when (i > 0)
                        (result += U.fromString ",")
                   return previousGeneratedLine
      -- Original generated column (also offsetted from previous entries).
      updating prevGenCol $ \previousGeneratedColumn -> do
        result += VLQ.encode (posColumn mapGenerated - previousGeneratedColumn)
        return (posColumn mapGenerated)
      -- Optional additional fields.
      case liftM2 (,) mapSourceFile mapOriginal of
        Nothing -> return ()
        Just (source,original) -> do
          -- Source index.
          updating prevSource $ \previousSource -> do
           result += VLQ.encode (indexOf source sources - previousSource)
           return (indexOf source sources)
          -- Original line (also offsetted from previous entries).
          updating prevOrigLine $ \previousOriginalLine -> do
           result += VLQ.encode (posLine original - 1 - previousOriginalLine)
           return (posLine original - 1)
          -- Original column (also offsetted from previous entries).
          updating prevOrigCol $ \previousOriginalColumn -> do
           result += VLQ.encode (posColumn original - previousOriginalColumn)
           return (posColumn original)
          -- Optional name
          forM_ mapName $ \name -> do
            updating prevName $ \previousName -> do
             result += VLQ.encode (indexOf name names - previousName)
             return (indexOf name names)
    -- Return the byte buffer.
    toLazyByteString <$> readSTRef result

  updating r f = readSTRef r >>= (f >=> writeSTRef r)
  r += y = modifySTRef r (<> lazyByteString y)
  x .= y = writeSTRef x y; infixr 1 .=
  indexOf e xs = fromIntegral (fromMaybe 0 (elemIndex e xs))

-- | Format version.
version :: Integer
version = 3
