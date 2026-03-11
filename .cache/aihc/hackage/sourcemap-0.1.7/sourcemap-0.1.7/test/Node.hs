{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | This test suite requires the source-map node package. That is the
-- reference implementation, so we generate sourcemaps and compare
-- them with what the source-map package would generate.

module Main where

import           SourceMap
import           SourceMap.Types

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as Bytes
import qualified Data.ByteString.Lazy.UTF8 as Bytes
import           Data.List
import           System.Exit
import           System.Process

-- | Run the test suite.
main :: IO ()
main =
  compareImplementations
    SourceMapping { smFile = "out.js"
                  , smSourceRoot = Nothing
                  , smMappings = mappings  }

-- | Compare the Haskell implementation with the JavaScript
-- implementation for the given source mapping.
compareImplementations :: SourceMapping -> IO ()
compareImplementations m = do
  !n <- generateNode m
  let !h = generate m
  if h == n
     then exitSuccess
     else do putStr "Node:    "; Bytes.putStrLn $ encode n
             putStr "Haskell: "; Bytes.putStrLn $ encode h
             exitFailure

-- | Generate a source map using the nodejs source-map package.
generateNode :: SourceMapping -> IO Value
generateNode SourceMapping{..} = do
  (ret, out, err) <- readProcessWithExitCode "node" [] source
  if ret /= ExitSuccess
    then error err
    else
      case decode (Bytes.fromString (concat (lines out))) of
         Just value -> return value
         Nothing    -> error "unable to parse node's json output"

  where source = unlines $
          ["var s = require('source-map');"
          ,""
          ,"var generator = new s.SourceMapGenerator({"] ++
          [intercalate "," (("file: " ++ show smFile) :
                             ["sourceRoot: " ++ show root | Just root <- [smSourceRoot]])] ++
          ["});"
          ,""] ++
          map addMapping smMappings ++
          ["console.log(generator.toString())"]
        addMapping Mapping{..} = unlines $
            ["generator.addMapping({"
            ,"  generated: " ++ showPos mapGenerated ++ ","] ++
            ["  original: " ++ showPos orig ++ "," | Just orig <- [mapOriginal]] ++
            ["  source: " ++ show src ++ "," | Just src <- [mapSourceFile]] ++
            ["  name: " ++ show name | Just name <- [mapName]] ++
            ["});"]
        showPos Pos{..} = "{ line: " ++ show posLine ++ ", column: " ++ show posColumn ++ " }"

-- | A sample source mapping.
mappings :: [Mapping]
mappings =
  [Mapping {
        mapGenerated = Pos 1 1,
        mapOriginal = Just (Pos 1 1),
        mapSourceFile = Just "one.js",
        mapName = Nothing
  }
  ,Mapping {
        mapGenerated = Pos 1 5,
        mapOriginal = Just (Pos 1 5),
        mapSourceFile = Just "one.js",
        mapName = Nothing
  }
  ,Mapping {
        mapGenerated = Pos 1 9,
        mapOriginal = Just (Pos 1 11),
        mapSourceFile = Just "one.js",
        mapName = Nothing
  }
  ,Mapping {
        mapGenerated = Pos 1 18,
        mapOriginal = Just (Pos 1 21),
        mapSourceFile = Just "one.js",
        mapName = Just "bar"
  }
  ,Mapping {
        mapGenerated = Pos 1 21,
        mapOriginal = Just (Pos 2 3),
        mapSourceFile = Just "one.js",
        mapName = Nothing
  }
  ,Mapping {
        mapGenerated = Pos 1 28,
        mapOriginal = Just (Pos 2 10),
        mapSourceFile = Just "one.js",
        mapName = Just "baz"
  }
  ,Mapping {
        mapGenerated = Pos 1 32,
        mapOriginal = Just (Pos 2 14),
        mapSourceFile = Just "one.js",
        mapName = Just "bar"
  }
  ,Mapping {
        mapGenerated = Pos 2 1,
        mapOriginal = Just (Pos 1 1),
        mapSourceFile = Just "two.js",
        mapName = Nothing
  }
  ,Mapping {
        mapGenerated = Pos 2 5,
        mapOriginal = Just (Pos 1 5),
        mapSourceFile = Just "two.js",
        mapName = Nothing
  }
  ,Mapping {
        mapGenerated = Pos 2 9,
        mapOriginal = Just (Pos 1 11),
        mapSourceFile = Just "two.js",
        mapName = Nothing
  },
  Mapping {
         mapGenerated = Pos 2 18,
         mapOriginal = Just (Pos 1 21),
         mapSourceFile = Just "two.js",
         mapName = Just "n"
   },
   Mapping {
         mapGenerated = Pos 2 21,
         mapOriginal = Just (Pos 2 3),
         mapSourceFile = Just "two.js",
         mapName = Nothing
   },
   Mapping {
         mapGenerated = Pos 2 28,
         mapOriginal = Just (Pos 2 10),
         mapSourceFile = Just "two.js",
         mapName = Just "n"
   }]
