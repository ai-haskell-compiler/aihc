{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE CPP #-}
-- |  Golden tests utils for th-deepstrict

module Language.Haskell.TH.DeepStrict.Golden (testType) where

import Test.Tasty
import Test.Tasty.Golden
import Language.Haskell.TH.DeepStrict
import System.Directory
import Data.HashMap.Strict         (HashMap)
import Data.HashSet                  (HashSet)
import Data.ByteString (ByteString)

import qualified Language.Haskell.TH          as TH
import qualified Language.Haskell.TH.Syntax   as TH
import qualified Language.Haskell.TH.Ppr      as Ppr
import qualified Data.Map.Strict              as M
import qualified Data.IntMap.Strict as IM

ghcVersion :: String
ghcVersion = show @Int __GLASGOW_HASKELL__

doGolden :: TH.Type -> String -> TestTree
doGolden typ out =
 goldenVsFileDiff (Ppr.pprint typ)
      (\ref new -> [ "diff", "-u", ref, new ])
      golden
      actual
      action
 where
   file = sanitise (Ppr.pprint typ) ++ "-ghc-" ++ ghcVersion
   sanitise = map replacement
   replacement '.' = '-'
   replacement ' ' = '_'
   replacement x   = x
   dir = ".golden/" <> file
   golden = dir <> "/golden"
   actual = dir <> "/actual"
   action = do
     createDirectoryIfMissing True dir
     writeBinaryFile actual out

-- | Create a golden test for a specific type
testType :: TH.Type -> TH.Q TH.Exp
testType typ = do
 eC <- emptyContext
 let context =
       eC {contextOverride = M.fromList
        [ (''ByteString, Just [])
        , (''M.Map, Just [Strict, Strict])
        , (''IM.IntMap, Just [Strict])
        , (''HashSet, Just [Strict]) -- is a lazy hashmap underneath, but the keys are strict
        , (''HashMap, Just [Strict, Strict]) -- is strict already, but let's reduce the output size
        ]}
 [| doGolden $(TH.lift typ) $(TH.lift . Ppr.pprint =<< isDeepStrictWith context typ ) |]
