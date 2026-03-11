{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.FromForeignRegionSpec (spec) where

import Control.Monad.IO.Class
import Data.Word
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.ByteString                     as BS
import qualified Data.Vector.Storable                as DVS
import qualified HaskellWorks.Data.FromForeignRegion as IO
import qualified Hedgehog.Gen                        as G
import qualified Hedgehog.Range                      as R
import qualified System.Directory                    as IO
import qualified System.IO                           as IO

{- HLINT ignore "Redundant do" -}

spec :: Spec
spec = describe "HaskellWorks.Data.FromForeignRegionSpec" $ do
  it "Be able to load file into Vector" $ requireProperty $ do
    ws <- forAll $ G.list (R.linear 0 (1024 * 16)) (G.word8 R.constantBounded)
    liftIO $ IO.createDirectoryIfMissing True "./tmp"
    (fp, h) <- liftIO $ IO.openBinaryTempFile "./tmp" "property-test-.txt"
    liftIO $ BS.hPut h (BS.pack ws)
    liftIO $ IO.hClose h
    !(_ :: DVS.Vector Word64) <- liftIO $ IO.mmapFromForeignRegion fp
    -- liftIO $ IO.removeFile fp
    True === True
