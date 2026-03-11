{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe
import Data.Time.Zones.DB
import Data.Time.Zones.Files
import Test.Tasty.HUnit
import Test.Tasty.TH
import System.Posix.Env

case_Budapest_is_Budapest :: IO ()
case_Budapest_is_Budapest = do
  pathBp <- timeZonePathFromDB "Europe/Budapest"
  readBp <- BL.readFile pathBp
  readBp @=? tzDataByLabel Europe__Budapest
  let Just budByName = tzDataByName "Europe/Budapest"
  readBp @=? budByName

case_Etc__GMT'1_is_Etc__GMT'1 :: IO ()
case_Etc__GMT'1_is_Etc__GMT'1 = do
  pathEtc <- timeZonePathFromDB "Etc/GMT+1"
  readEtc <- BL.readFile pathEtc
  readEtc @=? tzDataByLabel Etc__GMT'1
  let Just etcByName = tzDataByName "Etc/GMT+1"
  readEtc @=? etcByName

case_fromToName :: IO ()
case_fromToName = forM_ [minBound .. maxBound] t
  where
    t :: TZLabel -> IO ()
    t label = Just label @=? fromTZName (toTZName label)

case_fromToSomeTZLabel :: IO ()
case_fromToSomeTZLabel = forM_ [minBound .. maxBound] t
  where
    t :: TZLabel -> IO ()
    t label = label @=? case someTZLabelVal label of SomeTZLabel proxy -> tzLabelVal proxy

case_promoteTZLabel :: IO ()
case_promoteTZLabel = forM_ [minBound .. maxBound] t
  where
    t :: TZLabel -> IO ()
    t label = someTZLabel @=? case someTZLabel of SomeTZLabel proxy -> promoteTZLabel proxy SomeTZLabel
      where someTZLabel = someTZLabelVal label

case_data_is_fat :: IO ()
case_data_is_fat = do
  assert $ BL.length (tzDataByLabel Europe__Budapest) > 2000

main :: IO ()
main = do
  -- When we are running 'cabal test' the package is not yet
  -- installed, so we want to use the data directory from within the
  -- sources.
  setEnv "tz_datadir" "./tzdata" True
  $defaultMainGenerator
