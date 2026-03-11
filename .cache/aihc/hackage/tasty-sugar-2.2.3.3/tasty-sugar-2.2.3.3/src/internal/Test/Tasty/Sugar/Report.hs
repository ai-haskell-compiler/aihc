{-# LANGUAGE OverloadedStrings #-}

-- | Generate user-consumable reports regarding the findings of tasty-sugar.

module Test.Tasty.Sugar.Report
  (
    sweetsKVITable
  , sweetsTextTable
  )
  where

import           Data.KVITable
import           Data.KVITable.Render.ASCII ( render
                                            , defaultRenderConfig
                                            , RenderConfig(..) )
import           Data.String ( fromString )
import           Data.Text ( Text )
import           Lens.Micro ( (&), (.~) )
import qualified Prettyprinter as PP
import           System.FilePath ( takeFileName )

import           Test.Tasty.Sugar.Types


-- | Converts a set of discovered Sweets into a KVITable; this is usually done in
-- order to render the KVITable in a readable format.
sweetsKVITable :: [Sweets] -> KVITable FilePath
sweetsKVITable [] = mempty
sweetsKVITable sweets =
  let t = fromList $ concatMap
          (\s ->
              [
                ( ("base", fromString $ rootBaseName s)
                  : ("rootFile", fromString $ rootFile s)
                  : [ (fromString pn, fromString $ show $ PP.pretty pv)
                    | (pn,pv) <- expParamsMatch e ]
                  <> [ (fromString an, fromString $ takeFileName af)
                     | (an,af) <- associated e ]
                , takeFileName (expectedFile e)
                )
              | e <- expected s
              ])
          sweets
  in t & valueColName .~ "Expected File"

-- | Converts a set of discovered Sweets directly into a text-based table for
-- showing to the user.
sweetsTextTable :: [CUBE] -> [Sweets] -> Text
sweetsTextTable [] _ = "No CUBE provided for report"
sweetsTextTable _ [] = "No Sweets provided for report"
sweetsTextTable (c:_) s =
  let cfg = defaultRenderConfig
            { rowGroup = "base"
                         : "rootFile"
                         : (fromString . fst <$> take 1 (validParams c))
            , rowRepeat = False
            }
  in render cfg $ sweetsKVITable s
