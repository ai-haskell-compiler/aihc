{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- import           Control.Monad
-- import qualified Data.DList as DL
-- import           Data.Function
-- import           HaskellWorks.Data.LoadJson
-- import           HaskellWorks.Data.Micro
-- import           HaskellWorks.Data.MQuery

main :: IO ()
main = do
  putStrLn "Hello world"
  -- !json <- loadJsonWithPoppy512SMinMaxIndex "firehose.json"
  -- let q = MQuery (DL.singleton json)
  -- putPretty $ q >>= (
  --   item  >=> entry >=> named "attack"
  --         >=> entry >=> named "instances"
  --         >=> entry >=> named "instance"
  --         >=> item
  --         >=> entry >=> named "dataset"
  --         >=> item >=> having (entry >=> named "name") >=> having (entry >=> named "name" >=> asString >=> valueOf "protos_bytes")
  --         >=> entry >=> named "item"
  --         >=> item >=> having (entry >=> named "name" >=> asString >=> valueOf "tcp")
  --         >=> entry >=> named "class"
  --         >=> entry >=> named "current"
  --         >=> entry >=> named "value"
  --         >=> castAsInteger) & aggregate sum
