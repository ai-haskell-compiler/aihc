{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Pipes.MongoDB as MP
import Pipes
import Data.Text
import Database.MongoDB
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad ((>=>))

import qualified Pipes.Prelude as P

runAction :: Action IO a -> IO a
runAction a = do
  pipe <- connect (host "127.0.0.1")
  access pipe master "pipes-mongodb-example" a

main :: IO ()
main = runAction tasks

cities :: Producer (Maybe Text) (Action IO) ()
cities = MP.find (select [] "team")
     >-> P.map (look "home" >=> cast >=> look "city" >=> cast)

tasks :: Action IO ()
tasks = do
  clearTeams
  insertTeams
  -- pipes stuff
  runEffect (cities >-> P.print)

clearTeams :: MonadIO m => Action m ()
clearTeams = delete (select [] "team")

t :: Text -> Text
t v = v

insertTeams :: MonadIO m => Action m [Value]
insertTeams = insertMany "team" [
   ["name" =: t "Yankees", "home" =: ["city" =: t "New York", "state" =: t "NY"], "league" =: t "American"],
   ["name" =: t "Mets", "home" =: ["city" =: t "New York", "state" =: t "NY"], "league" =: t "National"],
   ["name" =: t "Phillies", "home" =: ["city" =: t "Philadelphia", t "state" =: t "PA"], "league" =: t "National"],
   ["name" =: t "Red Sox", "home" =: ["city" =: t "Boston", "state" =: t "MA"], "league" =: t "American"] ]
