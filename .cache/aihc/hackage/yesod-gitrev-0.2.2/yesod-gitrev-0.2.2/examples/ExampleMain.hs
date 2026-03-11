{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- to run this file, from the root dir of this repo:
-- stack build
-- stack runghc examples/ExampleMain.hs
-- Then visit localhost:3000/build-version in your browser

module ExampleMain where

import Yesod.Core
import Yesod.GitRev
import ExampleTH (ensureGitContext)

-- In case this is tested outside of a git context
$ensureGitContext

data Master = Master
  { getGitRev :: GitRev
  }

mkYesod "Master" [parseRoutes|
/build-version GitRevR GitRev getGitRev
|]

instance Yesod Master

main :: IO ()
main = warp 3000 $ Master $$(tGitRev)

main2 :: IO ()
main2 = warp 3000 $ Master $(gitRev)
