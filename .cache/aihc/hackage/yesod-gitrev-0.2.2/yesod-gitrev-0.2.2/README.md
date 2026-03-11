A subsite for displaying git information.

[![Hackage](https://img.shields.io/hackage/v/yesod-gitrev.svg)](https://hackage.haskell.org/package/yesod-gitrev)

You can use the `gitRev` splice (or `tGitRev` typed splice)
to generate a value of type `GitRev`.
Put this in your app's foundation,
add a route to the subsite,
and you're good to go.

See [Haskell and Yesod > Creating a Subsite]
(http://www.yesodweb.com/book/creating-a-subsite)
for details on how Yesod subsites work.

```
-- examples/Main.hs

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

import Yesod
import Yesod.GitRev

data Master = Master
  { getGitRev :: GitRev
  }

mkYesod "Master" [parseRoutes|
/build-version GitRevR GitRev getGitRev
|]

instance Yesod Master

main = warp 3000 $ Master $$(tGitRev)
```
