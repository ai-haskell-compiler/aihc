module HaskellWorks.Data.TreeCursor
    ( TreeCursor(..)
    ) where

import HaskellWorks.Data.Positioning

class TreeCursor k where
  firstChild  :: k -> Maybe k
  nextSibling :: k -> Maybe k
  parent      :: k -> Maybe k
  depth       :: k -> Maybe Count
  subtreeSize :: k -> Maybe Count
