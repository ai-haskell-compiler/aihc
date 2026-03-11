module TestImport
  ( module X
  , normalizeText
  , parseRoutes
  , ResourceTree(..)
  , st
  ) where

import Yesod.Routes.Flow.Generator as X

import ClassyPrelude as X
import qualified Data.Text as T
import Test.Hspec as X
import Text.Shakespeare.Text (st)
import Yesod.Core.Dispatch (parseRoutes)
import Yesod.Routes.TH.Types (ResourceTree(..))

-- Avoid having to exactly compare leading whitespace or empty lines
normalizeText :: Text -> Text
normalizeText = T.unlines . filter (/="") . map T.strip . T.lines
