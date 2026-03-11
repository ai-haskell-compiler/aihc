# hexml-lens
[![Hackage](https://img.shields.io/hackage/v/hexml-lens.svg)](https://hackage.haskell.org/package/hexml-lens)
[![Stackage Nightly](http://stackage.org/package/hexml-lens/badge/nightly)](http://stackage.org/nightly/package/hexml-lens)
[![GitHub CI](https://github.com/pepeiborra/hexml-lens/workflows/CI/badge.svg)](https://github.com/pepeiborra/hexml-lens/actions)

Folds and getters for the Hexml `Node` type.
```haskell
{-# LANGUAGE RecordWildCards #-}
import Control.Lens hiding (children)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Network.Wreq
import Text.XML.Hexml
import Text.XML.Hexml.Lens

url :: [Char]
url = "http://aiweb.cs.washington.edu/research/projects/xmltk/xmldata/data/courses/reed.xml"

data Place s = Place { building :: s , room :: Int}
  deriving Show

data Course s = Course { title, instructor :: s , place :: Place s}
  deriving Show

main :: IO ()
main = do
  r <- get url
  let stripDocType = LB.unlines . drop 2 . LB.lines
  let courses = take 10 $ r ^.. responseBody . to stripDocType . _XML . _children . folded . courseF
  print courses

courseF :: Fold Node (Course B.ByteString)
courseF = runFold $ do
  title <- Fold $ node "title" . textContents
  instructor <- Fold $ node "instructor" . textContents
  place <- Fold $ node "place" . placeF
  return $ Course{..}

placeF :: Fold Node (Place B.ByteString)
placeF = runFold $ do
  building <- Fold $ node "building" . textContents
  room     <- Fold $ node "room" . textContents . _Show
  return (Place building room)
```
