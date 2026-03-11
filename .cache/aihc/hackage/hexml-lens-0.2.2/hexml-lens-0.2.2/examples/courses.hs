{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PartialTypeSignatures    #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# OPTIONS -Wno-unused-top-binds #-}

import           Control.Applicative        (optional)
import qualified Control.Category           as C
import           Control.Lens               hiding (children)
import qualified Data.ByteString.Lazy.Char8 as LB
import           Network.Wreq
import           Test.Hspec
import           Text.XML.Hexml
import           Text.XML.Hexml.Lens

url,url2 :: String
url = "https://web.archive.org/web/20200211134934if_/http://aiweb.cs.washington.edu/research/projects/xmltk/xmldata/data/courses/reed.xml"
url2 = "https://web.archive.org/web/20171112123740if_/http://aiweb.cs.washington.edu:80/research/projects/xmltk/xmldata/data/courses/uwm.xml"

type Strings = String

data Place s = Place
  { _building :: s
  , _room     :: Int
  }
  deriving Show

data Course s = Course
  { _title            :: s
  , _courseInstructor :: Maybe s
  , _place            :: Maybe (Place s)
  , _courseSections   :: [Section s]
  }
  deriving Show

data Section s = Section
  { _name        :: s
  , _days        :: s
  , _start, _end :: s
  , _instructor  :: s
  }
  deriving Show

makeFields ''Place
makeFields ''Course
makeFields ''Section

main :: IO ()
main = do
  r <- get url
  r2 <- get url2
  let parser :: Fold (Response LB.ByteString) (Course Strings)
      parser = responseBody . to stripDocTypeB . _XML . _children . folded . courseF
  lengthOf parser r  `shouldBe` 2510
  lengthOf parser r2 `shouldBe` 2112
  r  `shouldSatisfy` allOf (parser.sections) null
  r2 `shouldSatisfy` allOf (parser.sections) (not.null)

stripDocTypeB :: LB.ByteString -> LB.ByteString
stripDocTypeB = LB.unlines . drop 2 . LB.lines

courseF :: Fold Node (Course Strings)
courseF = runFold $ do
  _title <- field "title"
  _courseInstructor <- optional $ field "instructor"
  _place <- optional $ Fold $ node "place" . placeF
  _courseSections <- Fold $ multiple $ node "section_listing" . sectionF
  return Course{..}

sectionF :: Fold Node (Section Strings)
sectionF = runFold $ do
  _instructor <- field "instructor"
  _name <- field "section"
  _days <- field "days"
  (_start, _end) <- Fold $ node "hours" . runFold hoursF
  return Section{..}

hoursF :: ReifiedFold Node (Strings, Strings)
hoursF = (,) <$> field "start" <*> field "end"

placeF :: Fold Node (Place Strings)
placeF = runFold $ do
  building <- field "building"
  room     <- Fold _Show C.. field "room"
  return (Place building room)

field :: (XML a, XML t) => t -> ReifiedFold Node a
field n = Fold $ node n . _inner
