module Lentil.ArgsSpec where

import Test.Hspec

import Lentil.Types
import Lentil.Query
import Lentil.Args

import qualified Options.Applicative as OA


-- TEST VARS --

testOpt :: LOptions
testOpt = LOptions (["alpha"], ["beta"]) Csv
                   [filterFilepath "al", negFilter . filterTags $ "."]
                   [] [(".qq",".s")] ["hax", "ke"] (Just "foo.txt")

ins :: [String]
ins  = words "alpha -f csv -x beta -p al -t ^ -a qq%s \
             \--output foo.txt -w Hax -w ke"

readOpts :: [String] -> Maybe LOptions
readOpts i = OA.getParseResult $ OA.execParserPure OA.defaultPrefs parInfo i
    where
          parInfo  = OA.info lOpts OA.fullDesc

-- convert an LOptions type to something we can compare/show (eliminates
-- LFilter, LSort)
convComp :: LOptions -> (([FilePath], [FilePath]), Format,
                         [Alias], [FlagWord], Maybe FilePath)
convComp (LOptions ix fr _ _ al fw ou) = (ix, fr, al, fw, ou)

-- TESTING --

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "lOpts" $ do
    it "parses options" $
      fmap convComp (readOpts ins) `shouldBe` Just (convComp testOpt)

