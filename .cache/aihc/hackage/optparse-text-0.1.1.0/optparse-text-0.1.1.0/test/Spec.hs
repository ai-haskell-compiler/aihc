{-# LANGUAGE OverloadedStrings #-}

-- For base < 4.8
import           Control.Applicative       ((<$>), (<*>))
import           Data.Monoid               (mempty)

import qualified Data.Text                 as T
import qualified Options.Applicative       as Opt
import           Options.Applicative.Extra (execParserPure, getParseResult)
import qualified Options.Applicative.Text  as OptT
import           Test.Hspec

data Options = Options { text    :: T.Text
                       , textArg :: T.Text
                       , textOpt :: T.Text }
  deriving (Eq, Show)

optParser :: Opt.Parser Options
optParser = Options <$> Opt.argument OptT.text ( Opt.metavar "TEXT" )
                 <*> OptT.textArgument ( Opt.metavar "TEXT2" )
                 <*> OptT.textOption ( Opt.long "textopt" )

parser :: Opt.ParserInfo Options
parser = Opt.info (Opt.helper <*> optParser) mempty

main :: IO ()
main = hspec .
  describe "optparse-text" $ do
    let parse args = getParseResult $ execParserPure Opt.defaultPrefs parser args

    it "parses to text arguments" $
      parse ["arg1", "arg2", "--textopt", "hello world"] `shouldBe`
        Just Options {text = "arg1", textArg = "arg2", textOpt = "hello world"}

    it "parses unicode" $
      parse ["🎈", "nø tû brëxit", "--textopt", "Benno Fünfstück"] `shouldBe`
        Just Options {text = "🎈", textArg = "nø tû brëxit", textOpt = "Benno Fünfstück"}
