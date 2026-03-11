import Test.Hspec
import Test.Hspec.Parsec
import Text.Parsec
import Text.Parsec.String (Parser)

main :: IO ()
main = hspec $ do
  describe "yamlBool" $ do
    let yamlBool' = parse yamlBool ""

    it "correctly parses \"True\"" $ do
      yamlBool' "True" `shouldParse` True

    it "doesn't parse \"yes\"" $ do
      yamlBool' `shouldFailOn` "yes"

yamlBool :: Parser Bool
yamlBool = 
        (choice (map string false) *> pure False)
    <|> (choice (map string true) *> pure True)
  where
    false = ["false", "False", "FALSE"]
    true = ["true", "True", "TRUE"]
