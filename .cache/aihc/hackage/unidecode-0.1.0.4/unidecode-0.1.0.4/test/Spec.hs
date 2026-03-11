import           Test.Hspec
import           Text.Unidecode

main :: IO ()
main = hspec spec

spec = describe "unidecode" $ do
  it "doesn't hurt ascii text" $ do
    unidecode 'a' `shouldBe` "a"
