import Test.Hspec
import Network.Download

-- available in base starting with base-4.6
isRight :: Either a b -> Bool
isRight = either (const False) (const True)

main :: IO ()
main = hspec $ do
  describe "openURI" $ do
    it "returns Right data constructor for http" $ do
     doc <- openURI "http://www.google.com"
     doc `shouldSatisfy` isRight
         
  describe "openAsTags" $ do
    it "returns Right data constructor for http" $ do
     doc <- openAsTags "http://www.google.com"
     doc `shouldSatisfy` isRight

  describe "openAsXML" $ do
    it "returns Right data constructor for http" $ do
     doc <- openAsXML "http://www.google.com"
     doc `shouldSatisfy` isRight

  describe "openAsFeed" $ do
    it "returns Right data constructor for http" $ do
     doc <- openAsFeed "http://www.google.com"
     doc `shouldSatisfy` isRight         

