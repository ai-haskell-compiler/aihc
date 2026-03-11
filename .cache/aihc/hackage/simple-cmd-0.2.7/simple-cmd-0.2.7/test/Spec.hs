import Test.Hspec
import SimpleCmd

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "cmds" $ do
    it "cmd" $ do
      out <- cmd "echo" ["hello"]
      out `shouldBe` "hello"

    it "cmdBool true" $ do
      ok <- cmdBool "true" []
      ok `shouldBe` True

    it "cmdBool false" $ do
      ok <- cmdBool "false" []
      ok `shouldBe` False

    it "cmdLines" $ do
      out <- cmdLines "echo" ["1\n2"]
      out `shouldBe` ["1","2"]

    it "cmdN" $
      cmdN "ls" ["*"]

  describe "pipes" $ do
    it "pipe" $ do
      out <- pipe ("echo", ["hello"]) ("grep",["hello"])
      out `shouldBe` "hello"

  describe "timeIO" $ do
    it "sleep" $
      timeIO $ cmd_ "sleep" ["1"]
