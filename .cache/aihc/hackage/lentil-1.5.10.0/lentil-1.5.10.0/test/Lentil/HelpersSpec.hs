module Lentil.HelpersSpec where


import Test.Hspec
import Text.Megaparsec
import Text.Megaparsec.Char

import Lentil.Helpers

-- Parsing tests

-- simple parser (choosable if we are at begin of line or else)
sp :: StateParser () a -> String -> Maybe a
sp p cs = either (const Nothing) Just
                    (runStateParser p () fp cs)
    where fp = "<f>"


main :: IO ()
main = hspec spec


spec :: Spec
spec = do

  describe "someTill (megaparsec reexport)" $ do
    it "behaves like manyTill on non-empty string" $
      sp (someTill anySingle newline) "foo\n" `shouldBe`
      sp (manyTill anySingle newline) "foo\n"
    it "fails on empty string (while manyTill does not)" $
      (sp (someTill anySingle newline) "\n",
       sp (manyTill  anySingle newline) "\n") `shouldBe` (Nothing,
                                                        Just "")
    it "has the same behaviour as manyTill on 1 char w/o end char" $
      sp (someTill anySingle newline) "f" `shouldBe`
      sp (manyTill anySingle newline) "f"

  describe "aliasp" $ do
    it "parses an extension alias" $
      aliasp "aaa%bc" `shouldBe` Just (".aaa", ".bc")
    it "fails on incorrect input" $
      aliasp "aaabc" `shouldBe` Nothing

