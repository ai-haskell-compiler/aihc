module Lentil.Parse.IssueSpec where

import Test.Hspec
import Text.Megaparsec.Char

import Lentil.Types
import Lentil.Parse.Issue
import Lentil.Helpers

-- Parsing tests

-- simple parser (choosable if we are at begin of line or else)
sp :: [FlagWord] -> ParIssue a -> String -> Maybe a
sp fws p cs = either (const Nothing) Just
                     (runStateParser p fws fp cs)
    where
          fp  = "<f>"


main :: IO ()
main = hspec spec



spec :: Spec
spec = do

  describe "normaliseFlagword" $ do
    it "normalises the capitalisation of a flag-word" $
      normaliseFlagword "TODO" `shouldBe` "todo"
    it "normalises capitalisation midword" $
      normaliseFlagword "tOdo" `shouldBe` "todo"
    it "correctly handles empty strings" $
      normaliseFlagword "" `shouldBe` ""

  describe "ciString" $ do
    it "parses a case insensitive string" $
      sp [] (ciString "tEst") "Test" `shouldBe` Just "Test"

  describe "eoft" $ do
    it "parses a textfile end of file" $
      sp [] (string "test" <* eoft) "test\n" `shouldBe` Just "test"
    it "or an eof without '\\n'" $
      sp [] (string "test" <* eoft) "test"   `shouldBe` Just "test"

  describe "blankline" $ do
    it "parses an empty line" $
      sp [] blankline "\n\nlol"   `shouldBe` Just ()
    it "deals with eof blanklines too" $
      sp [] blankline "\n" `shouldBe` Just ()

  describe "htmlify" $ do
    it "removes unneeded whitespace" $
      htmlify "one  two " `shouldBe` "one two"
    it "removes newlines too" $
      htmlify "one  two\nthree " `shouldBe` "one two three"

  describe "tag" $ do
    it "parses a tag" $
      fmap tagString (sp [] tag "[test-tag]") `shouldBe` Just "test-tag"
    it "should not allow whitespace inside a tag" $
      fmap tagString (sp [] tag "[broken tag]") `shouldBe` Nothing

  describe "incipit" $ do
    it "parses the initial section of an issue, returns nothing" $
       do sp [] incipit "TODO: "  `shouldBe` Just "todo"
          sp [] incipit "FIXME: "  `shouldBe` Just "fixme"
          sp [] incipit "XXX: "  `shouldBe` Just "xxx"
    it "parses the initial section of an issue (user-defined flagword)" $
       sp ["feature"] incipit "Feature: "  `shouldBe` Just "feature"
    it "parses the initial sect. (user-defined, normalising)" $
       sp ["feaTure"] incipit "Feature: "  `shouldBe` Just "feature"
    it "doesn't work without previous newline" $
      sp [] incipit "TODO:some"  `shouldBe` Nothing
    it "doesn't work with longer matches (todoodle)" $
      sp [] incipit "TODOodle:some"  `shouldBe` Nothing
    it "doesn't work if you don't add a space after :" $
      do sp [] incipit "TODO:some"  `shouldBe` Nothing
         sp [] incipit "TODO_some"  `shouldBe` Nothing
    it "does allow you to omit the :" $
      sp [] incipit "xxx some"  `shouldBe` Just "xxx"
    it "is case unsensitive in flag-word" $
      sp [] incipit "tOdO some"  `shouldBe` Just "todo"
    it "doesn't allow anything before flag-words" $
      sp [] incipit "so fixme someday"  `shouldBe` Nothing
    it "fails if we are not at the beginning of line" $
      sp [] incipit "so fixme someday"  `shouldBe` Nothing

  describe "tags" $ do
    it "parses a series of tags, sep by whitespace" $
      sp [] tags " [tag] [field:test] [tog]"
         `shouldBe` Just [Tag "tag", Tag "field:test", Tag "tog"]
    it "allows ending whitespace" $
      sp [] tags "   [tag] [field:test] [tog] "
         `shouldBe` Just [Tag "tag", Tag "field:test", Tag "tog"]

  describe "freeText" $ do
    it "parses a free form text" $
      sp [] freeText "this is it\n\n" `shouldBe` Just (Just "this is it")
    it "can be ended by tags/fields" $
      sp [] freeText "this is it [field:val]" `shouldBe`
            Just (Just "this is it")
    it "trims extra whitespace" $
      sp [] freeText "this is  it [bug]" `shouldBe` Just (Just "this is it")
    it "cannot be ended by new issue on same line" $
      sp [] freeText "this is it   TODO: desc\n" `shouldBe`
             Just (Just "this is it TODO: desc")
    it "cannot be ended by new issue on new-but-not-beginning-of-line" $
      sp [] freeText "this is it \nn  TODO: desc\n" `shouldBe`
             Just (Just "this is it n TODO: desc")
    it "can be ended by blank line" $
      sp [] freeText "this is it\n\n  TODO: desc" `shouldBe`
             Just (Just "this is it")
    it "can be ended by incomplete blank line (eof)" $
      sp [] freeText "this is it\n" `shouldBe` Just (Just "this is it")

  describe "issue" $ do
    it "parses an issue" $
      sp [] issue "fixMe this is it [t] [f:w]"
         `shouldBe` (Just $ Issue "<f>" 1 (Just "this is it")
                                  [Tag "fixme", Tag "t", Tag "f:w"])
    it "parses tagless/fieldless issues too" $
      sp [] issue "TODO: this is it\n"
         `shouldBe` (Just $ Issue "<f>" 1 (Just "this is it") [])
    it "parses an issue not ended by \\n" $
      sp [] issue "todo block1 "
         `shouldBe` (Just $ Issue "<f>" 1 (Just "block1") [])
    it "doesn't display the eventual \\n at eof" $
      sp [] issue "TOdO: this is it\n"
         `shouldBe` (Just $ Issue "<f>" 1 (Just "this is it") [])
    it "does accept a lone (naked) todo" $
      sp [] issue "todo\n"
         `shouldBe` (Just $ Issue "<f>" 1 Nothing [])
    it "issues declared with fixme get a free [fixme] tag" $
      sp [] issue "fixme blah\n"
         `shouldBe` (Just $ Issue "<f>" 1 (Just "blah") [Tag "fixme"])
    it "doesn't parse tags non separated by a space" $
      sp [] issue "todo blah[f]\n"
         `shouldBe` (Just $ Issue "<f>" 1 (Just "blah[f]") [])
    it "does parse an empty todo" $
      sp [] issue "todo\n"
         `shouldBe` (Just $ Issue "<f>" 1 Nothing [])
    it "does parse an empty fixme" $
      sp [] issue "fixme \n"
         `shouldBe` (Just $ Issue "<f>" 1 Nothing [Tag "fixme"])
    it "does parse an empty todo + tags" $
      sp [] issue "todo [alfa]\n"
         `shouldBe` (Just $ Issue "<f>" 1 Nothing [Tag "alfa"])
    it "does parse tags before description" $
      sp [] issue "todo [alfa] beta\n"
         `shouldBe` (Just $ Issue "<f>" 1 (Just "beta") [Tag "alfa"])
    it "works without newline too" $
      sp [] issue "fixme "
         `shouldBe` (Just $ Issue "<f>" 1 Nothing [Tag "fixme"])

  describe "issues" $ do
    it "parses multiple issues" $
      sp [] issues "TODO: this is it [f:w]\n TODO: hey [t]"
         `shouldBe` Just [Issue "<f>" 1 (Just "this is it") [Tag "f:w"],
                          Issue "<f>" 2 (Just "hey") [Tag "t"]]
    it "parses multiple issues, first starting w/ newline" $
      sp [] issues "\nTODO: this is it [f:w]\n TODO: hey [t]"
         `shouldBe` Just [Issue "<f>" 2 (Just "this is it") [Tag "f:w"],
                          Issue "<f>" 3 (Just "hey") [Tag "t"]]
    it "parses multiple issues, first whitespaced" $
      sp [] issues "   TODO: this is it [f:w]\n TODO: hey [t]"
         `shouldBe` Just [Issue "<f>" 1 (Just "this is it") [Tag "f:w"],
                          Issue "<f>" 2 (Just "hey") [Tag "t"]]
    it "does not parse multiple issues if on the same line" $
      sp [] issues "TODO: this is it [f:w] TODO: hey [t]"
         `shouldBe` Just [Issue "<f>" 1 (Just "this is it") [Tag "f:w"]]
    it "parses two issues, tagless, if the second starts on nl" $
      sp [] issues "TODO: this is it \n  todo hey [t]"
         `shouldBe` Just [Issue "<f>" 1 (Just "this is it") [],
                          Issue "<f>" 2 (Just "hey") [Tag "t"]]
    it "doesn't pick issues in the middle of a sentence" $
      sp [] issues "\nthis is not a todo as you can see" `shouldBe` Just []
    it "does not choke on quasi-incipit" $
      sp [] issues "TODO/HACK, we \n TODO this is it\n"
         `shouldBe` Just [Issue "<f>" 2 (Just "this is it") []]

