module Lentil.Parse.SourceSpec where

import Test.Hspec
import Text.Megaparsec (many, anySingle)

import Lentil.Helpers
import Lentil.Types
import Lentil.Parse.Source

-- Parsing tests

-- simple parser
sp :: ParSource a -> String -> Maybe a
sp p cs = either (const Nothing) Just
                 (runStateParser p () fp cs)
    where fp = "<f>"

-- short comment constructors
sl, ml :: Row -> String -> CommentString
sl r t = SingleLine r t
ml r t = MultiLine r t


main :: IO ()
main = hspec spec


spec :: Spec
spec = do

  describe "lineComment" $ do
    it "parses a line comment" $
      sp (lineComment "//") "// Test\n" `shouldBe` Just (sl 1 " Test")
    it "fails without trailing \\n" $
      sp (lineComment "//") "// Test"   `shouldBe` Nothing
    it "for single-char line-comments, erases repeating of such char" $
      sp (lineComment "#") "## Test\n"   `shouldBe` Just (sl 1 " Test")
    it "does not erease repeating for multi-char line-comments" $
      sp (lineComment "//") "//// Test\n"   `shouldBe` Just (sl 1 "// Test")

  describe "blockComment" $ do
    it "parses a block comment" $
      sp (blockComment ("/*","*/")) "/* Test\n2 */" `shouldBe`
         Just (ml 1 " Test\n2 ")

  describe "litString" $ do
    it "parses code string" $
      sp (litString ClangLike '"') "\"palla\"" `shouldBe` Just "palla"
    it "parses code string with escaped \" inside" $
      sp (litString ClangLike '"') "\"pal\\\"la\"" `shouldBe` Just "pal\"la"
    it "parses code string with comments symbols inside" $
      sp (litString ClangLike '"') "\"pal#la\"" `shouldBe` Just "pal#la"
    it "parses escape character (Pascal, SQL)" $
      sp (litString SQLLike '\'') "\'\\\'" `shouldBe` Just "\\"

  describe "litChar" $ do
    it "parses a string literal character inside (common)" $
      sp (litChar CommonChr '\'') "'a'" `shouldBe`  Just 'a'
    it "parses escaped characters too (common)" $
      sp (litChar CommonChr '\'') "'\"'" `shouldBe` Just '\"'
    it "parses an erlang-style char" $
      sp (litChar ErlangChr '$') "$%" `shouldBe` Just '%'

  let hss = StdSyntax ["--"] [("{-", "-}")]
                      ClangLike ['"'] CommonChr ['\'']
  describe "program" $ do
    it "parses program instructions till eof" $
      sp (program hss) "prova " `shouldBe`  Just "prova "
    it "stops at single-line comment" $
      sp (program hss) "prova -- " `shouldBe`  Just "prova "
    it "stops at blockcomment" $
      sp (program hss) "prova {- " `shouldBe`  Just "prova "
    it "stops at literal string" $
      sp (program hss) "prova \"babby " `shouldBe`  Just "prova "
    it "stops at literal char" $
      sp (program hss) "prova ' '" `shouldBe`  Just "prova "
    it "stops at ' which is not a literal char" $
      sp (program hss) "prova' " `shouldBe`  Just "prova' "

  -- RST --

  describe "rstTodo" $ do
    it "parses a reStructuredText+sphinx todo directive" $
      sp rstTodo ".. todo:: prova\n\n  foo" `shouldBe`
                 Just (ml 1 "TODO  prova\n \n   foo")
    it "fails if directive is not at the beginning of line" $
      sp rstTodo " .. todo:: prova\n\n  foo" `shouldBe` Nothing

  describe "rstOther" $ do
    it "parses a reStructuredText+sphinx other-than-todo directive part" $
      sp rstOther "gianni \n.. todo:: prova\n\n  foo" `shouldBe`
                 Just "gianni "

  describe "rstDocmuentPart" $ do
    it "parses a reStructuredText+sphinx document part: todo" $
      sp rstDocumentPart ".. todo:: prova\n\n  foo" `shouldBe`
                 Just (Just $ ml 1 "TODO  prova\n \n   foo")
    it "parses a reStructuredText+sphinx document part: non-todo" $
      sp (rstDocumentPart *> many anySingle)
         "gianni \n.. todo:: prova\n\n  foo" `shouldBe`
          Just ".. todo:: prova\n\n  foo"

  -- ORG --

  let orgOneLine = Just (sl 1 " TODO prova")
  describe "orgTodo" $ do
    it "parses a single line orgmode todo" $
      sp orgTodo "* TODO prova\n" `shouldBe` orgOneLine
    it "deeper levels" $
      sp orgTodo "** TODO prova\n" `shouldBe` orgOneLine
    it "doesn't parse next line" $
      sp orgTodo "** TODO prova\nxyz" `shouldBe` orgOneLine
    it "parses INACTIVE item" $
      sp orgTodo "** INACTIVE prova\n" `shouldBe`
          Just (sl 1 " INACTIVE prova")
    it "parses priority level" $
      sp orgTodo "** TODO [#A] prova\n" `shouldBe`
          Just (sl 1 " TODO [#A] prova")

  describe "orgListSingle" $ do
    it "parses a list-like orgmode todo" $
      sp orgListSingle "- [ ] prova\n" `shouldBe` orgOneLine
    it "`+` format" $
      sp orgListSingle "+ [ ] prova\n" `shouldBe` orgOneLine
    it "number/dot format" $
      sp orgListSingle "3. [ ] prova\n" `shouldBe` orgOneLine
    it "number/parens format" $
      sp orgListSingle "2) [ ] prova\n" `shouldBe` orgOneLine
    it "does not parse done items" $
      sp orgListSingle "- [X] prova\n" `shouldBe` Nothing

  let orgMultiSing = Just (ml 1 " TODO prova")
  describe "orgListMulti" $ do
    it "parses a list-like orgmode todo, multiline" $
      sp orgListMulti "- [ ] prova\n la rana" `shouldBe`
        Just (ml 1 " TODO prova\n la rana")
    it "stops at new list marker" $
      sp orgListMulti "- [ ] prova\n- [ ] xxx" `shouldBe` orgMultiSing
    it "does not stop at new list marker without newline" $
      sp orgListMulti "- [ ] prova\n - [ ] xxx" `shouldBe`
        Just (ml 1 " TODO prova\n - [ ] xxx")
    it "stops at blank line list marker" $
      sp orgListMulti "- [ ] prova\n\nxxx" `shouldBe` orgMultiSing

  -- SOURCE --

  let rbs = StdSyntax ["#"] [] ClangLike ['"', '\''] CommonChr []
  describe "source" $ do
    it "parses one piece of source (line-comment)" $
      sp (source hss) "-- hey\n my " `shouldBe` Just [sl 1 " hey"]
    it "parses one piece of source (block-comment)" $
      sp (source hss) "{-hey-}\n my " `shouldBe` Just [ml 1 "hey"]
    it "parses one piece of source (string-literal)" $
      sp (source hss) "\"hey\"" `shouldBe` Just []
    it "parses a string for language with ' and \" available" $
      sp (source rbs) "\"he#y\"" `shouldBe` Just []
    it "parses one piece of source (char-literal)" $
      sp (source hss) "\'h\'" `shouldBe` Just []
    it "parses one piece of source (program instructions)" $
      sp (source hss) "prime'" `shouldBe` Just []
    it "should not choke on en empty file" $
      sp (source hss) "" `shouldBe`  Just []

  describe "groupLineComms" $ do
    it "groups line comments" $
      groupLineComms [SingleLine 1 "a",
                      SingleLine 2 "b",
                      SingleLine 4 "c"] `shouldBe` [(1, "a\nb\n"),
                                                    (4, "c\n"   )]

  describe "comms2Tuple" $ do
    it "groups comments" $
      comms2Tuple [SingleLine 1 "a",
                   SingleLine 2 "b",
                   MultiLine  5 "c",
                   SingleLine 6 "d"] `shouldBe` [(1, "a\nb\n"),
                                                 (5, "c"     ),
                                                 (6, "d\n"   )]

