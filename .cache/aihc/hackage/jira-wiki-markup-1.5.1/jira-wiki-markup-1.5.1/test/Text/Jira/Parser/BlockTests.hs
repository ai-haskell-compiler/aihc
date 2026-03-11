{-|
Module      : Text.Jira.Parser.BlockTests
Copyright   : © 2019–2023 Albert Krewinkel
License     : MIT

Maintainer  : Albert Krewinkel <tarleb@zeitkraut.de>
Stability   : alpha
Portability : portable

Tests for jira wiki block parsers.
-}
module Text.Jira.Parser.BlockTests (tests) where

import Data.Either (isLeft)
import Data.Text ()
import Text.Jira.Markup
import Text.Jira.Parser.Block
import Text.Jira.Parser.Core

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), (@?))

import qualified Data.Text as Text

tests :: TestTree
tests = testGroup "Blocks"
  [ testGroup "components"
    [ testGroup "para"
      [ testCase "two lines" $
        parseJira block "one\ntwo\n" @?=
        Right (Para [Str "one", Linebreak, Str "two"])

      , testCase "ended by newline" $
        parseJira para "Hello, World\n" @?=
        Right (Para [Str "Hello,", Space, Str "World"])

      , testCase "ended by eof" $
        parseJira para "Hello, World" @?=
        Right (Para [Str "Hello,", Space, Str "World"])

      , testCase "ended by spaces and newline" $
        parseJira para "Hello, World  \n" @?=
        Right (Para [Str "Hello,", Space, Str "World"])

      , testCase "ended by blank line" $
        parseJira para "Hello\n\n" @?=
        Right (Para [Str "Hello"])

      , testCase "deleted text after linebreak" $
        parseJira para "foo\n-deleted-\n" @?=
        Right (Para [Str "foo", Linebreak, Styled Strikeout [Str "deleted"]])
      ]

    , testGroup "header"
      [ testCase "Level 1" $
        parseJira header "h1. Intro\n" @?=
        Right (Header 1 [Str "Intro"])

      , testCase "many spaces before title" $
        parseJira header "h2.         space\n" @?=
        Right (Header 2 [Str "space"])

      , testCase "no space after dot" $
        parseJira header "h3.hug\n" @?=
        Right (Header 3 [Str "hug"])

      , testCase "empty header" $
        parseJira header "h4.\n" @?=
        Right (Header 4 [])

      , testCase "Level 6" $
        parseJira header "h6. More\n" @?=
        Right (Header 6 [Str "More"])

      , testCase "Level 7 fails" $
        isLeft (parseJira header "h7. More\n") @? "level 7 header"

      , testCase "Level 0 fails" $
        isLeft (parseJira header "h0. More\n") @? "level 0 header"

      , testCase "leading spaces are disallowed" $
        isLeft (parseJira header " h1. nope\n") @? "leading spaces"
      ]

    , testGroup "horizontalRule"
      [ testCase "single ruler" $
        parseJira horizontalRule "----\n" @?=
        Right HorizontalRule
      ]

    , testGroup "list"
      [ testCase "single item list" $
        parseJira list "* hello\n" @?=
        Right (List CircleBullets [[Para [Str "hello"]]])

      , testCase "simple list" $
        let text = Text.unlines
                   [ "* one"
                   , "* two"
                   ]
        in parseJira list text @?=
           Right (List CircleBullets
                  [ [Para [Str "one"]]
                  , [Para [Str "two"]]])

      , testCase "list followed by different list" $
        parseJira ((,) <$> list <*> list) "- first\n* second\n" @?=
        Right ( List SquareBullets [[Para [Str "first"]]]
              , List CircleBullets [[Para [Str "second"]]])

      , testCase "nested lists" $
        parseJira list "* first\n** nested\n" @?=
        Right (List CircleBullets
               [
                 [ Para [Str "first"]
                 , List CircleBullets [[Para [Str "nested"]]]
                 ]
               ])

      , testCase "deeply nested list" $
        parseJira list "*-* nested\n*-* list\n" @?=
        Right (List CircleBullets
               [
                 [ List SquareBullets
                   [[ List CircleBullets
                      [ [Para [Str "nested"]]
                      , [Para [Str "list"]]]
                    ]]
                 ]
               ])

      , testCase "markers can vary" $
        parseJira list "#-* nested\n*** list\n" @?=
        Right (List Enumeration
               [[ List SquareBullets
                  [[ List CircleBullets
                     [ [Para [Str "nested"]]
                     , [Para [Str "list"]]
                     ]
                   ]]
                ]])

      , testCase "single nested list after paragraph" $
        let text = Text.unlines
                   [ "* line"
                   , "continued"
                   , "** nested"
                   ]
        in parseJira list text @?=
           Right (List CircleBullets
                  [ [ Para [Str "line", Linebreak, Str "continued"]
                    , List CircleBullets [[Para [Str "nested"]]]]])

      , testCase "multiple nested lists after paragraph" $
        let text = Text.unlines
                   [ "* line"
                   , "continued"
                   , "** nested"
                   , "*# another"
                   ]
        in parseJira list text @?=
           Right (List CircleBullets
                  [ [ Para [Str "line", Linebreak, Str "continued"]
                    , List CircleBullets [[Para [Str "nested"]]]
                    , List Enumeration [[Para [Str "another"]]]]])

      , testCase "item after nested list" $
        let text = Text.unlines
                   [ "* first"
                   , "* second"
                   , "** nested1"
                   , "** nested2"
                   , "* third"
                   ]
        in parseJira list text @?=
           Right (List CircleBullets
                  [ [ Para [Str "first"] ]
                  , [ Para [Str "second"]
                    , List CircleBullets [ [Para [Str "nested1"]]
                                         , [Para [Str "nested2"]]]]
                  , [ Para [Str "third"]] ])

      , testCase "nested lists" $
        let text = Text.unlines
                   [ "** eins"
                   , "*- zwei"
                   , "** drei"
                   ]
        in parseJira list text @?=
           Right (List CircleBullets
                  [ [ List CircleBullets [[Para [Str "eins"]]]
                    , List SquareBullets [[Para [Str "zwei"]]]
                    , List CircleBullets [[Para [Str "drei"]]]
                    ]
                  ])

      , testCase "indentation is ignored" $
        let text = Text.unlines
                   [ "        * One"
                   , "        * Two"
                   , "        ** Two.One"
                   , "    * Three"
                   ]
        in parseJira list text @?=
           Right (List CircleBullets
                 [ [ Para [Str "One"] ]
                 , [ Para [Str "Two"]
                   , List CircleBullets [[Para [Str "Two.One"]]]]
                 , [ Para [Str "Three"] ]
                 ])
      ]

    , testGroup "Table"
      [ testCase "single cell" $
        parseJira table "| Lua \n" @?=
        Right (Table [Row [BodyCell [Para [Str "Lua"]]]])

      , testCase "header cell" $
        parseJira table "|| Language\n" @?=
        Right (Table [Row [HeaderCell [Para [Str "Language"]]]])

      , testCase "2x2 table" $
        parseJira table
        (Text.unlines [ "|| Language || Type ||"
                           , "| Lua | dynamic |\n"])
        @?=
        Right (Table [ Row [ HeaderCell [Para [Str "Language"]]
                           , HeaderCell [Para [Str "Type"]]
                           ]
                     , Row [ BodyCell [Para [Str "Lua"]]
                           , BodyCell [Para [Str "dynamic"]]
                           ]
                     ])

      , testCase "row headeres" $
        parseJira table
        (Text.unlines [ "|| Language | Haskell ||"
                           , "|| Type | static |\n"])
        @?=
        Right (Table [ Row [ HeaderCell [Para [Str "Language"]]
                           , BodyCell [Para [Str "Haskell"]]
                           ]
                     , Row [ HeaderCell [Para [Str "Type"]]
                           , BodyCell [Para [Str "static"]]
                           ]
                     ])

      , testCase "list in table" $
        parseJira table "| * foo\n* bar\n" @?=
        Right (Table [
                  Row [BodyCell [List CircleBullets
                                  [ [Para [Str "foo"]]
                                  , [Para [Str "bar"]]
                                  ]]]])

      , testCase "multiple line cells" $
        parseJira table "| foo\nbar | baz |\n" @?=
        Right (Table [
                  Row [ BodyCell [Para [Str "foo", Linebreak, Str "bar"]]
                      , BodyCell [Para [Str "baz"]]]])

      , testCase "multiple lists in cell" $
        parseJira table "| * foo\n- bar\n" @?=
        Right (Table [Row [BodyCell [ List CircleBullets [[Para [Str "foo"]]]
                                    , List SquareBullets [[Para [Str "bar"]]]]]])

      , testCase "empty cell in row" $
        parseJira table (Text.unlines
                         [ "|a|b|"
                         , "| |b|"
                         , "|a| |"
                         ]) @?=
        Right (Table
               [ Row [BodyCell [Para [Str "a"]], BodyCell [Para [Str "b"]]]
               , Row [BodyCell [],               BodyCell [Para [Str "b"]]]
               , Row [BodyCell [Para [Str "a"]], BodyCell []]
               ])
      ]

    , testGroup "code"
      [ testCase "no language" $
        parseJira code "{code}\nprint('Hi Mom!'){code}\n" @?=
        Right (Code (Language "java") [] "print('Hi Mom!')")

      , testCase "with language" $
        parseJira code "{code:swift}\nfunc foo() -> Int { return 4 }{code}\n" @?=
        Right (Code (Language "swift") []
               "func foo() -> Int { return 4 }")

      , testCase "with parameters" $
        parseJira code "{code:title=coffee|bgColor=#ccc}\nblack(){code}\n" @?=
        Right (Code (Language "java")
               [Parameter "title" "coffee", Parameter "bgColor" "#ccc"]
               "black()")

      , testCase "with language and parameter" $
        parseJira code
        "{code:haskell|title=Hello World}\nputStrLn \"Hello, World!\"{code}\n" @?=
        Right (Code (Language "haskell")
               [Parameter "title" "Hello World"]
               "putStrLn \"Hello, World!\"")
      ]

    , testGroup "noformat"
      [ testCase "no parameters" $
        parseJira noformat "{noformat}\nline 1\nline 2{noformat}\n" @?=
        Right (NoFormat [] "line 1\nline 2")

      , testCase "with parameters" $
        parseJira noformat "{noformat:title=test}\nline 1\nline 2{noformat}\n" @?=
        Right (NoFormat [Parameter "title" "test"] "line 1\nline 2")

      , testCase "without newline" $
        parseJira noformat "{noformat}raw text{noformat}\n" @?=
        Right (NoFormat [] "raw text")
      ]

    , testGroup "panel"
      [ testCase "two-line paragraph" $
        parseJira panel "{panel}\nline 1\nline 2\n{panel}\n" @?=
        Right (Panel [] [Para [Str "line", Space, Str "1", Linebreak,
                               Str "line", Space, Str "2"]])

      -- FIXME: the next two shouldn't require a blank line after the contents
      , testCase "list" $
        parseJira panel "{panel}\n* first\n* second\n\n{panel}\n" @?=
        Right (Panel [] [List CircleBullets [ [Para [Str "first"]]
                                            , [Para [Str "second"]]]])

      , testCase "with parameters" $
        parseJira panel "{panel:title=test}\nline\n{panel}\n" @?=
        Right (Panel [Parameter "title" "test"] [Para [Str "line"]])
      ]

    , testGroup "color"
      [ testCase "single paragraph" $
        parseJira color "{color:red}This is red.\n{color}\n" @?=
        Right (Color (ColorName "red")
               [Para [Str "This", Space, Str "is", Space, Str "red."]])

      , testCase "paragraph preceeded by newline" $
        parseJira color "{color:#cccccc}\nThis is gray.\n{color}\n" @?=
        Right (Color (ColorName "#cccccc")
               [Para [ Linebreak, Str "This", Space
                     , Str "is", Space, Str "gray."]])
      ]

    , testGroup "blockQuote"
      [ testCase "single line right before eof" $
        parseJira blockQuote "bq. this text" @?=
        Right (BlockQuote [Para [Str "this", Space, Str "text"]])

      , testCase "single line blockquote" $
        parseJira blockQuote "bq. this test\n" @?=
        Right (BlockQuote [Para [Str "this", Space, Str "test"]])

      , testCase "single line w\\o leading space" $
        parseJira blockQuote "bq.another test\n" @?=
        Right (BlockQuote [Para [Str "another", Space, Str "test"]])

      , testCase "multiline block quote" $
        parseJira blockQuote "{quote}\n    quote\n me\n{quote}\n" @?=
        Right (BlockQuote [Para [Str "quote", Linebreak, Str "me"]])

      , testCase "multi-paragraph block quote" $
        parseJira blockQuote "{quote}\npara1\n\npara2\n{quote}\n" @?=
        Right (BlockQuote [ Para [Str "para1"]
                          , Para [Str "para2"]])

      , testCase "condensed block quote" $
        parseJira blockQuote "{quote}life is good{quote}\n" @?=
        Right (BlockQuote [Para [Str "life", Space, Str "is", Space, Str "good"]])
      ]
    ]

  , testGroup "block combinations"
    [ testCase "single paragraph" $
      parseJira block "Lorem ipsum." @?=
      Right (Para [Str "Lorem", Space, Str "ipsum."])

    , testCase "para before header" $
      parseJira ((,) <$> block <*> block) "paragraph\nh1.header\n" @?=
      Right (Para [Str "paragraph"], Header 1 [Str "header"])

    , testCase "para after header" $
      parseJira ((,) <$> block <*> block) "h2.header\nparagraph\n" @?=
      Right (Header 2 [Str "header"], Para [Str "paragraph"])

    , testCase "para before horizontal rule" $
      parseJira ((,) <$> block <*> return HorizontalRule) "paragraph\n----\n" @?=
      Right (Para [Str "paragraph"], HorizontalRule)

    , testCase "para after horizontal rule" $
      parseJira ((,) <$> block <*> block) "----\nparagraph\n" @?=
      Right (HorizontalRule, Para [Str "paragraph"])

    , testCase "para before blockquote" $
      parseJira ((,) <$> block <*> block) "before\nbq. a quote\n" @?=
      Right ( Para [Str "before"]
            , BlockQuote [Para [Str "a", Space, Str "quote"]])

    , testCase "para after blockquote" $
      parseJira ((,) <$> block <*> block) "bq. a quote\nafter\n" @?=
      Right ( BlockQuote [Para [Str "a", Space, Str "quote"]]
            , Para [Str "after"] )

    , testCase "para after list" $
      parseJira ((,) <$> block <*> block) "* foo\n\nbar\n" @?=
      Right (List CircleBullets [[Para [Str "foo"]]], Para [Str "bar"])

    , testCase "successive lists of same type" $
      parseJira ((,) <$> block <*> block) "* foo\n\n* bar\n" @?=
      Right ( List CircleBullets [[Para [Str "foo"]]]
            , List CircleBullets [[Para [Str "bar"]]])

    , testCase "para before table" $
      parseJira ((,) <$> block <*> block) "tabletest\n||Name|\n|Test|\n" @?=
      Right ( Para [Str "tabletest"]
            , Table [ Row [HeaderCell [Para [Str "Name"]]]
                    , Row [BodyCell [Para [Str "Test"]]]
                    ]
            )

    , testCase "para after table" $
      parseJira ((,) <$> block <*> block) "|| point |\nhuh\n" @?=
      Right ( Table [Row [HeaderCell [Para [Str "point"]]]]
            , Para [Str "huh"])

    , testCase "para after blankline terminated table" $
      parseJira ((,) <$> block <*> block) "|| love\n\npeace\n" @?=
      Right ( Table [Row [HeaderCell [Para [Str "love"]]]]
            , Para [Str "peace"])

    , testCase "para before code" $
      parseJira ((,) <$> block <*> block) "nice\n{code}\nhappy(){code}\n" @?=
      Right ( Para [Str "nice"]
            , Code (Language "java") [] "happy()")

    , testCase "para after code" $
      parseJira ((,) <$> block <*> block) "{code}\nfn(){code}\ntext" @?=
      Right ( Code (Language "java") [] "fn()"
            , Para [Str "text"])

    , testCase "para before noformat" $
      parseJira ((,) <$> block <*> block)
                 "wholesome\n{noformat}\nenjoy{noformat}\n" @?=
      Right ( Para [Str "wholesome"]
            , NoFormat [] "enjoy")

    , testCase "para after noformat" $
      parseJira ((,) <$> block <*> block) "{noformat}\nlala{noformat}\ntext" @?=
      Right ( NoFormat [] "lala"
            , Para [Str "text"])
    ]
  ]
