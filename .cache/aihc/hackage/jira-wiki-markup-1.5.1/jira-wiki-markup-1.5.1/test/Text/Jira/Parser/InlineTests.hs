{-|
Module      : Text.Jira.Parser.InlineTests
Copyright   : © 2019–2023 Albert Krewinkel
License     : MIT

Maintainer  : Albert Krewinkel <tarleb@zeitkraut.de>
Stability   : alpha
Portability : portable

Tests for the jira wiki inline markup parsers.
-}
module Text.Jira.Parser.InlineTests (tests) where

import Data.Either (isLeft)
import Data.Text ()
import Text.Jira.Markup
import Text.Jira.Parser.Core
import Text.Jira.Parser.Inline
import Text.Parsec (many1)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), (@?))

tests :: TestTree
tests = testGroup "Inline"
  [ testGroup "components"
    [ testGroup "str"
      [ testCase "simple word" $
        parseJira str "word" @?= Right (Str "word")

      , testCase "non-special symbols" $
        parseJira str ",.#%" @?= Right (Str ",.#%")

      , testCase "umlauts" $
        parseJira str "äéíöüßðå" @?= Right (Str "äéíöüßðå")

      , testCase "mix of alphanums and non-special chars" $
        parseJira str "20.09" @?= Right (Str "20.09")

      , testCase "space fails" $
        isLeft (parseJira str " ") @?
        "str should only be parsed into Space"
      ]

    , testGroup "specialChar"
      [ testCase "plain special char" $
        parseJira specialChar "!" @?= Right (SpecialChar '!')

      , testCase "escaped symbol" $
        parseJira specialChar "\\{" @?= Right (SpecialChar '{')
      ]

    , testGroup "dash"
      [ testCase "en dash" $
        parseJira dash "--" @?= Right (Str "–")

      , testCase "em dash" $
        parseJira dash "---" @?= Right (Str "—")
      ]

    , testGroup "emoji"
      [ testCase "smiling face" $
        parseJira emoji ":D" @?= Right (Emoji IconSmiling)

      , testCase "winking face" $
        parseJira emoji ";)" @?= Right (Emoji IconWinking)

      , testCase "checkmark" $
        parseJira emoji "(/)" @?= Right (Emoji IconCheckmark)

      , testCase "red x" $
        parseJira emoji "(x)" @?= Right (Emoji IconX)

      , testCase "thumbs up" $
        parseJira emoji "(y)" @?= Right (Emoji IconThumbsUp)

      , testCase "green star" $
        parseJira emoji "(*g)" @?= Right (Emoji IconStarGreen)

      , testCase "may not be followed by a letter" $
        isLeft (parseJira emoji "(x)nope") @? "no letters after emojis"
      ]

    , testGroup "whitespace"
      [ testCase "space" $
        parseJira whitespace " " @?= Right Space

      , testCase "tab" $
        isLeft (parseJira whitespace "\t") @?
        "TAB is not considered whitespace"

      , testCase "nonbreaking space fails" $
        isLeft (parseJira whitespace "\160") @?
        "NBSP is not considered whitespace"

      , testCase "zero width space fails" $
        isLeft (parseJira whitespace "\8203") @?
        "ZWSP is not considered whitespace"

      , testCase "newline fails" $
        isLeft (parseJira whitespace "\n") @?
        "newline is not considered whitespace"
      ]

    , testGroup "entity"
      [ testCase "named entity" $
        parseJira entity "&copy;" @?= Right (Entity "copy")

      , testCase "numerical entity" $
        parseJira entity "&#65;" @?= Right (Entity "#65")

      , testCase "invalid entity" $
        parseJira entity "&haskell;" @?= Right (Entity "haskell")

      , testCase "space" $
        isLeft (parseJira entity "&a b;") @?
        "entities may not contain spaces"

      , testCase "symbol" $
        isLeft (parseJira entity "&a-b;") @?
        "entities may not contain symbols"

      , testCase "number without hash" $
        isLeft (parseJira entity "&65;") @?
        "numerical entities must start with &#"

      , testCase "no name" $
        isLeft (parseJira entity "&;") @?
        "entities must not be empty"
      ]

    , testGroup "styled"

      [ testCase "deleted" $
        parseJira styled "-far-fetched-" @?=
        Right (Styled Strikeout [Str "far", SpecialChar '-', Str "fetched"])

      , testCase "symbol before closing char" $
        parseJira styled "-backwards<-" @?=
        Right (Styled Strikeout [Str "backwards<"])

      , testGroup "emphasis"
        [ testCase "single word" $
          parseJira styled "_single_" @?= Right (Styled Emphasis [Str "single"])

        , testCase "multi word" $
          parseJira styled "_multiple words_" @?=
          Right (Styled Emphasis [Str "multiple", Space, Str "words"])

        , testCase "forced markup" $
          parseJira styled "{_}forced{_}" @?=
          Right (Styled Emphasis [Str "forced"])

        , testCase "symbol before opening underscore" $
          parseJira (str *> styled) "#_bar_" @?=
          Right (Styled Emphasis [Str "bar"])

        , testCase "neither symbol nor space before opening underscore" $
          isLeft (parseJira (str *> styled) "foo_bar_") @? "space after opening char"

        , testCase "disallow space after opening underscore" $
          isLeft (parseJira styled "_ nope_") @? "space after underscore"

        , testCase "require word boundary after closing underscore" $
          isLeft (parseJira styled "_nope_nope") @? "no boundary after closing"

        , testCase "disallow newline in markup" $
          isLeft (parseJira styled "_eol\nnext line_") @? "newline in markup"

        , testCase "zero with space as word boundary" $
          parseJira ((,) <$> styled <*> str) "_yup_\8203next" @?=
          Right (Styled Emphasis [Str "yup"], Str "\8203next")
        ]

      , testCase "inserted" $
        parseJira styled "+multiple words+" @?=
        Right (Styled Insert [Str "multiple", Space, Str "words"])

      , testCase "strong" $
        parseJira styled "*single*" @?= Right (Styled Strong [Str "single"])

      , testCase "subscript" $
        parseJira styled "~multiple words~" @?=
        Right (Styled Subscript [Str "multiple", Space, Str "words"])

      , testCase "superscript" $
        parseJira styled "^multiple words^" @?=
        Right (Styled Superscript [Str "multiple", Space, Str "words"])
    ]

    , testCase "monospaced" $
      parseJira monospaced "{{multiple words}}" @?=
      Right (Monospaced [Str "multiple", Space, Str "words"])

    , testGroup "linebreak"
      [ testCase "linebreak before text" $
        parseJira linebreak "\na" @?=
        Right Linebreak

      , testCase "double-backslash linebreak" $
        parseJira linebreak "\\\\" @?=
        Right Linebreak

      , testCase "linebreak at eof fails" $
        isLeft (parseJira linebreak "\n") @? "newline before eof"

      , testCase "linebreak before blank line fails" $
        isLeft (parseJira linebreak "\n\n") @? "newline before blank line"

      , testCase "linebreak before list fails" $
        isLeft (parseJira linebreak "\n\n") @? "newline before list"

      , testCase "linebreak before header fails" $
        isLeft (parseJira linebreak "\nh1.foo\n") @? "newline before header"

      , testCase "three backslashes do not cause a linebreak" $
        isLeft (parseJira linebreak "\\\\\\") @? "three backslashes"
      ]

    , testCase "anchor" $
      parseJira anchor "{anchor:testing}" @?=
      Right (Anchor "testing")

    , testGroup "autolink"
      [ testCase "hypertext link" $
        parseJira autolink "https://example.org/foo" @?=
        Right (AutoLink (URL "https://example.org/foo"))

      , testCase "link followed by text" $
        parseJira autolink "ftp://example.com/passwd has passwords" @?=
        Right (AutoLink (URL "ftp://example.com/passwd"))

      , testCase "email" $
        parseJira autolink "mailto:nobody@test.invalid" @?=
        Right (AutoLink (URL "mailto:nobody@test.invalid"))

      , testCase "braces cannot be in bare links" $
        parseJira autolink "https://example.edu/{*}" @?=
        Right (AutoLink (URL "https://example.edu/"))

      , testCase "file URIs are not autolinks" $
        isLeft (parseJira autolink "file:///etc/fstab") @? ""
      ]

    , testGroup "citation"
      [ testCase "name" $
        parseJira citation "??John Doe??" @?=
        Right (Citation [Str "John", Space, Str "Doe"])

      , testCase "with markup" $
        parseJira citation "??Jane *Example* Doe??" @?=
        Right (Citation [ Str "Jane", Space, Styled Strong [Str "Example"]
                        , Space, Str "Doe"])
      ]

    , testGroup "link"
      [ testCase "unaliased link" $
        parseJira link "[https://example.org]" @?=
        Right (Link External [] (URL "https://example.org"))

      , testCase "aliased link" $
        parseJira link "[Example|https://example.org]" @?=
        Right (Link External [Str "Example"] (URL "https://example.org"))

      , testCase "alias with emphasis" $
        parseJira link "[_important_ example|https://example.org]" @?=
        Right (Link External
               [Styled Emphasis [Str "important"], Space, Str "example"]
                (URL "https://example.org"))

      , testCase "alias with URL" $
        parseJira link "[https://example.org website|https://example.org]" @?=
        Right (Link External
                [ Str "https", SpecialChar ':', Str "//example.org"
                , Space, Str "website"]
                (URL "https://example.org"))

      , testCase "link to anchor" $
        parseJira link "[see here|#there]" @?=
        Right (Link External [Str "see", Space, Str "here"] (URL "#there"))

      , testCase "mail address" $
        parseJira link "[send mail|mailto:me@nope.invalid]" @?=
        Right (Link Email [Str "send", Space, Str "mail"]
               (URL "me@nope.invalid"))

      , testGroup "attachment link"
        [ testCase "simple attachment" $
          parseJira link "[testing^test.xml]" @?=
          Right (Link Attachment [Str "testing"] (URL "test.xml"))

        , testCase "attachment without description" $
          parseJira link "[^results.txt]" @?=
          Right (Link Attachment [] (URL "results.txt"))

        , testCase "filename with space and unicode" $
          parseJira link "[^Straßenbahn Berlin.jpg]" @?=
          Right (Link Attachment [] (URL "Straßenbahn Berlin.jpg"))
        ]

      , testGroup "smart links"
        [ testCase "smart link" $
          parseJira link "[hslua|https://github.com/hslua/hslua|smart-link]" @?=
          Right (Link SmartLink [Str "hslua"]
                 (URL "https://github.com/hslua/hslua"))

        , testCase "smart card" $
          parseJira link
            "[repo|https://github.com/tarleb/jira-wiki-markup|smart-card]" @?=
          Right (Link SmartCard [Str "repo"]
                 (URL "https://github.com/tarleb/jira-wiki-markup"))
        ]

      , testCase "user link" $
        parseJira link "[testing|~account-id:something]" @?=
        Right (Link User [Str "testing"] (URL "account-id:something"))

      , testCase "user without description" $
        parseJira link "[~username]" @?=
        Right (Link User [] (URL "username"))
      ]

    , testGroup "image"
      [ testCase "local file" $
        parseJira image "!image.jpg!" @?=
        Right (Image [] (URL "image.jpg"))

      , testCase "no newlines" $
        isLeft (parseJira image "!hello\nworld.png!") @?
        "no newlines in image names"

      , testCase "thumbnail" $
        parseJira image "!image.png|thumbnail!" @?=
        Right (Image [Parameter "thumbnail" ""] (URL "image.png"))

      , testCase "parameters" $
        parseJira image "!image.gif|align=right, vspace=4!" @?=
        let params = [ Parameter "align" "right"
                     , Parameter "vspace" "4"
                     ]
        in Right (Image params (URL "image.gif"))

      , testCase "quoted parameter" $
        parseJira image "!foo.jpg|alt=\"some foo!\"!" @?=
        let params = [ Parameter "alt" "some foo!"]
        in Right (Image params (URL "foo.jpg"))

      , testCase "first character cannot be a space" $
        isLeft (parseJira image "! foo.jpg!") @?
        "An image cannot start with a space."
      ]

    , testGroup "color"
      [ testCase "colored word" $
        parseJira colorInline "{color:red}red{color}" @?=
        Right (ColorInline (ColorName "red") [Str "red"])

      , testCase "hex color" $
        parseJira colorInline "{color:#526487}blueish{color}" @?=
        Right (ColorInline (ColorName "#526487") [Str "blueish"])

      , testCase "hex color without hash" $
        parseJira colorInline "{color:526Ab7}blueish{color}" @?=
        Right (ColorInline (ColorName "#526Ab7") [Str "blueish"])
      ]
    ]

  , testGroup "inline parser"
    [ testCase "simple sentence" $
      parseJira (normalizeInlines <$> many1 inline) "Hello, World!" @?=
      Right [Str "Hello,", Space, Str "World", SpecialChar '!']

    , testCase "with entity" $
      parseJira (many1 inline) "shopping at P&amp;C" @?=
      Right [ Str "shopping", Space, Str "at", Space
            , Str "P", Entity "amp", Str "C"
            ]

    , testCase "autolink followed by pipe" $
      parseJira (many1 inline) "https://jira.example/file.txt|" @?=
      Right [AutoLink (URL "https://jira.example/file.txt"), SpecialChar '|']

    , testCase "autolink followed by pipe" $
      parseJira (many1 inline) "https://jira.example/file.txt|" @?=
      Right [AutoLink (URL "https://jira.example/file.txt"), SpecialChar '|']

    , testCase "backslash-escaped char" $
      parseJira (normalizeInlines <$> many1 inline) "opening brace: \\{" @?=
      Right [ Str "opening", Space, Str "brace", SpecialChar ':', Space
            , SpecialChar '{']

    , testCase "icon after word" $
      parseJira (many1 inline) "checkmark(/)" @?=
      Right [Str "checkmark", Emoji IconCheckmark]

    , testCase "smiley after word" $
      parseJira (normalizeInlines <$> many1 inline) "smiley:)" @?=
      Right [Str "smiley", Emoji IconSlightlySmiling]

    , testCase "escaped smiley after word" $
      parseJira (normalizeInlines <$> many1 inline) "closing paren\\:)" @?=
      Right [Str "closing", Space, Str "paren", SpecialChar ':', Str ")"]

    , testCase "smiley between words" $
      parseJira (normalizeInlines <$> many1 inline) "verdict: :D funny" @?=
      Right [ Str "verdict", SpecialChar ':', Space
            , Emoji IconSmiling, Space, Str "funny"]

    , testCase "smiley within word" $
      parseJira (normalizeInlines <$> many1 inline) "C:DE" @?=
      Right [ Str "C", SpecialChar ':', Str "DE" ]

    , testCase "backslash is literal if it's not necessary" $
      parseJira (normalizeInlines <$> many1 inline) "\\:PA" @?=
      Right [ SpecialChar '\\', SpecialChar ':', Str "PA"]

    , testCase "dash with spaces" $
      parseJira (many1 inline) "one  -- two" @?=
      Right [Str "one", Space, Str "–", Space, Str "two"]

    , testCase "forced markup" $
      parseJira (many1 inline) "H{~}2{~}O" @?=
      Right [Str "H", Styled Subscript [Str "2"], Str "O"]

    , testCase "color in sentence" $
      parseJira (many1 inline) "This is {color:red}red{color}." @?=
      Right [ Str "This", Space, Str "is", Space
            , ColorInline (ColorName "red") [Str "red"]
            , Str "."
            ]

    , testCase "hypen between numbers" $
      -- the hypens used to be treated as deletion markers.
      parseJira (many1 inline) "-15 02-3" @?=
      Right [ SpecialChar '-', Str "15" , Space, Str "02"
            , SpecialChar '-', Str "3"
            ]

    , testCase "ascii arrows" $
      -- the hypens used to be treated as deletion markers.
      parseJira (many1 inline) "-> step ->" @?=
      Right [ SpecialChar '-', Str ">" , Space, Str "step", Space
            , SpecialChar '-', Str ">"
            ]

    , testCase "long ascii arrow" $
      parseJira (many1 inline) "click --> done" @?=
      Right [ Str "click", Space, SpecialChar '-', SpecialChar '-'
            , Str ">", Space, Str "done"]

    ]
  ]
