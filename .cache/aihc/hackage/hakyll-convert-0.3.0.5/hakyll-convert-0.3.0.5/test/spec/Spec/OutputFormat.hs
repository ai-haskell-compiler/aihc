{-# LANGUAGE OverloadedStrings #-}

module Spec.OutputFormat (tests) where

import Data.Default
import Data.Maybe (isJust)
import qualified Data.Text as T
import Hakyll.Convert.Common (DistilledPost (..))
import Hakyll.Convert.OutputFormat
import Spec.SpecHelpers
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "OutputFormat"
    [ validOutputFormatTests,
      formatPathTests
    ]

validOutputFormatTests :: TestTree
validOutputFormatTests =
  testGroup
    "`validOutputFormat`"
    [ falseOnEmptyFormat,
      synchronyWithFormatPath
    ]
  where
    falseOnEmptyFormat =
      testCase
        "returns False if format string is empty"
        (validOutputFormat "" @?= False)

    synchronyWithFormatPath =
      testProperty
        "returns False if `formatPath` returns `Nothing`, otherwise True"
        ( \format ->
            let format' = T.pack format
                result = validOutputFormat format'
                formatPathResult = isJust (formatPath format' def)
             in not (null format) ==> (result && formatPathResult) || (not result && not formatPathResult)
        )

formatPathTests :: TestTree
formatPathTests =
  testGroup
    "`formatPath`"
    [ noChange,
      lowercaseO,
      lowercaseS,
      lowercaseY,
      uppercaseY,
      lowercaseM,
      lowercaseD,
      uppercaseH,
      uppercaseM,
      uppercaseS,
      complexExamples,
      abortsProcessingOnUnknownFormat
    ]
  where
    noChange =
      let input = "Hello, world!/2020-09-03-test.markdown"
       in testCase
            "does not change text that has no percent signs in it"
            (formatPath input def @?= Just input)

    lowercaseO =
      testGroup
        "%o is replaced by the original filepath"
        [ testCase
            "works with HTTP schema"
            (formatPath "%o" (def {dpUri = "http://example.com/post.html"}) @?= Just "post"),
          testCase
            "works with HTTPS schema"
            (formatPath "%o" (def {dpUri = "https://example.com/post.html"}) @?= Just "post"),
          testCase
            "trailing slashes are removed"
            (formatPath "%o" (def {dpUri = "https://example.com/2020-09-03-hello////"}) @?= Just "2020-09-03-hello"),
          testCase
            "file extension is removed"
            (formatPath "%o" (def {dpUri = "https://example.com/first-post.html"}) @?= Just "first-post"),
          testCase
            "all file extensions are removed"
            (formatPath "%o" (def {dpUri = "https://example.com/first-post.aspx.gz"}) @?= Just "first-post")
        ]

    lowercaseS =
      testCase
        "%s is replaced by the original slug"
        (formatPath "%s" (def {dpUri = "https://example.com/today-is-my-birthday.php"}) @?= Just "today-is-my-birthday")

    lowercaseY =
      testCase
        "%y is replaced by the two-digit publication year"
        (formatPath "%y" (def {dpDate = fromGregorian 1917 01 01 0 0 0}) @?= Just "17")

    uppercaseY =
      testCase
        "%Y is replaced by the four-digit publication year"
        (formatPath "%Y" (def {dpDate = fromGregorian 1917 1 1 0 0 0}) @?= Just "1917")

    lowercaseM =
      testCase
        "%m is replaced by the two-digit publication month"
        (formatPath "%m" (def {dpDate = fromGregorian 2011 3 1 2 3 4}) @?= Just "03")

    lowercaseD =
      testCase
        "%d is replaced by the two-digit publication day"
        (formatPath "%d" (def {dpDate = fromGregorian 2013 1 31 0 0 0}) @?= Just "31")

    uppercaseH =
      testCase
        "%H is replaced by the two-digit publication hour, 00 to 23"
        (formatPath "%H" (def {dpDate = fromGregorian 2014 1 1 23 0 0}) @?= Just "23")

    uppercaseM =
      testCase
        "%M is replaced by the two-digit publication minute"
        (formatPath "%M" (def {dpDate = fromGregorian 2015 1 2 3 59 0}) @?= Just "59")

    uppercaseS =
      testCase
        "%S is replaced by the two-digit publication second"
        (formatPath "%S" (def {dpDate = fromGregorian 2016 1 2 3 4 0}) @?= Just "00")

    complexExamples =
      testGroup
        "format string can contain multiple formats"
        [ helper "%H:%M:%S" "18:30:02",
          helper "old/%Y/%m/%d-%H%M%S-%s.html" "old/2003/12/13-183002-hello-world.html",
          helper "/posts/%y-%m-%d-%H%M%S-%s/" "/posts/03-12-13-183002-hello-world/",
          helper "/migrated/%o" "/migrated/~joe/hello-world",
          helper "99.9%% true/%o" "99.9% true/~joe/hello-world"
        ]
      where
        post =
          DistilledPost
            { dpUri = "https://example.com/~joe/hello-world.php",
              dpBody = "",
              dpTitle = Nothing,
              dpTags = [],
              dpCategories = [],
              dpDate = fromGregorian 2003 12 13 18 30 2
            }

        helper format expected = testCase format (formatPath (T.pack format) post @?= Just expected)

    abortsProcessingOnUnknownFormat =
      testGroup
        "returns Nothing upon encountering an unsupported format"
        [ testCase "unknown format %x" (formatPath "%H%M%S-%x.html" def @?= Nothing),
          testCase "no format specifier after percent sign" (formatPath "%H%M%" def @?= Nothing)
        ]
