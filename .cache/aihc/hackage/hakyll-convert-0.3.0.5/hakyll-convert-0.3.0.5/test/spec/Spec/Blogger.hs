{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Spec.Blogger (tests) where

import qualified Data.Text as T
import Hakyll.Convert.Blogger
import Hakyll.Convert.Common (DistilledPost (..))
import Spec.SpecHelpers
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (expectFail)
import Test.Tasty.HUnit
import qualified Text.Atom.Feed as Atom

deriving instance Eq DistilledPost

deriving instance Show DistilledPost

tests :: TestTree
tests =
  testGroup
    "Blogger.distill"
    [ extractsPostUri,
      extractsPostBody,
      extractsPostTitle,
      canSkipComments,
      canExtractComments,
      enumeratesAllCommentAuthors,
      errorsOnNonHtmlPost,
      errorsOnNonHtmlComment,
      turnsIncorrectDatesIntoEpochStart,
      parsesDates,
      extractsPostTags
    ]

extractsPostUri :: TestTree
extractsPostUri =
  testGroup
    "extracts post's URI"
    [ testCase (T.unpack uri) (dpUri (distill False (createInput uri)) @?= uri)
      | uri <-
          [ "https://example.com/testing-post-uris",
            "http://www.example.com/~joe/posts.atom"
          ]
    ]
  where
    createInput uri =
      FullPost
        { fpPost = entry,
          fpComments = [],
          fpUri = uri
        }
    entry =
      Atom.nullEntry
        "https://example.com/entry"
        (Atom.TextString "Test post")
        "2003-12-13T18:30:02Z"

extractsPostBody :: TestTree
extractsPostBody =
  testGroup
    "extracts post's body"
    [ testCase (T.unpack body) (dpBody (distill False (createInput body)) @?= body)
      | body <-
          [ "<p>Today was a snowy day, and I decided to...</p>",
            "<h3>My opinion on current affairs</h3><p>So you see, I...</p>"
          ]
    ]
  where
    createInput body =
      FullPost
        { fpPost = createEntry body,
          fpComments = [],
          fpUri = "https://example.com"
        }
    createEntry body =
      ( Atom.nullEntry
          "https://example.com/entry"
          (Atom.TextString "Test post")
          "2003-12-13T18:30:02Z"
      )
        { Atom.entryContent = Just (Atom.HTMLContent body)
        }

extractsPostTitle :: TestTree
extractsPostTitle =
  testGroup
    "extracts post's title"
    [ testCase (T.unpack title) (dpTitle (distill False (createInput title)) @?= Just (title))
      | title <-
          [ "First post",
            "You won't believe what happened to me today",
            "Trying out <i>things</i>&hellip;"
          ]
    ]
  where
    createInput title =
      FullPost
        { fpPost = createEntry title,
          fpComments = [],
          fpUri = "https://example.com/titles.atom"
        }
    createEntry title =
      Atom.nullEntry
        "https://example.com/entry"
        (Atom.TextString title)
        "2003-12-13T18:30:02Z"

canSkipComments :: TestTree
canSkipComments =
  testCase
    "does not extract comments if first argument is False"
    (dpBody (distill False input) @?= expected)
  where
    input =
      FullPost
        { fpPost = entry,
          fpComments = [comment],
          fpUri = "https://example.com/feed"
        }
    entry =
      ( Atom.nullEntry
          "https://example.com/entry"
          (Atom.TextString "First post")
          "2003-12-13T18:30:02Z"
      )
        { Atom.entryContent = Just (Atom.HTMLContent "<p>Hello, world!</p>"),
          Atom.entryPublished = Just "2003-12-13T18:30:02Z"
        }
    comment =
      ( Atom.nullEntry
          "https://example.com/entry#comment1"
          (Atom.TextString "Nice")
          "2003-12-13T20:00:03Z"
      )
        { Atom.entryContent = Just (Atom.HTMLContent "<p>Nice post.</p>")
        }

    expected = "<p>Hello, world!</p>"

canExtractComments :: TestTree
canExtractComments =
  testGroup
    "extracts comments if first argument is True"
    [ noDateNoAuthor,
      dateNoAuthor,
      noDateAuthor,
      dateAuthor
    ]
  where
    createInput comment =
      FullPost
        { fpPost = entry,
          fpComments = [comment],
          fpUri = "https://example.com/feed"
        }

    entry =
      ( Atom.nullEntry
          "https://example.com/entry"
          (Atom.TextString "First post")
          "2003-12-13T18:30:02Z"
      )
        { Atom.entryContent = Just (Atom.HTMLContent "<p>Hello, world!</p>"),
          Atom.entryPublished = Just "2003-12-13T18:30:02Z"
        }

    noDateNoAuthor =
      testCase
        "comments with no \"published\" date and no author"
        (dpBody (distill True (createInput commentNoDateNoAuthor)) @?= expectedNoDateNoAuthor)
    commentNoDateNoAuthor =
      ( Atom.nullEntry
          "https://example.com/entry#comment1"
          (Atom.TextString "Nice")
          "2003-12-13T20:00:03Z"
      )
        { Atom.entryContent = Just (Atom.HTMLContent "<p>Nice post.</p>")
        }
    expectedNoDateNoAuthor =
      "<p>Hello, world!</p>\n\n\
      \<h3 id='hakyll-convert-comments-title'>Comments</h3>\n\
      \<div class='hakyll-convert-comment'>\n\
      \<p class='hakyll-convert-comment-date'>On unknown date,  wrote:</p>\n\
      \<div class='hakyll-convert-comment-body'>\n\
      \<p>Nice post.</p>\n\
      \</div>\n\
      \</div>"

    dateNoAuthor =
      testCase
        "comments with a \"published\" date but no author"
        (dpBody (distill True (createInput commentDateNoAuthor)) @?= expectedDateNoAuthor)
    commentDateNoAuthor =
      commentNoDateNoAuthor
        { Atom.entryPublished = Just "2019-01-02T03:04:05Z"
        }
    expectedDateNoAuthor =
      "<p>Hello, world!</p>\n\n\
      \<h3 id='hakyll-convert-comments-title'>Comments</h3>\n\
      \<div class='hakyll-convert-comment'>\n\
      \<p class='hakyll-convert-comment-date'>On 2019-01-02T03:04:05Z,  wrote:</p>\n\
      \<div class='hakyll-convert-comment-body'>\n\
      \<p>Nice post.</p>\n\
      \</div>\n\
      \</div>"

    noDateAuthor =
      testCase
        "comments with no \"published\" date but with an author"
        (dpBody (distill True (createInput commentNoDateAuthor)) @?= expectedNoDateAuthor)
    commentNoDateAuthor =
      commentNoDateNoAuthor
        { Atom.entryAuthors = [Atom.nullPerson {Atom.personName = "John Doe"}]
        }
    expectedNoDateAuthor =
      "<p>Hello, world!</p>\n\n\
      \<h3 id='hakyll-convert-comments-title'>Comments</h3>\n\
      \<div class='hakyll-convert-comment'>\n\
      \<p class='hakyll-convert-comment-date'>On unknown date, John Doe wrote:</p>\n\
      \<div class='hakyll-convert-comment-body'>\n\
      \<p>Nice post.</p>\n\
      \</div>\n\
      \</div>"

    dateAuthor =
      testCase
        "comments with a \"published\" date and an author"
        (dpBody (distill True (createInput commentDateAuthor)) @?= expectedDateAuthor)
    commentDateAuthor =
      commentNoDateNoAuthor
        { Atom.entryPublished = Just "2019-01-02T03:04:05Z",
          Atom.entryAuthors = [Atom.nullPerson {Atom.personName = "John Doe"}]
        }
    expectedDateAuthor =
      "<p>Hello, world!</p>\n\n\
      \<h3 id='hakyll-convert-comments-title'>Comments</h3>\n\
      \<div class='hakyll-convert-comment'>\n\
      \<p class='hakyll-convert-comment-date'>On 2019-01-02T03:04:05Z, John Doe wrote:</p>\n\
      \<div class='hakyll-convert-comment-body'>\n\
      \<p>Nice post.</p>\n\
      \</div>\n\
      \</div>"

enumeratesAllCommentAuthors :: TestTree
enumeratesAllCommentAuthors =
  testCase
    "enumerates all authors of a multi-author comment"
    (dpBody (distill True input) @?= expected)
  where
    input =
      FullPost
        { fpPost = entry,
          fpComments = [comment],
          fpUri = "https://example.com/feed"
        }
    entry =
      ( Atom.nullEntry
          "https://example.com/entry"
          (Atom.TextString "First post")
          "2003-12-13T18:30:02Z"
      )
        { Atom.entryContent = Just (Atom.HTMLContent "<p>Hello, world!</p>"),
          Atom.entryPublished = Just "2003-12-13T18:30:02Z"
        }
    comment =
      ( Atom.nullEntry
          "https://example.com/entry#comment1"
          (Atom.TextString "Nice")
          "2103-05-11T18:37:49Z"
      )
        { Atom.entryContent = Just (Atom.HTMLContent "<p>Nice post.</p>"),
          Atom.entryAuthors =
            [ Atom.nullPerson {Atom.personName = "First Author"},
              Atom.nullPerson {Atom.personName = "Second Author"}
            ]
        }

    expected =
      "<p>Hello, world!</p>\n\n\
      \<h3 id='hakyll-convert-comments-title'>Comments</h3>\n\
      \<div class='hakyll-convert-comment'>\n\
      \<p class='hakyll-convert-comment-date'>On unknown date, First Author Second Author wrote:</p>\n\
      \<div class='hakyll-convert-comment-body'>\n\
      \<p>Nice post.</p>\n\
      \</div>\n\
      \</div>"

nullDistilledPost :: DistilledPost
nullDistilledPost =
  DistilledPost
    { dpUri = "",
      dpBody = "",
      dpTitle = Nothing,
      dpTags = [],
      dpCategories = [],
      dpDate = fromGregorian 2003 12 13 18 30 2
    }

errorsOnNonHtmlPost :: TestTree
errorsOnNonHtmlPost =
  expectFail $
    testCase
      "`error`s if post has non-HTML body"
      (distill False input @?= nullDistilledPost)
  where
    input =
      FullPost
        { fpPost = entry,
          fpComments = [],
          fpUri = "https://example.com/feed"
        }
    entry =
      ( Atom.nullEntry
          "https://example.com/entry"
          (Atom.TextString "First post")
          "2003-12-13T18:30:02Z"
      )
        { Atom.entryContent = Just (Atom.TextContent "oops, this will fail")
        }

errorsOnNonHtmlComment :: TestTree
errorsOnNonHtmlComment =
  expectFail $
    testCase
      "`error`s if comment has non-HTML body"
      (distill False input @?= nullDistilledPost)
  where
    input =
      FullPost
        { fpPost = entry,
          fpComments = [comment],
          fpUri = "https://example.com/feed"
        }
    entry =
      ( Atom.nullEntry
          "https://example.com/entry"
          (Atom.TextString "First post")
          "2003-12-13T18:30:02Z"
      )
        { Atom.entryContent = Just (Atom.TextContent "testing...")
        }

    comment =
      ( Atom.nullEntry
          "https://example.com/entry#2"
          (Atom.TextString "test comment")
          "2003-12-13T18:30:02Z"
      )
        { Atom.entryContent = Just (Atom.TextContent "oops, this will fail")
        }

turnsIncorrectDatesIntoEpochStart :: TestTree
turnsIncorrectDatesIntoEpochStart =
  testGroup
    "turns incorrect \"published\" dates into Unix epoch start date"
    [ testCase (T.unpack date) (dpDate (distill False (createInput date)) @?= expected)
      | date <-
          [ "First of April",
            "2020.07.30",
            "2020.07.30 00:01",
            "2020-07-30 00:01",
            "2020-07-30T00:01",
            "2020-07-30T00:01Z",
            "Sun, 31st July, 2020"
          ]
    ]
  where
    createInput date =
      FullPost
        { fpPost = createEntry date,
          fpComments = [],
          fpUri = "https://example.com/feed"
        }
    createEntry date =
      ( Atom.nullEntry
          "https://example.com/entry"
          (Atom.TextString "First post")
          date
      )
        { Atom.entryContent = Just (Atom.HTMLContent ""),
          Atom.entryPublished = Just date
        }

    expected = fromGregorian 1970 1 1 0 0 0

parsesDates :: TestTree
parsesDates =
  testGroup
    "parses \"published\" dates"
    [ testCase (T.unpack dateStr) (dpDate (distill False (createInput dateStr)) @?= expected)
      | (dateStr, expected) <-
          [ ("2020-07-30T15:50:21Z", fromGregorian 2020 7 30 15 50 21),
            ("1015-02-18T01:04:13Z", fromGregorian 1015 2 18 1 4 13),
            ("2020-07-30T15:50:21+0000", fromGregorian 2020 7 30 15 50 21),
            ("1015-02-18T01:04:13+0000", fromGregorian 1015 2 18 1 4 13),
            ("1015-02-18T01:04:13+0001", fromGregorian 1015 2 18 1 (4 - 1) 13),
            ("1015-02-18T01:04:13-0001", fromGregorian 1015 2 18 1 (4 + 1) 13),
            ("1015-02-18T01:04:13+0100", fromGregorian 1015 2 18 (1 - 1) 4 13),
            ("1015-02-18T01:04:13-0100", fromGregorian 1015 2 18 (1 + 1) 4 13)
          ]
    ]
  where
    createInput date =
      FullPost
        { fpPost = createEntry date,
          fpComments = [],
          fpUri = "https://example.com/feed"
        }
    createEntry date =
      ( Atom.nullEntry
          "https://example.com/entry"
          (Atom.TextString "First post")
          date
      )
        { Atom.entryContent = Just (Atom.HTMLContent ""),
          Atom.entryPublished = Just date
        }

extractsPostTags :: TestTree
extractsPostTags =
  testCase
    "extracts post's tags"
    (dpTags (distill False input) @?= expected)
  where
    input =
      FullPost
        { fpPost = entry,
          fpComments = [],
          fpUri = "https://example.com/feed"
        }
    entry =
      ( Atom.nullEntry
          "https://example.com/entry"
          (Atom.TextString "First post")
          "2003-12-13T18:30:02Z"
      )
        { Atom.entryContent = Just (Atom.HTMLContent ""),
          Atom.entryCategories =
            [ Atom.newCategory "first tag",
              Atom.newCategory "second tag",
              Atom.newCategory "third tag",
              (Atom.newCategory "blogger category (should be ignored)")
                { Atom.catScheme = Just "http://schemas.google.com/g/2005#kind"
                }
            ]
        }

    expected = ["first tag", "second tag", "third tag"]
