module Main where

import Examples
import Issues
import Samples
import Zenacy.HTML.Internal.Buffer.Tests
import Zenacy.HTML.Internal.Entity.Tests
import Zenacy.HTML.Internal.HTML.Tests
import Zenacy.HTML.Internal.Image.Tests
import Zenacy.HTML.Internal.Lexer.Tests
import Zenacy.HTML.Internal.Oper.Tests
import Zenacy.HTML.Internal.Parser.Tests
import Zenacy.HTML.Internal.Query.Tests
import Zenacy.HTML.Internal.Token.Tests
import Zenacy.HTML.Internal.Trie.Tests
import Zenacy.HTML.Internal.Zip.Tests
import Test.Framework
  ( defaultMain
  )

main :: IO ()
main = defaultMain
  [ testBuffer
  , testEntity
  , testToken
  , testLexer
  , testParser
  , testHtml
  , testImage
  , testOper
  , testTrie
  , testZip
  , testQuery
  , testExamples
  , testSamples
  , testIssues
  ]
