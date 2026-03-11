{-# LANGUAGE
      OverloadedStrings,
      StrictData
  #-}

module HaskellLex where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

import Text (splits)


-- We have two basic problems:

-- 1. splitting haskell code based on the presence of {{ }}
--    processing template directives
--    splitting into chunks according to the need for line pragmas
--
-- 2. splitting off haskell code according to user-chosen template symbols
--    lexing the haskell contents in order to prevent injection problems


-- We don't want to trigger comments in strings literals or comments,
-- so we track them. In order to track string literals, we must also
-- track character literals (since '"' does not start or end a string
-- literal).
data SourceState =
  InCode |
  InCodeOpenBrace |
  InCodeMinus |
  InCharStart |
  InCharEnd |
  InString |
  InStringEscape |
  InLineComment |
  InBlockComment Int |
  InBlockCommentMinus Int |
  InBlockCommentOpenBrace Int
  deriving (Eq, Show)


evolveSource :: SourceState -> Char -> SourceState
evolveSource InCode '{' = InCodeOpenBrace
evolveSource InCode '\'' = InCharStart
evolveSource InCode '"' = InString
evolveSource InCode '-' = InCodeMinus
evolveSource InCode _ = InCode
evolveSource InCodeOpenBrace '-' = InBlockComment 0
evolveSource InCodeOpenBrace c = evolveSource InCode c
evolveSource InCodeMinus '-' = InLineComment
evolveSource InCodeMinus c = evolveSource InCode c
evolveSource InCharStart '\\' = InCharStart
evolveSource InCharStart _ = InCharEnd
evolveSource InCharEnd _ = InCode
evolveSource InString '"' = InCode
evolveSource InString '\\' = InStringEscape
evolveSource InString _ = InString
evolveSource InStringEscape _ = InString
evolveSource InLineComment '\n' = InCode
evolveSource InLineComment _ = InLineComment
evolveSource (InBlockComment d) '-' = InBlockCommentMinus d
evolveSource (InBlockComment d) '{' = InBlockCommentOpenBrace d
evolveSource (InBlockComment d) _ = InBlockComment d
evolveSource (InBlockCommentMinus 0) '}' = InCode
evolveSource (InBlockCommentMinus d) '}' = InBlockComment (d-1)
evolveSource (InBlockCommentMinus d) '-' = InBlockCommentMinus d
evolveSource (InBlockCommentMinus d) _ = InBlockComment d
evolveSource (InBlockCommentOpenBrace d) '-' = InBlockComment (d+1)
evolveSource (InBlockCommentOpenBrace d) '{' = InBlockCommentOpenBrace d
evolveSource (InBlockCommentOpenBrace d) _ = InBlockComment d


evolveLine :: Int -> Char -> Int
evolveLine n '\n' = n+1
evolveLine n _ = n


data ReadStatus = ReadStatus {
  char :: Char,
  before :: Text,
  after :: Text,
  line :: Int,
  sourceState :: SourceState
}


reading :: Int -> Text -> [ReadStatus]
reading n t = scanl step start $ splits t where

  start = ReadStatus {
    char = '?',
    before = "",
    after = t,
    line = n,
    sourceState = InCode
  }

  step s (c,(t1,t2)) = ReadStatus {
    char = c,
    before = t1,
    after = t2,
    line = evolveLine (line s) c,
    sourceState = evolveSource (sourceState s) c
  }


splitCodeOn :: Text -> Text -> Maybe (Text, Text)
splitCodeOn pat src = f InCode steps where
  steps = zip3 (T.unpack src) (T.inits src) (T.tails src)
  f _ [] = Nothing
  f InCode ((c,u,v):l) = case T.stripPrefix pat v of
    Nothing -> f (evolveSource InCode c) l
    Just v' -> Just (u,v')
  f s ((c,_,_):l) = f (evolveSource s c) l


-- TODO Adapt this code to check that Haskell code in templates is
-- balanced and ends in code (to protect against injection errors)

data ParenStatus = ParenStatus {
  parenDepth :: Int,
  parenBadness :: Int
} deriving (Eq)

instance Semigroup ParenStatus where
  ParenStatus d1 b1 <> ParenStatus d2 b2 = ParenStatus (d1+d2) (b1 `min` (d1+b2))

instance Monoid ParenStatus where
  mempty = ParenStatus 0 0

-- We assume this string begins in Haskell code: check that it then
-- ends in Haskell code and has balanced parentheses
balancedCode :: Text -> Bool
balancedCode = f . T.foldl' g (InCode, mempty) where
  f = (== (InCode, mempty))
  g (s,p) c = (evolveSource s c, h s p c)
  h InCode p '(' = p <> ParenStatus 1 0
  h InCode p ')' = p <> ParenStatus (-1) (-1)
  h _ p _ = p
