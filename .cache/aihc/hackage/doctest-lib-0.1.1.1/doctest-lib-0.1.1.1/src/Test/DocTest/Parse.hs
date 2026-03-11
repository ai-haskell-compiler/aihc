module Test.DocTest.Parse (
  DocTest(..),
  Expression,
  Interaction,
  parseComment,
  ) where

import Test.DocTest.Location (Located(Located), unLoc)
import Test.DocTest.Base

import Data.List (stripPrefix, isPrefixOf, tails)
import Data.Maybe (fromMaybe, isJust)
import Data.Char (isSpace)

import Control.Arrow (second)
import Control.Monad (msum)
import Control.Applicative ((<$>), (<|>))



data DocTest = Example Expression ExpectedResult | Property Expression
   deriving (Eq, Show)

type Expression = String

type Interaction = (Expression, ExpectedResult)

data Prompt = ExamplePrompt | PropPrompt

parseComment :: [Located pos String] -> [Located pos DocTest]
parseComment = go
  where
    examplePrompt :: String
    examplePrompt = ">>>"

    propPrompt :: String
    propPrompt = "prop>"

    maybePrompt ::
        Located pos String -> Maybe (String, (Prompt, Located pos String))
    maybePrompt (Located loc line) =
        (\(indentation, str) ->
            fmap ((,) indentation) $
            (,) ExamplePrompt . Located loc <$> stripPrefix examplePrompt str
            <|>
            (,) PropPrompt . Located loc <$> stripPrefix propPrompt str)
        .
        span isSpace
            $ line

    isClosingLine :: Located pos String -> Bool
    isClosingLine = isPrefixOf ":}" . dropWhile isSpace . unLoc

    isBlankLine :: Located pos String -> Bool
    isBlankLine  = null . dropWhile isSpace . unLoc

    isEndOfInteraction :: Located pos String -> Bool
    isEndOfInteraction x = isJust (maybePrompt x) || isBlankLine x

    go xs =
      case dropWhileNothing maybePrompt xs of
        Nothing -> []
        Just ((ind, (prompt, firstLine@(Located loc firstLineStr))), rest) ->
          let firstLineUnindented = dropWhile isSpace firstLineStr in
          case isPrefixOf ":{" firstLineUnindented of
            False -> cont prompt ind firstLine rest
            True ->
              case second (splitAt 1) $ break isClosingLine rest of
                (ys,(closing,zs)) ->
                    cont prompt ind
                        (Located loc $ unlines $
                            firstLineUnindented : map unLoc (ys++closing))
                        zs

    cont prompt ind expr@(Located loc exprStr) rest =
        case prompt of
            PropPrompt -> fmap Property expr : go rest
            ExamplePrompt ->
                let (ys,zs) = break isEndOfInteraction rest
                in  Located loc
                        (Example exprStr $ map mkExpectedLine $ unindent ind ys)
                    :
                    go zs


-- Cf. utility-ht:Data.List.HT
dropWhileNothing :: (a -> Maybe b) -> [a] -> Maybe (b, [a])
dropWhileNothing f =
   msum .
   map (\at -> case at of [] -> Nothing; a:as -> fmap (flip (,) as) (f a)) .
   tails


unindent :: String -> [Located pos String] -> [String]
unindent pre = map (tryStripPrefix pre . unLoc)

tryStripPrefix :: String -> String -> String
tryStripPrefix prefix ys = fromMaybe ys $ stripPrefix prefix ys

mkExpectedLine :: String -> ExpectedLine
mkExpectedLine x = case x of
    "<BLANKLINE>" -> ExpectedLine [LineChunk ""]
    "..." -> WildCardLine
    _ -> ExpectedLine $ mkLineChunks x

mkLineChunks :: String -> [LineChunk]
mkLineChunks = finish . foldr go (0, [], [])
  where
    mkChunk :: String -> [LineChunk]
    mkChunk "" = []
    mkChunk x  = [LineChunk x]

    go :: Char -> (Int, String, [LineChunk]) -> (Int, String, [LineChunk])
    go '.' (count, acc, res) = if count == 2
          then (0, "", WildCardChunk : mkChunk acc ++ res)
          else (count + 1, acc, res)
    go c   (count, acc, res) = if count > 0
          then (0, c : replicate count '.' ++ acc, res)
          else (0, c : acc, res)
    finish (count, acc, res) = mkChunk (replicate count '.' ++ acc) ++ res
