{-|
Parse a string into our custom tag soup data structure.

The parser works only on proper Unicode texts.
That is, you must have decoded it before,
e.g. using decoding functions from hxt or encoding package.
'Text.HTML.Tagchup.Process.findMetaEncoding'
can assist you retrieving the character set encoding
from meta information of the document at hand.
-}
module Text.HTML.Tagchup.Parser (
    CharType,
    runSoup, runSoupWithPositions, runSoupWithPositionsName,
    runTag, runInnerOfTag,
  ) where

import Text.HTML.Tagchup.Parser.Tag
   (CharType, StringType, parsePosTag, parsePosTagMergeWarnings, )

import Text.HTML.Tagchup.Parser.Combinator (manyS, )

import qualified Text.HTML.Tagchup.Parser.Combinator as Parser
import qualified Text.HTML.Tagchup.Parser.Stream as Stream

import qualified Text.HTML.Tagchup.PositionTag as PosTag
import qualified Text.HTML.Tagchup.Tag         as Tag
import qualified Text.XML.Basic.Name        as Name

import Control.Monad (liftM, )

import Data.Maybe (fromMaybe, )

-- import qualified Numeric


-- * run parser in several ways

{- |
Parse a single tag, throws an error if there is a syntax error.
This is useful for parsing a match pattern.
-}
runTag ::
   (Stream.C source, StringType sink, Show sink,
    Name.Attribute name, Name.Tag name, Show name) =>
   source -> Tag.T name sink
runTag str =
   let makeError msg =
          error $ "runTag: " ++ msg
       ((openTag, closeTag), warnings) =
          fromMaybe (makeError "no parse at all") $
          Parser.run "pattern string" parsePosTag str
   in  if not (null warnings)
         then
           makeError
              (unlines $ "parsing results in" : map show warnings)
         else
           case closeTag of
              Nothing -> PosTag.tag_ openTag
              _ -> makeError "self-closing tag not supported"

{- |
Parse the inner of a single tag.
That is, @runTag \"\<bla\>\"@ is the same as @runInnerOfTag \"bla\"@.
-}
runInnerOfTag ::
   (StringType sink, Show sink,
    Name.Attribute name, Name.Tag name, Show name) =>
   String -> Tag.T name sink
runInnerOfTag str = runTag $ "<"++str++">"



runSoupWithPositionsName ::
   (Stream.C source, StringType sink,
    Name.Attribute name, Name.Tag name) =>
   FilePath -> source -> [PosTag.T name sink]
runSoupWithPositionsName fileName =
   PosTag.concatTexts .
   Parser.runIdentity .
   Parser.eval fileName
      (liftM concat $ manyS parsePosTagMergeWarnings)


-- | Parse an HTML document to a list of 'Tag.T'.
-- Automatically expands out escape characters.
runSoupWithPositions ::
   (Stream.C source, StringType sink,
    Name.Attribute name, Name.Tag name) =>
   source -> [PosTag.T name sink]
runSoupWithPositions =
   runSoupWithPositionsName "input"

-- | Like 'runSoupWithPositions' but hides source file positions.
runSoup ::
   (Stream.C source, StringType sink,
    Name.Attribute name, Name.Tag name) =>
   source -> [Tag.T name sink]
runSoup = map PosTag.tag_ . runSoupWithPositions
