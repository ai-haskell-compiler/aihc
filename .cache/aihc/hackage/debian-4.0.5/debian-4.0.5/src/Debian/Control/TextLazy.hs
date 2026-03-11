{-# LANGUAGE FlexibleInstances, OverloadedStrings, ScopedTypeVariables, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}
module Debian.Control.TextLazy
    ( -- * Types
      Control'(..)
    , Paragraph'(..)
    , Field'(..)
    , Control
    , Paragraph
    , Field
    -- , ControlParser
    , ControlFunctions(..)
    -- * Control File Parser
    -- , pControl
    -- * Helper Functions
    , mergeControls
    , fieldValue
    , removeField
    , prependFields
    , appendFields
    , renameField
    , modifyField
    , raiseFields
    , decodeControl
    , decodeParagraph
    , decodeField
    ) where

import qualified Data.ByteString.Char8 as B
import Data.Char (toLower, chr)
import Data.List (find)
import qualified Data.Text.Lazy as T (Text, pack, unpack, map, dropAround, {-reverse,-} fromStrict, toStrict)
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
--import Data.Text.IO as T (readFile)
import qualified Debian.Control.ByteString as B
--import Text.Parsec.Error (ParseError)
--import Text.Parsec.Text (Parser)
--import Text.Parsec.Prim (runP)
import Debian.Control.Common (ControlFunctions(parseControlFromFile, parseControlFromHandle, parseControl, lookupP, stripWS, asString),
                              Control'(Control), Paragraph'(Paragraph), Field'(Field, Comment),
                              mergeControls, fieldValue, removeField, prependFields, appendFields,
                              renameField, modifyField, raiseFields, protectFieldText')

-- | @parseFromFile p filePath@ runs a string parser @p@ on the
-- input read from @filePath@ using 'Prelude.readFile'. Returns either a 'ParseError'
-- ('Left') or a value of type @a@ ('Right').
--
-- >  main    = do{ result <- parseFromFile numbers "digits.txt"
-- >              ; case result of
-- >                  Left err  -> print err
-- >                  Right xs  -> print (sum xs)
-- >              }
{-
parseFromFile :: Parser a -> String -> IO (Either ParseError a)
parseFromFile p fname
    = do input <- T.readFile fname `E.catch` (\ (_ :: E.SomeException) -> B.readFile fname >>= return . decode)
         return (runP p () fname input)
-}

type Field = Field' T.Text
type Control = Control' T.Text
type Paragraph = Paragraph' T.Text

decodeControl :: B.Control -> Control
decodeControl (B.Control paragraphs) = Control (map decodeParagraph paragraphs)

decodeParagraph :: B.Paragraph -> Paragraph
decodeParagraph (B.Paragraph s) = B.Paragraph (map decodeField s)

decodeField :: Field' B.ByteString -> Field' T.Text
decodeField (B.Field (name, value)) = Field (decode name, decode value)
decodeField (B.Comment s) = Comment (decode s)

decode :: B.ByteString -> T.Text
decode = T.fromStrict . decodeUtf8With (\ _ w -> fmap (chr . fromIntegral) w)

-- * ControlFunctions

instance ControlFunctions T.Text where
    parseControlFromFile filepath =
        -- The ByteString parser is far more efficient than the Text
        -- parser.  By calling decodeControl we tell the compiler to
        -- use it instead.
        parseControlFromFile filepath >>= return . either Left (Right . decodeControl)
    parseControlFromHandle sourceName handle =
        parseControlFromHandle sourceName handle >>= return . either Left (Right . decodeControl)
    parseControl sourceName c =
        -- Warning: This is very slow, it does a utf8 round trip
        either Left (Right . decodeControl) (parseControl sourceName (encodeUtf8 (T.toStrict c)))
    lookupP fieldName (Paragraph paragraph) =
        find (hasFieldName (map toLower fieldName)) paragraph
        where hasFieldName :: String -> Field' T.Text -> Bool
              hasFieldName name (Field (fieldName',_)) = T.pack name == T.map toLower fieldName'
              hasFieldName _ _ = False
    stripWS = T.dropAround (`elem` (" \t" :: String))
      -- T.strip would also strip newlines
    protectFieldText = protectFieldText'
    asString = T.unpack

-- * Control File Parser
{-
-- type ControlParser = GenParser T.Text
type ControlParser a = Parsec T.Text () a

-- |A parser for debian control file. This parser handles control files
-- that end without a newline as well as ones that have several blank
-- lines at the end. It is very liberal and does not attempt validate
-- the fields in any way. All trailing, leading, and folded whitespace
-- is preserved in the field values. See 'stripWS'.
pControl :: ControlParser Control
pControl =
    do many $ char '\n'
       sepEndBy pParagraph pBlanks >>= return . Control

pParagraph :: ControlParser Paragraph
pParagraph = many1 (pComment <|> pField) >>= return . Paragraph

-- |We are liberal in that we allow *any* field to have folded white
-- space, even though the specific restricts that to a few fields.
pField :: ControlParser Field
pField =
    do c1 <- noneOf "#\n"
       fieldName <-  many1 $ noneOf ":\n"
       char ':'
       fieldValue <- many fcharfws
       (char '\n' >> return ()) <|> eof
       return $ Field (T.cons c1 (T.pack fieldName), T.pack fieldValue)

pComment :: ControlParser Field
pComment =
    do char '#'
       text <- many (satisfy (not . ((==) '\n')))
       char '\n'
       return $ Comment (T.pack ("#" <> text <> "\n"))

fcharfws :: ControlParser Char
fcharfws = fchar <|> (try $ lookAhead (string "\n ") >> char '\n') <|> (try $ lookAhead (string "\n\t") >> char '\n') <|> (try $ lookAhead (string "\n#") >> char '\n')

fchar :: ControlParser Char
fchar = satisfy (/='\n')

_fws :: ControlParser T.Text
_fws =
    try $ do char '\n'
             ws <- many1 (char ' ')
             c <- many1 (satisfy (not . ((==) '\n')))
             return $ T.cons '\n' (T.pack ws <> T.pack c)

-- |We go with the assumption that 'blank lines' mean lines that
-- consist of entirely of zero or more whitespace characters.
pBlanks :: ControlParser T.Text
pBlanks =
    do s <- many1 (oneOf " \n")
       return . T.pack $ s
-}
