{- |
Module      : Text.XML.HXT.CSS.Parser

Stability   : stable

A parser for CSS selectors.
-}

module Text.XML.HXT.CSS.Parser
    ( safeParseCSS
    , parseCSS
    ) where

import Text.Parsec hiding (spaces)
import Text.Parsec.String
import Control.Monad
import Control.Applicative hiding (many, (<|>))
import Data.Char
import Prelude

import Text.XML.HXT.CSS.TypeDefs

-- | Parse a string to an AST. If the parser fails, it returns a left value
-- with an error message.
safeParseCSS :: String -> Either String SelectorsGroup
safeParseCSS s =
    case parse (spaces *> selectorsGroup <* eof) "" s of
        Right sel -> Right sel
        Left msg -> Left $ "Invalid CSS selector " ++
            show s ++ ": " ++ show msg

-- | Like 'safeParseCSS', but calls 'error' if given an invalid CSS
-- selector.
parseCSS :: String -> SelectorsGroup
parseCSS = either error id . safeParseCSS

selectorsGroup :: Parser SelectorsGroup
selectorsGroup = SelectorsGroup <$>
    selector `sepBy1` (spaces >> char ',' >> spaces)

selector :: Parser Selector
selector = do
    sss <- simpleSelectorSeq
    choice
        [ try $ Child sss      <$> (spaces *> char '>' *> spaces  *> selector)
        , try $ AdjSibling sss <$> (spaces *> char '+' *> spaces  *> selector)
        , try $ FolSibling sss <$> (spaces *> char '~' *> spaces  *> selector)
        , try $ Descendant sss <$> (                      spaces1 *> selector)
        , return (Selector sss)
        ]

simpleSelectorSeq :: Parser SimpleSelectorSeq
simpleSelectorSeq =
    SimpleSelectorSeq <$> (seq1 <|> seq2)
  where
    seq1 = (:) <$> (typeSelector <|> universalSelector) <*> many part
    seq2 = many1 part
    part = choice [idSelector, classSelector, attrSelector
                  , negation, pseudo]

universalSelector :: Parser SimpleSelector
universalSelector = UniversalSelector <$ char '*'

typeSelector :: Parser SimpleSelector
typeSelector = TypeSelector <$> ident

idSelector :: Parser SimpleSelector
idSelector = IdSelector <$> (char '#' *> many1 nmchar)

classSelector :: Parser SimpleSelector
classSelector = ClassSelector <$> (char '.' *> ident)

attrSelector :: Parser SimpleSelector
attrSelector = do
    void $ char '['
    spaces
    attr <- ident
    attrTest <- option AttrExists $ do
        op <- choice
            [ AttrPrefix     <$ whole "^="
            , AttrSuffix     <$ whole "$="
            , AttrSubstr     <$ whole "*="
            , AttrEq         <$ char '='
            , AttrContainsSp <$ whole "~="
            , AttrBeginHy    <$ whole "|="
            ]
        spaces
        val <- ident <|> stringLit
        spaces
        return (op val)
    spaces
    void $ char ']'
    return $ AttrSelector attr attrTest

negation :: Parser SimpleSelector
negation = Negation <$> (notP *> spaces *> arg <* spaces <* char ')')
  where
    notP = try $ char ':' *> stringCI "not" *> char '('
    arg = choice [typeSelector, universalSelector, idSelector
                 , classSelector, attrSelector, pseudo]

-- does not much :not(..)
pseudo :: Parser SimpleSelector
pseudo = do
    void $ char ':'
    s <- ident
    case () of
        () | Just p <- findPseudoClass s ->
            return (Pseudo p)
        () | Just p <- findPseudoNthClass s -> do
            arg <- char '(' *> nth <* char ')'
            return (PseudoNth $ p arg)
        () | map toLower s == "not" ->
            fail "negation is not allowed here"
        () -> fail $ "'" ++ s ++ "' is not a valid pseudo-class"

nth :: Parser Nth
nth = spaces *> p <* spaces
  where
    p = choice
        [ try fullNth
        , Nth 0 <$> (signOpt <*> integer)
        , Odd  <$ stringCI "odd"
        , Even <$ stringCI "even"
        ]
    fullNth = do
        a <- signOpt <*> option 1 integer
        void $ charCI 'n'
        b <- option 0 $ do
            spaces
            sign <*> (spaces *> integer)
        return (Nth a b)
    sign = id <$ char '+' <|> negate <$ char '-'
    signOpt = option id sign

--------------------------------------------------------------------------------
-- auxiliary parsers

ident :: Parser String
ident = (:) <$> nmstart <*> many nmchar

nmstart :: Parser Char
nmstart = satisfy p <|> nonascii {- <|> escape -} <?> "nmstart"
  where
    p c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_'

nmchar :: Parser Char
nmchar = satisfy p <|> nonascii {- <|> escape -} <?> "nmchar"
  where
    p c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') ||
        isDigit c || elem c "_-"

stringLit :: Parser String
stringLit = string1 <|> string2
  where
    string1 =
        char '"' *>
            many (noneOf "\n\r\f\\\"" <|> nl <|> nonascii {- <|> escape -})
                <* char '"'
    string2 =
        char '\'' *>
            many (noneOf "\n\r\f\\'"  <|> nl <|> nonascii {- <|> escape -})
                <* char '\''

nonascii :: Parser Char
nonascii = satisfy (> '\DEL') <?> "nonascii"

{-escape :: Parser Char-}
{-escape = mzero -- not supported by any major browser-}

{-unicode :: Parser Char-}
{-unicode = mzero -- not supported by any major browser-}

nl :: Parser Char
nl = choice
    [ void $ char '\n'
    , void $ char '\r' >> optionMaybe (char '\n')
    , void $ char '\f'
    ] >> return '\n'

integer :: Read a => Parser a
integer = read <$> many1 digit

spaces :: Parser ()
spaces = skipMany (oneOf " \t\r\n\f") <?> "white space"

spaces1 :: Parser ()
spaces1 = skipMany1 (oneOf " \t\r\n\f") <?> "white space"

whole :: String -> Parser String
whole = try . string

stringCI :: String -> Parser String
stringCI (c : cs) = (:) <$> charCI c <*> stringCI cs
stringCI [] = return []

charCI :: Char -> Parser Char
charCI c
    | cL == cU = char c
    | otherwise = char cL <|> char cU
  where
    cL = toLower c
    cU = toUpper c
