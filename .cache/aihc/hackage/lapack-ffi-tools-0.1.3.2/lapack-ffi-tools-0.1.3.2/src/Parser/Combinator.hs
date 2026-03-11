module Parser.Combinator where

import qualified Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec (Parser, (<|>))

import Control.Monad.HT (void)
import Control.Monad (liftM2)


space :: Parser ()
space =
   void (Parsec.char ' ')
   <|>
   Parsec.try (Parsec.newline >> continue)

newline :: Parser ()
newline = do
   void Parsec.newline
   Parsec.notFollowedBy continue

continue :: Parser ()
continue = do
   Parsec.skipMany $ Parsec.char ' '
   void $ Parsec.char '$'

spaces :: Parser ()
spaces = Parsec.skipMany space

garbage :: Parser ()
garbage = Parsec.skipMany $ Parsec.noneOf "\n"

infixl 1 `followedBy`

followedBy :: Parser a -> Parser () -> Parser a
followedBy p end = do a <- p; end; return a

oneLine :: Parser a -> Parser a
oneLine p = p `followedBy` newline

lexeme :: Parser a -> Parser a
lexeme p = p `followedBy` spaces

identifier :: Parser String
identifier =
   lexeme $
   liftM2 (:) Parsec.letter (Parsec.many (Parsec.alphaNum <|> Parsec.char '_'))

commaSep :: Parser a -> Parser [a]
commaSep = flip Parsec.sepBy (lexeme $ Parsec.char ',')

commaSep1 :: Parser a -> Parser [a]
commaSep1 = flip Parsec.sepBy1 (lexeme $ Parsec.char ',')

parens :: Parser a -> Parser a
parens = Parsec.between (lexeme $ Parsec.char '(') (lexeme $ Parsec.char ')')

keyword :: String -> Parser ()
keyword str = void $ lexeme $ Parsec.string str
