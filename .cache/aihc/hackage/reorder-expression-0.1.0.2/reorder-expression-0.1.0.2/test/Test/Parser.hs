module Test.Parser
    ( Parser
    , expr
    ) where

import Text.Parsec
import Test.Expr
import Expression.Reorder (Fixity(..), Assoc(..))

type Parser = Parsec String Int

expr :: String -> LExpr
expr s = case runParser pExpr 0 "" s of
    Left e -> error $ "parse error" <> show e
    Right x -> x

getColumn :: Parser Int
getColumn = pred . sourceColumn . statePos <$> getParserState

nextIndex :: Parser OpIndex
nextIndex = OpIndex <$> (getState <* modifyState succ)

pExpr :: Parser LExpr
pExpr = pBinary <* eof

pBinary :: Parser LExpr
pBinary = do
    x <- pPostfix
    xs <- many $ do
        p1 <- getColumn
        a <- AssocLeft <$ char '<' <|> AssocRight <$ char '>' <|> AssocNone <$ char '='
        p <- read <$> many1 digit
        _ <- char '.'
        p2 <- getColumn
        spaces
        i <- nextIndex
        y <- pPostfix
        pure (Fixity a p @@ Pos p1 p2, i, y)
    pure $ foldl (\acc (op, i, y) -> ExprBinary i op acc y @@ acc <~> y) x xs

pPostfix :: Parser LExpr
pPostfix = do
    x <- pPrefix
    os <- many $ choice
        [ do
            p1 <- getColumn
            _ <- char '!'
            p <- read <$> many1 digit
            _ <- char '.'
            p2 <- getColumn
            spaces
            i <- nextIndex
            let op = Fixity AssocRight p @@ Pos p1 p2
            pure $ \acc -> ExprPostfix i op acc @@ acc <~> op
        , do
            p1 <- getColumn
            i <- nextIndex
            _ <- char '@'
            _ <- char '{'
            spaces
            xs <- pBinary `sepBy` (char ',' >> spaces)
            _ <- char '}'
            p2 <- getColumn
            spaces
            let op = xs @@ Pos p1 p2
            pure $ \acc -> ExprIndex i acc op @@ acc <~> op
        ]
    pure $ foldl (\acc f -> f acc) x os

pPrefix :: Parser LExpr
pPrefix = do
    os <- many $ do
        p1 <- getColumn
        _ <- char '~'
        p <- read <$> many1 digit
        _ <- char '.'
        p2 <- getColumn
        spaces
        i <- nextIndex
        pure (Fixity AssocLeft p @@ Pos p1 p2, i)
    x <- pAssoc <|> pGroup <|> pAtom
    pure $ foldl (\acc (op, i) -> ExprPrefix i op acc @@ op <~> acc) x (reverse os)

pAssoc :: Parser LExpr
pAssoc = do
    _ <- char '('
    spaces
    x <- pBinary
    _ <- char ')'
    spaces
    pure x

pGroup :: Parser LExpr
pGroup = do
    p1 <- getColumn
    _ <- char '{'
    spaces
    x <- pBinary
    _ <- char '}'
    p2 <- getColumn
    spaces
    pure $ ExprGroup x @@ Pos p1 p2

pAtom :: Parser LExpr
pAtom = do
    p1 <- getColumn
    x <- alphaNum
    p2 <- getColumn
    spaces
    pure $ ExprAtom x @@ Pos p1 p2
