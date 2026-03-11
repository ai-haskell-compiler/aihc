-- | Parser for DIMACS CNF format
--
-- explained in e.g. https://jix.github.io/varisat/manual/0.2.0/formats/dimacs.html and
-- https://users.aalto.fi/~tjunttil/2021-DP-AUT/notes-sat/solving.html#the-dimacs-cnf-file-format
--
{-# LANGUAGE CPP #-}
module PureSAT.DIMACS (
    parseDimacsFile,
    parseDimacs,
    demo,
) where

import Control.Applicative ((<|>))
import Data.Foldable (foldl')
import Data.Maybe (catMaybes)

#if MIN_VERSION_parsec(3,1,17)
import Control.Exception (throwIO)
#endif

import qualified Text.Parsec as P
import qualified Text.Parsec.ByteString as P
import qualified Data.ByteString as BS

import Control.Monad (forM_, forM)
import Control.Monad.ST (runST)
import qualified PureSAT.Main as PureSAT
import PureSAT.Prim

demo :: [[Int]] -> [Int]
demo clauses = runST $ do
    s <- PureSAT.newSolver

    -- create literal
    literals <- newPrimArray maxLiteral
    forM_ [1..maxLiteral] $ \i -> do
        l <- PureSAT.newLit s
        writePrimArray literals (i - 1) l

    -- addClauses
    forM_ clauses $ \clause -> do
        clause' <- forM clause $ \i -> do
            l <- readPrimArray literals (abs i - 1)
            return $ if i < 0 then PureSAT.neg l else l

        PureSAT.addClause s clause'

    -- solve
    res <- PureSAT.solve s

    -- read back the solution
    if res
    then do
        forM [1..maxLiteral] $ \i -> do
            l <- readPrimArray literals (i - 1)
            x <- PureSAT.modelValue s l
            return $ if x then i else negate i

    else return []
  where
    maxLiteral = foldl' (\acc clause -> foldl' (\x y -> max x (abs y)) acc clause) 0 clauses

-- | Parse DIMACS file
--
-- with parsec >= 3.1.17 the 'P.ParseError' is thrown on parse failure , otherwise @UserError@.
parseDimacsFile :: FilePath -> IO [[Int]]
parseDimacsFile fn = do
    contents <- BS.readFile fn

#if MIN_VERSION_parsec(3,1,17)
    either throwIO return $
#else
    either (fail . show) return $
#endif
        parseDimacs fn contents

parseDimacs :: FilePath -> BS.ByteString -> Either P.ParseError [[Int]]
parseDimacs fn contents = P.parse (skipSpace *> dimacs <* P.eof) fn contents

dimacs :: P.Parser [[Int]]
dimacs = do
    es <- P.many entry
    return (catMaybes es)

entry :: P.Parser (Maybe [Int])
entry = do
    c <- P.anyChar
    case c of
        'p' -> Nothing <$ skipLine -- we omit header
        'c' -> Nothing <$ skipLine
        '-' -> Just <$> negativeClause
        _ | '0' <= c && c <= '9'
            -> Just <$> positiveClause c
        _   -> fail $ "unexpecter character: " ++ show c

skipLine :: P.Parser ()
skipLine = aux <|> P.eof
  where
    aux = do
        c <- P.anyChar
        if c == '\n'
        then skipSpace
        else skipLine

skipSpace :: P.Parser ()
skipSpace = P.skipMany $ P.satisfy (\c -> c == ' ' || c == '\n' || c == '\r' || c == '\t')

negativeClause :: P.Parser [Int]
negativeClause = do
    l <- variable
    if l == 0
    then return []
    else go (negate l :)
  where
    go :: ([Int] -> [Int]) -> P.Parser [Int]
    go !acc = do
        l <- literal
        if l == 0
        then return (acc [])
        else go (acc . (l :))

positiveClause :: Char -> P.Parser [Int]
positiveClause d = do
    l <- variable' d
    if l == 0
    then return []
    else go (l :)
  where
    go :: ([Int] -> [Int]) -> P.Parser [Int]
    go !acc = do
        l <- literal
        if l == 0
        then return (acc [])
        else go (acc . (l :))

literal :: P.Parser Int
literal = do
    c <- P.anyChar
    case c of
        '-' -> negate <$> literal
        _ | '0' <= c && c <= '9'
            -> variable' c
        _   -> fail $ "unexpecter character: " ++ show c

variable :: P.Parser Int
variable = do
    d <- P.satisfy (\x -> '0' <= x && x <= '9')
    variable' d

variable' :: Char -> P.Parser Int
variable' d = do
    ds <- P.many $ P.satisfy (\x -> '0' <= x && x <= '9')
    skipSpace
    return $ foldl' (\acc x -> acc * 10 + toInt x) 0 (d:ds)
  where
    toInt :: Char -> Int
    toInt x = fromEnum x - fromEnum '0'
