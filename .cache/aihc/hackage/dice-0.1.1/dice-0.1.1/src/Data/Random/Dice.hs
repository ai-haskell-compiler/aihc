{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Random.Dice where

import Data.Random
import Data.Random.Distribution.Uniform (integralUniform)
import System.Random.Stateful

import Control.Monad
import Control.Monad.Except
import Data.Functor.Identity
import Data.Ratio
import Data.List

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language
import Text.Printf

----------------------------------------------------------------
-- A simple expression language

data Expr a
    = Const   String   a
    | Plus   (Expr a) (Expr a)
    | Minus  (Expr a) (Expr a)
    | Times  (Expr a) (Expr a)
    | Divide (Expr a) (Expr a)
--    Repeat :: Expr Int -> Expr a -> Expr [a]
    deriving Show

instance Functor Expr where
    fmap f = foldExpr (\s x -> Const s (f x)) Plus Minus Times Divide

foldExpr c (+) (-) (*) (/) {-(#)-} = fold
    where
        fold (Const  s a) = c s a
        fold (Plus   x y) = fold x + fold y
        fold (Minus  x y) = fold x - fold y
        fold (Times  x y) = fold x * fold y
        fold (Divide x y) = fold x / fold y
--        fold (Repeat n y) = undefined # fold y

evalExprWithDiv :: (Num a, Monad m) => (a -> a -> m a) -> Expr a -> m a
evalExprWithDiv (/) = foldExpr (const return) (liftM2 (+)) (liftM2 (-)) (liftM2 (*)) divM -- (*)
    where
        divM x y = join (liftM2 (/) x y)

evalFractionalExpr :: (Eq a, Fractional a, MonadError String m) => Expr a -> m a
evalFractionalExpr = evalExprWithDiv divM
    where
        divM x 0 = throwError "Divide by zero!"
        divM x y = return (x / y)

evalIntegralExpr :: (Integral a, MonadError String m) => Expr a -> m a
evalIntegralExpr = evalExprWithDiv divM
    where
        divM x 0 = throwError "Divide by zero!"
        divM x y = return (div x y)

----------------------------------------------------------------
-- Commuting Expr with an arbitrary Monad m

commute con x y = do
    x <- runExpr x
    y <- runExpr y
    return (con x y)

runExpr :: Monad m => Expr (m a) -> m (Expr a)
runExpr (Const  s x) = x >>= return . Const s
runExpr (Plus   x y) = commute Plus   x y
runExpr (Minus  x y) = commute Minus  x y
runExpr (Times  x y) = commute Times  x y
runExpr (Divide x y) = commute Divide x y
-- runExpr (Repeat x y) = commute Repeat x y

----------------------------------------------------------------
-- Pretty-printing 'Expr's

fmtIntegralExpr :: (Show a, Integral a) => Expr a -> String
fmtIntegralExpr (Const _ e) = show e
fmtIntegralExpr e =
    showParen True (fmtExprPrec showScalarConst e 0)
    . showString " => "
    . showError (evalIntegralExpr e)
    $ ""

fmtIntegralListExpr :: (Show a, Integral a) => Expr [a] -> String
fmtIntegralListExpr (Const _ []) = "0"
fmtIntegralListExpr (Const _ [e]) = show e
fmtIntegralListExpr e =
    showParen True (fmtExprPrec showListConst e 0)
    . showString " => "
    . showError (evalIntegralExpr (fmap sum e))
    $ ""

fmtSimple :: (Integral a, Show a) => Expr [a] -> String
fmtSimple (Const _ []) = "0"
fmtSimple (Const _ [e]) = show e
fmtSimple e =
    showParen False (fmtExprPrec showSimpleListConst e 0)
    . showString " => "
    . showError (evalIntegralExpr (fmap sum e))
    $ ""

fmtSimpleRational :: Expr [Integer] -> String
fmtSimpleRational (Const _ []) = "0"
fmtSimpleRational (Const _ [e]) = show e
fmtSimpleRational e =
    showParen False (fmtExprPrec showSimpleListConst e 0)
    . showString " => "
    . showErrorWith showRationalWithDouble (evalFractionalExpr (fmap (fromInteger.sum) e))
    $ ""

showScalarConst d  v  p = showString d . showString "[" . shows v . showString "]"
showListConst   d  v  p = showString d . shows v

showSimpleConst showsPrec d [v] p = showsPrec p v
showSimpleConst showsPrec d  v  p = showParen (p > 0) (foldl1 (.) (intersperse (showChar '+') (map (showsPrec 6) v)))

showSimpleListConst :: Show a => String -> [a] -> Int -> ShowS
showSimpleListConst = showSimpleConst showsPrec

showSimpleRationalConst = showSimpleConst showRational

showError :: Show a => ExceptT String Identity a -> ShowS
showError = showErrorWith shows

showErrorWith f (ExceptT (Identity (Left  e))) = showString e
showErrorWith f (ExceptT (Identity (Right x))) = f x

showDouble :: Double -> ShowS
showDouble d = showString (trim (printf "%.04g" d))
    where trim = reverse . dropWhile (=='0') . reverse

showRational p d
    | denominator d == 1    = shows (numerator d)
    | otherwise             = showParen (p > 7)
        ( shows (numerator d)
        . showChar '/'
        . shows (denominator d)
        )

showRationalWithDouble d
    | isInt     = showRational 0 d
    | otherwise = showRational 0 d
                . showString " => "
                . showDouble (fromRational d)
    where isInt = denominator d == 1

fmtExprPrec :: (String -> a -> Int -> ShowS) -> Expr a -> Int -> ShowS
fmtExprPrec showConst e = foldExpr
    (\d v p -> showConst d v p)
    (\x y p -> showParen (p >  6) (x 6 . showString " + " . y 6))
    (\x y p -> showParen (p >  6) (x 6 . showString " - " . y 7))
    (\x y p -> showParen (p >  7) (x 7 . showString " * " . y 7))
    (\x y p -> showParen (p >  7) (x 7 . showString " / " . y 8))
    e

----------------------------------------------------------------
-- Rolling dice

rollEm :: String -> IO (Either ParseError String)
rollEm str = case parseExpr "rollEm" str of
    Left err    -> return (Left err)
    Right ex    -> do
        ex <- do
          g <- newIOGenM =<< newStdGen
          sampleFrom g (runExpr ex)
        return (Right (fmtSimpleRational (fmap (summarizeRollsOver 3) ex)))
--        return (Right (fmtIntegralListExpr ex))

summarizeRollsOver :: Num a => Int -> [a] -> [a]
summarizeRollsOver n xs
    | null (drop n xs)  = xs
    | otherwise         = [sum xs]

roll :: (Integral a, UniformRange a) => a -> a -> RVar [a]
roll count sides
    | count > 100   = do
        x <- stdNormal :: RVar Double
        let e = count*(sides+1)`div`2
            e' = fromIntegral (count*(sides+1)`mod`2)/2
            v = fromIntegral (sides*sides-1)/12
            x' = e' + x * sqrt (fromIntegral count * v)
        return [e + round x']
    | otherwise     = do
        ls <- replicateM (fromIntegral count) (integralUniform 1 sides)
        return ls

----------------------------------------------------------------
-- The parser

parseExpr :: (Integral a, UniformRange a) => String -> String -> Either ParseError (Expr (RVar [a]))
parseExpr src str = runParser expr False src str

-- a token-lexer thing
diceLang :: TokenParser st
diceLang = makeTokenParser
    (haskellStyle { reservedOpNames = ["*","/","+","-"{-,"#"-}] })

expr :: (Integral a, UniformRange a) => CharParser Bool (Expr (RVar [a]))
expr = do
    whiteSpace diceLang
    e <- term
    eof

    hasRolls <- getState
    if hasRolls
        then return e
        else fail "no rolls in expression"

term :: (Integral a, UniformRange a) => CharParser Bool (Expr (RVar [a]))
term = buildExpressionParser table primExp
    where   table =
                [ [binary "*" Times AssocLeft, binary "/" Divide AssocLeft ]
                , [binary "+" Plus  AssocLeft, binary "-" Minus  AssocLeft ]
--                , [binary "#" Repeat AssocRight]
                ]
            binary name fun assoc = Infix (do{ reservedOp diceLang name; return fun }) assoc

primExp :: (Integral a, UniformRange a) => CharParser Bool (Expr (RVar [a]))
primExp = try dieExp <|> numExp <|> parens diceLang term

dieExp :: (Integral a, UniformRange a) => CharParser Bool (Expr (RVar [a]))
dieExp = do
    (cStr, count) <- option ("", 1) number
    (sStr, sides) <- char 'd' >> positiveNumber
    setState True
    return (Const (cStr ++ 'd' : sStr) (roll (fromInteger count) (fromInteger sides)))

numExp :: Num a => CharParser st (Expr (RVar [a]))
numExp = do
    (str, num) <- number
    return (Const str (return [fromInteger num]))

number :: CharParser st (String, Integer)
number = do
    n <- many1 digit <?> "number"
    whiteSpace diceLang
    return (n, read n)

positiveNumber :: CharParser st (String, Integer)
positiveNumber = do
    (s,n) <- number
    guard (n>0)
    return (s,n)
