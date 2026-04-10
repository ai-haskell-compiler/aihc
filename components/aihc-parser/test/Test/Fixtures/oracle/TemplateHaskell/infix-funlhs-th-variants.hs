{- ORACLE_TEST pass -}
{- Test infix function definitions with TemplateHaskell -}
{-# LANGUAGE TemplateHaskell #-}
module InfixFunlhsThVariants where

-- Basic infix with operator symbol
infixl 6 <+>
(<+>) :: Int -> Int -> Int
x <+> y = x + y

-- Infix with multiple patterns after parenthesized operator
infixr 5 </>
(</>) :: Path b Dir -> Path Rel t -> Path b t
(</>) (Path a) (Path b) = Path (a ++ b)

-- Infix with wildcards
infix 4 `matches`
matches :: String -> String -> Bool
[] `matches` [] = True
_ `matches` _ = False

-- Multiple equations with infix
infixr 3 ***
(***) :: Int -> Int -> Int
x *** 0 = 0
x *** y = x + (x *** (y - 1))
