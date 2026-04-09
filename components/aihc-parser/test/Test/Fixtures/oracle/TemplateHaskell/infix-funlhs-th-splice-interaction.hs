{- ORACLE_TEST pass -}
{- Ensure TH splices work correctly alongside infix definitions -}
{-# LANGUAGE TemplateHaskell #-}
module InfixFunlhsThSpliceInteraction where

import Language.Haskell.TH

-- Infix definition followed by splice
infixl 6 <#>
(<#>) :: Int -> Int -> Int
x <#> y = x * y

-- Splice that generates a definition
$(pure [])

-- Another infix after splice  
infixr 5 <+>
(<+>) :: String -> String -> String
x <+> y = x ++ y

-- Infix with pattern that could look like splice
infix 4 `app`
app :: (a -> b) -> a -> b
f `app` x = f x
