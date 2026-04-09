{- ORACLE_TEST pass -}
{- Test infix function definitions with both TH and CPP -}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module InfixFunlhsThAndCpp where

infixr 5 </>
(</>) :: Path b Dir -> Path Rel t -> Path b t
(</>) (Path a) (Path b) = Path (a ++ b)

#ifdef DEBUG
debugInfo :: String
debugInfo = "debug mode"
#endif

-- Template Haskell splice alongside infix
showPath :: Path b t -> String
showPath = $(varE 'show)
