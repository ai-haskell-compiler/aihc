{- ORACLE_TEST xfail - infix funlhs with TemplateHaskell causes parse error -}
{-# LANGUAGE TemplateHaskell #-}
module InfixFunlhsThCpp where
infixr 5 </>
(</>) :: Path b Dir -> Path Rel t -> Path b t
(</>) (Path a) (Path b) = Path (a ++ b)
