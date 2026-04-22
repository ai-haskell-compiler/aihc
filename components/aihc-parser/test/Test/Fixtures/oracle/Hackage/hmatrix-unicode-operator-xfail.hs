{- ORACLE_TEST xfail lexer rejects unicode em-dash as operator character -}
{-# LANGUAGE UnicodeSyntax #-}
module A where
(——) = undefined
