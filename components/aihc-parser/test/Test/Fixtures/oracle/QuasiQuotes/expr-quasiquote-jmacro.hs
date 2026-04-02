{- ORACLE_TEST pass -}
{-# LANGUAGE QuasiQuotes #-}
module ExprQuasiQuoteJMacro where

-- Regression test for quasi-quote expressions in equation right-hand sides
-- (e.g. happstack-jmacro style)
scoped :: a -> b
scoped js = [jmacro| (function { `(js)`; })(); |]
