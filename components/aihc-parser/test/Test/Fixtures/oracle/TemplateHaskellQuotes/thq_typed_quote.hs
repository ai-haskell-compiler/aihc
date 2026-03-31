{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskellQuotes #-}
module THQ_Typed_Quote where

tq = [|| 1 + 2 ||]
tqe = [e|| 1 + 2 ||]