{- ORACLE_TEST pass -}
{-# LANGUAGE CPP #-}
module MacroExpansionBlockCommentLiteral where

#define HET 0x68657462 /* 'h' 'e' 't' 'b' */

f x = case x of
  HET -> ()
  _ -> ()
