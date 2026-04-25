{- ORACLE_TEST xfail aihc cpp preprocessing leaves C-style block comments in expanded source, so the parser sees '/*' that GHC strips on a second CPP pass -}
{-# LANGUAGE CPP #-}
module MacroExpansionBlockCommentLiteral where

#define HET 0x68657462 /* 'h' 'e' 't' 'b' */

f x = case x of
  HET -> ()
  _ -> ()
