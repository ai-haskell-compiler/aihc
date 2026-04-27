{- ORACLE_TEST xfail CTYPE pragma on data declaration is not preserved in roundtrip -}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE CApiFFI #-}

module CtypeDataTwoStrings where

data {-# CTYPE "termbox.h" "struct tb_cell" #-} Tb_cell = Tb_cell
