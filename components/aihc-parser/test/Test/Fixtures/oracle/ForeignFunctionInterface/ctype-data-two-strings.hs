{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE CApiFFI #-}

module CtypeDataTwoStrings where

data {-# CTYPE "termbox.h" "struct tb_cell" #-} Tb_cell = Tb_cell
