{- ORACLE_TEST pass -}
{-# LANGUAGE PatternSynonyms #-}
module PatternSynonymsExportDoubleDot
  ( X
      ( A,
        B,
        ..
      ),
  )
where

data X = A | B
