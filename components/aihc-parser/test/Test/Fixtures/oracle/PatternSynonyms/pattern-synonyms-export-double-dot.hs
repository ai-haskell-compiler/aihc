{- ORACLE_TEST xfail double-dot in export list with pattern synonyms -}
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
