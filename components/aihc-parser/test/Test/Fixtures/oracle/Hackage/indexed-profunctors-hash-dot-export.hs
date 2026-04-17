{- ORACLE_TEST xfail reason="overloaded labels lex #. as a label instead of an exported operator" -}
{-# LANGUAGE OverloadedLabels #-}

module IndexedProfunctorsHashDotExport
  ( (#.),
  )
where

(#.) x y = x
