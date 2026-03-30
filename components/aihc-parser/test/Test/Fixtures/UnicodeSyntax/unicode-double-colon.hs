{- ORACLE_TEST
id: unicode-double-colon
category: types
expected: pass
-}
{-# LANGUAGE UnicodeSyntax #-}

module UnicodeSyntaxDoubleColon where

identity :: Int -> Int
identity x = x

-- Same with Unicode
identityU ∷ Int → Int
identityU x = x
