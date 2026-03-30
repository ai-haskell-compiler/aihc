{- ORACLE_TEST
id: decls-type-signature-context-multiline
category: declarations
expected: pass
reason: parser supports multiline parenthesized contexts in type signatures
-}
module DTypeSigCtxMultiline where

memo :: (Ord a)
     => (a -> b)
     -> (a -> b)
memo f = f
