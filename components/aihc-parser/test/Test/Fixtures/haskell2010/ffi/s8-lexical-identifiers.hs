{- ORACLE_TEST
id: ffi-s8-lexical-identifiers
category: ffi
expected: pass
reason: parser preserves ccall/stdcall as identifiers in non-foreign contexts
-}
module FfiS8LexicalIdentifiers where
ccall = 1
stdcall = 2
foreignName = ccall + stdcall
