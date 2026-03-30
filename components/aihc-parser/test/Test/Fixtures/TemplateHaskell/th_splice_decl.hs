{- ORACLE_TEST
id: th_splice_decl
category: declarations
expected: xfail
reason: TemplateHaskell top-level splice
-}
{-# LANGUAGE TemplateHaskell #-}
module TH_Splice_Decl where

$decl

$(makeLenses ''Foo)
