{- ORACLE_TEST
id: th_splice_p
category: patterns
expected: xfail
reason: TemplateHaskell $p syntax
-}
{-# LANGUAGE TemplateHaskell #-}
module TH_Splice_P where

f $pat = True
g $(pat arg) = False
