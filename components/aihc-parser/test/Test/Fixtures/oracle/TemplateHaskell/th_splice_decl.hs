{- ORACLE_TEST xfail TemplateHaskell top-level splice -}
{-# LANGUAGE TemplateHaskell #-}
module TH_Splice_Decl where

$decl

$(makeLenses ''Foo)