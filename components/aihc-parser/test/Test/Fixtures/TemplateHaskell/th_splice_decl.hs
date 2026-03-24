{-# LANGUAGE TemplateHaskell #-}
module TH_Splice_Decl where

$decl

$(makeLenses ''Foo)
