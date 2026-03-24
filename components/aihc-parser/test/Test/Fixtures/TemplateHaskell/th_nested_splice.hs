{-# LANGUAGE TemplateHaskell #-}
module TH_Nested_Splice where

x = [| 1 + $y |]
z = [|| 1 + $$y ||]
