{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TransformListComp #-}
module M where

x = [[] | then [t| _ |] by []]
