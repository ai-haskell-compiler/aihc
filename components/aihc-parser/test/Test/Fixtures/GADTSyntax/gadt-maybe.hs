{- ORACLE_TEST
id: gadt-maybe
category: declarations
expected: pass
-}
{-# LANGUAGE GADTSyntax #-}

module GadtMaybe where

data Maybe a where
    Nothing :: Maybe a
    Just    :: a -> Maybe a
