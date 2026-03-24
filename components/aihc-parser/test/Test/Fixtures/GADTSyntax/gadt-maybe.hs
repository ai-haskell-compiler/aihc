{-# LANGUAGE GADTSyntax #-}

module GadtMaybe where

data Maybe a where
    Nothing :: Maybe a
    Just    :: a -> Maybe a
