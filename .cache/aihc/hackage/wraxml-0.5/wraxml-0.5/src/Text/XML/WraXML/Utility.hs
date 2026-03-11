module Text.XML.WraXML.Utility where

compose :: [a -> a] -> a -> a
compose = flip (foldr ($))
