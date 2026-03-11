{- |
We provide a type class for tag and attribute names.
Instances can be names that preserve case,
names with lowercase letters as canonical representation.
-}
module Text.XML.Basic.Name where

-- * types and classes

class Ord name => C name where
   fromString :: String -> name
   toString :: name -> String


{- |
We need to distinguish between tag names and attribute names,
because DOCTYPE as tag name must be written upper case,
whereas as attribute name it may be written either way.
-}
class Ord ident => Tag ident where
   tagFromString :: String -> ident
   tagToString :: ident -> String

class Ord ident => Attribute ident where
   attributeFromString :: String -> ident
   attributeToString :: ident -> String



-- * convenience functions

match :: (C name) => String -> name -> Bool
match proto = (fromString proto ==)

matchAny :: (C name) => [String] -> name -> Bool
matchAny proto = flip elem (map fromString proto)
