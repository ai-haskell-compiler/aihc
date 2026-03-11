{- |
This name type preserves the characters case of its input
and divides the names into namespace and local identifier.
-}
module Text.XML.Basic.Name.Qualified where

import qualified Text.XML.Basic.Name as Name
import qualified Data.Accessor.Basic as Accessor


data T = Cons {namespace_, local_ :: String}
   deriving (Show, Eq, Ord)

namespace :: Accessor.T T String
namespace = Accessor.fromSetGet (\n p -> p{namespace_ = n}) namespace_

local :: Accessor.T T String
local = Accessor.fromSetGet (\n p -> p{local_ = n}) local_


fromString :: String -> T
fromString =
   uncurry Cons .
   (\(n,pl) ->
       case pl of
          ':':l -> (n,l)
          _ -> ("",n)) .
   break (':'==)

toString :: T -> String
toString (Cons n l) =
   if null n
     then l
     else n ++ ':' : l


instance Name.Tag T where
   tagFromString = fromString
   tagToString = toString

instance Name.Attribute T where
   attributeFromString = fromString
   attributeToString = toString
