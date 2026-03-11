module Text.XML.Basic.Attribute where

import qualified Text.XML.Basic.Name as Name
import qualified Text.XML.Basic.Format as Fmt

import Text.XML.Basic.Utility (updateAppend, )

import qualified Data.Accessor.Basic as Accessor

import Data.Foldable (Foldable(foldMap), )
import Data.Traversable (Traversable, sequenceA, traverse, )
import Control.Applicative (Applicative, pure, liftA, )

import qualified Data.List as List

import Prelude hiding (any, )


{- | An HTML attribute @id=\"name\"@ generates @(\"id\",\"name\")@ -}
data T name string =
   Cons {
      name_  :: Name name,
      value_ :: string
   } deriving (Eq, Ord)

cons :: (Name.Attribute name) => Name name -> string -> T name string
cons = Cons

new :: (Name.Attribute name) => String -> string -> T name string
new n v = Cons (Name.fromString n) v

lift ::
   (Name name -> string -> (Name name, string)) ->
   T name string -> T name string
lift f (Cons n v) = uncurry Cons $ f n v

toPair :: (Name.Attribute name) => T name string -> (String, string)
toPair (Cons n v) = (Name.toString n, v)

fromPair :: (Name.Attribute name) => (String, string) -> T name string
fromPair (n,v) = Cons (Name.fromString n) v

name :: Accessor.T (T name string) (Name name)
name = Accessor.fromSetGet (\n p -> p{name_ = n}) name_

value :: Accessor.T (T name string) string
value = Accessor.fromSetGet (\n p -> p{value_ = n}) value_


instance (Name.Attribute name, Show string) => Show (T name string) where
   showsPrec p = showsPrec p . toPair

instance (Name.Attribute name, Fmt.C string) => Fmt.C (T name string) where
   run attr =
      Fmt.name (name_ attr) . Fmt.eq .
      Fmt.stringQuoted (Fmt.run (value_ attr) "")

{- |
Each attribute is preceded by a space,
that is there is a space between adjacent attributes
and one leading space.
-}
formatListBlankHead ::
   (Name.Attribute name, Fmt.C string) =>
   [T name string] -> ShowS
formatListBlankHead =
   Fmt.many (\attr -> Fmt.blank . Fmt.run attr)

instance Functor (T name) where
   fmap f (Cons n v) = Cons n (f v)

instance Foldable (T name) where
   foldMap f (Cons _n v) = f v

instance Traversable (T name) where
   sequenceA (Cons n v) = liftA (Cons n) v



mapName :: (Name name0 -> Name name1) -> T name0 string -> T name1 string
mapName f (Cons n v) = Cons (f n) v



newtype Name ident = Name {unname :: ident}
   deriving (Eq, Ord)

instance Show ident => Show (Name ident) where
   showsPrec p = showsPrec p . unname

instance Name.Attribute ident => Name.C (Name ident) where
   fromString = Name . Name.attributeFromString
   toString = Name.attributeToString . unname


versionName :: (Name.Attribute name) => Name name
versionName = Name.fromString versionString

encodingName :: (Name.Attribute name) => Name name
encodingName = Name.fromString encodingString


versionString :: String
versionString = "version"

encodingString :: String
encodingString = "encoding"


-- * attribute lists

mapValues ::
   (str0 -> str1) ->
   ([T name str0] -> [T name str1])
mapValues f =
   map (fmap f)

mapValuesA :: Applicative f =>
   (str0 -> f str1) ->
   ([T name str0] -> f [T name str1])
mapValuesA f =
   traverse (traverse f)


{- |
Process specific attributes of an attribute list.
The function name is inspired by Data.Map.
-}
adjustOn ::
   (Name name -> Bool) ->
   (string -> string) ->
   ([T name string] -> [T name string])
adjustOn p f =
   map (\attr ->
      fmap (if p (name_ attr) then f else id) attr)

adjustOnA :: Applicative f =>
   (Name name -> Bool) ->
   (string -> f string) ->
   ([T name string] -> f [T name string])
adjustOnA p f =
   traverse (\attr ->
      traverse (if p (name_ attr) then f else pure) attr)


insert ::
   (Name.Attribute name) =>
   Name name ->
   string ->
   ([T name string] -> [T name string])
insert = insertWith const

{- |
Insert an attribute into an attribute list.
If an attribute with the same name is already present,
then the value of this attribute is changed to @f newValue oldValue@.
The function name is analogous to Data.Map.
-}
insertWith ::
   (Name.Attribute name) =>
   (string -> string -> string) ->
   Name name ->
   string ->
   ([T name string] -> [T name string])
insertWith f n v =
   updateAppend
      ((n ==) . name_)
      (Cons n v)
      (fmap (f v))


-- * match attributes

match ::
   (Name.Attribute name, Eq string) =>
   String -> string -> T name string -> Bool
match n v attr =
   Name.match n (name_ attr) && v == value_ attr

{- |
@matchManyValues name [value0, value1] attrs@
checks whether @(name, value0)@ or @(name, value1)@
is contained in @attrs@.
The values are handled case-sensitive.
-}
matchAnyValue ::
   (Name.Attribute name, Eq string) =>
   String -> [string] -> T name string -> Bool
matchAnyValue n vs attr =
   Name.match n (name_ attr) && elem (value_ attr) vs


lookup ::
   (Name.Attribute name) =>
   Name name -> [T name string] -> Maybe string
lookup n =
   fmap value_ .
   List.find ((n==) . name_)

lookupLit ::
   (Name.Attribute name) =>
   String -> [T name string] -> Maybe string
lookupLit n =
   fmap value_ .
   List.find (Name.match n . name_)


any :: (T name string -> Bool) -> [T name string] -> Bool
any = List.any

anyName :: (Name name -> Bool) -> [T name string] -> Bool
anyName p = any (p . name_)

anyValue :: (string -> Bool) -> [T name string] -> Bool
anyValue p = any (p . value_)


anyLit ::
   (Name.Attribute name, Eq string) =>
   String -> string -> [T name string] -> Bool
anyLit n v = any (match n v)

anyNameLit ::
   (Name.Attribute name) =>
   String -> [T name string] -> Bool
anyNameLit n = anyName (Name.match n)

anyValueLit :: (Eq string) => string -> [T name string] -> Bool
anyValueLit v = anyValue (v==)
