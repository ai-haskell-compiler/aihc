module Text.XML.WraXML.Element where

import qualified Text.XML.Basic.Tag as Tag
import qualified Text.XML.Basic.Attribute as Attr
import qualified Text.XML.Basic.Name as Name

import qualified Data.Accessor.Basic as Accessor

import           Data.Foldable as Foldable(Foldable(foldMap))
import           Data.Traversable as Traversable(Traversable(traverse))
import           Control.Applicative (Applicative, )
import qualified Control.Applicative as App
import           Data.Monoid (mconcat, )

import qualified Text.XML.Basic.Format as Format


data T name str =
   Cons {
      name_       :: Tag.Name name,
      attributes_ :: [Attr.T name str]
   } deriving (Show)

type Filter name str = T name str -> T name str


cons ::
   (Name.Tag name, Name.Attribute name) =>
   Tag.Name name -> [Attr.T name str] -> T name str
cons = Cons

name :: Accessor.T (T name str) (Tag.Name name)
name = Accessor.fromSetGet (\n p -> p{name_ = n}) name_

attributes :: Accessor.T (T name str) [Attr.T name str]
attributes = Accessor.fromSetGet (\n p -> p{attributes_ = n}) attributes_


-- * tests

checkName :: (Tag.Name name -> Bool) -> (T name str -> Bool)
checkName p (Cons tagName _) = p tagName

checkAttributes :: ([Attr.T name str] -> Bool) -> (T name str -> Bool)
checkAttributes p (Cons _ attrs) = p attrs


-- * modification

instance Functor (T name) where
   fmap f (Cons tagName attrs) =
      Cons tagName (Attr.mapValues f attrs)


lift ::
   (Tag.Name name -> [Attr.T name str] -> (Tag.Name name, [Attr.T name str])) ->
   (Filter name str)
lift f (Cons tagName attrs) =
   uncurry Cons (f tagName attrs)


-- | process the attribute list of a specific tag
processAttrs ::
   (Name.Tag name, Name.Attribute name) =>
   (Tag.Name name -> Bool) ->
   ([Attr.T name str] -> [Attr.T name str]) ->
   (Filter name str)
processAttrs p f =
   lift (\tagName ->
      (,) tagName . if p tagName then f else id)

processAttrValue ::
   (Name.Tag name, Name.Attribute name) =>
   (Tag.Name name, Attr.Name name) ->
   (str -> str) ->
   (Filter name str)
processAttrValue (tagName,attrName) f =
   processAttrs (tagName ==)
      (Attr.adjustOn (attrName ==) f)

processAttrValueCond ::
   (Name.Tag name, Name.Attribute name) =>
   (Tag.Name name, Attr.Name name) ->
   ([Attr.T name str] -> Bool) ->
   (str -> str) ->
   (Filter name str)
processAttrValueCond (tagName,attrName) cond f =
   processAttrs (tagName ==)
      (\attrs -> Attr.adjustOn (attrName ==)
         (if cond attrs then f else id)
         attrs)



instance Foldable (T name) where
   foldMap f (Cons _tagName attrs) =
      mconcat $ map (foldMap f) attrs

instance Traversable (T name) where
   traverse f (Cons tagName attrs) =
      App.liftA (Cons tagName) (Attr.mapValuesA f attrs)



-- * monadic modification

-- | process the attribute list of a specific tag
processAttrsA ::
   (Name.Tag name, Name.Attribute name, Applicative m) =>
   (Tag.Name name -> Bool) ->
   ([Attr.T name str] -> m [Attr.T name str]) ->
   (T name str -> m (T name str))
processAttrsA p f =
   liftA (\tagName ->
      App.liftA ((,) tagName) .
      if p tagName then f else App.pure)

processAttrValueA ::
   (Name.Tag name, Name.Attribute name, Applicative m) =>
   (Tag.Name name, Attr.Name name) ->
   (str -> m str) ->
   (T name str -> m (T name str))
processAttrValueA (tagName,attrName) f =
   processAttrsA (tagName==)
      (Attr.adjustOnA (attrName==) f)

processAttrValueCondA ::
   (Name.Tag name, Name.Attribute name, Applicative m) =>
   (Tag.Name name, Attr.Name name) ->
   ([Attr.T name str] -> Bool) ->
   (str -> m str) ->
   (T name str -> m (T name str))
processAttrValueCondA (tagName,attrName) cond f =
   processAttrsA (tagName==)
      (\attrs -> Attr.adjustOnA (attrName==)
         (if cond attrs then f else App.pure)
         attrs)

liftA ::
   (Name.Tag name, Name.Attribute name, Applicative m) =>
   (Tag.Name name -> [Attr.T name str] ->
      m (Tag.Name name, [Attr.T name str])) ->
   (T name str -> m (T name str))
liftA f (Cons tagName attrs) =
   App.liftA (uncurry Cons) (f tagName attrs)



-- * format

format ::
   (Name.Tag name, Name.Attribute name, Format.C string) =>
   (Tag.Name name -> Bool) -> ShowS -> T name string -> [ShowS] -> ShowS
format isCondensed trailingSlash (Cons tagName attrs) formatSubTrees =
   let t = Name.toString tagName
   in  if isCondensed tagName
         then Format.angle
               (showString t . Attr.formatListBlankHead attrs
                 . trailingSlash)
         else Format.angle
               (showString t . Attr.formatListBlankHead attrs)
               . foldr (.) id formatSubTrees
               . Format.angle (Format.slash . showString t)
