module Text.XML.Basic.ProcessingInstruction (
   T(..),
   mapName,
   mapAttributes, mapAttributesA,
   ) where

import qualified Text.XML.Basic.Attribute as Attr
import qualified Text.XML.Basic.Name as Name
import qualified Text.XML.Basic.Format as Fmt

import Data.Monoid (mempty, )

import Data.Foldable (Foldable(foldMap), )
import Data.Traversable (Traversable(sequenceA), traverse, )
import Control.Applicative (Applicative, pure, liftA, )


data T name string =
     Known [Attr.T name string]
   | Unknown String
     deriving (Eq, Ord {- , Show -} )


{-
JHC cannot generate this instance automatically,
since it fails to generate the (Name.Attribute name) constraint.
-}
instance (Name.Attribute name, Show string) => Show (T name string) where
   showsPrec p x =
      showParen (p>10) $
      case x of
         Known attrs -> showString "Known " . showsPrec 11 attrs
         Unknown str -> showString "Unknown " . shows str


instance (Name.Attribute name, Fmt.C string) => Fmt.C (T name string) where
   run p =
      case p of
         Known attrs -> Attr.formatListBlankHead attrs
         Unknown str -> Fmt.blank . showString str


instance Functor (T name) where
   fmap f proc =
      case proc of
         Known attrs  -> Known $ map (fmap f) attrs
         Unknown text -> Unknown text

instance Foldable (T name) where
   foldMap f proc =
      case proc of
         Known attrs   -> foldMap (foldMap f) attrs
         Unknown _text -> mempty

instance Traversable (T name) where
   sequenceA proc =
      case proc of
         Known attrs  -> liftA Known $ traverse sequenceA attrs
         Unknown text -> pure $ Unknown text

mapName ::
   (Attr.Name name0 -> Attr.Name name1) ->
   T name0 string -> T name1 string
mapName f =
   mapAttributes (map (Attr.mapName f))


mapAttributes ::
   ([Attr.T name0 string0] -> [Attr.T name1 string1]) ->
   T name0 string0 -> T name1 string1
mapAttributes f proc =
   case proc of
      Known attrs  -> Known $ f attrs
      Unknown text -> Unknown text

mapAttributesA ::
   (Applicative f) =>
   ([Attr.T name0 string0] -> f [Attr.T name1 string1]) ->
   T name0 string0 -> f (T name1 string1)
mapAttributesA f proc =
   case proc of
      Known attrs  -> liftA Known $ f attrs
      Unknown text -> pure $ Unknown text
