module Text.HTML.Tagchup.PositionTag where

import qualified Text.HTML.Tagchup.Character as Chr
import qualified Text.HTML.Tagchup.Tag as Tag
import qualified Text.XML.Basic.Name as Name
import qualified Text.XML.Basic.Position as Position

import Data.Tuple.HT (mapFst, )
import Data.Monoid (Monoid, mempty, mappend, )

import qualified Data.Accessor.Basic as Accessor
import qualified Control.Applicative as App

import Data.Foldable (Foldable(foldMap), )
import Data.Traversable (Traversable(sequenceA), )
import Control.Applicative (Applicative, )


data T name string =
   Cons {
      position_ :: Position.T,
      tag_ :: Tag.T name string
   }

instance (Name.Attribute name, Show string, Show name) =>
   Show (T name string) where
  showsPrec p (Cons pos t) =
     showParen (p > 10)
        (showString "PosTag.cons " .
         showsPrec 11 pos . showString " " .
         showsPrec 11 t)

{-
> cons (Position.new "bla" 0 0) (Tag.Close $ Name.fromString "bla" :: Tag.T Text.XML.Basic.Name.LowerCase.T String)
-}
cons :: Position.T -> Tag.T name string -> T name string
cons = Cons

position :: Accessor.T (T name string) Position.T
position = Accessor.fromSetGet (\n p -> p{position_ = n}) position_

tag :: Accessor.T (T name string) (Tag.T name string)
tag = Accessor.fromSetGet (\n p -> p{tag_ = n}) tag_

lift ::
   (Tag.T name0 string0 -> Tag.T name1 string1) ->
   (T name0 string0 -> T name1 string1)
lift f (Cons p t) = Cons p (f t)

liftA ::
   Applicative f =>
   (Tag.T name0 string0 -> f (Tag.T name1 string1)) ->
   (T name0 string0 -> f (T name1 string1))
liftA f (Cons p t) = App.liftA (Cons p) (f t)


instance Functor (T name) where
   fmap f = lift (fmap f)

instance Foldable (T name) where
   foldMap f = foldMap f . tag_

instance Traversable (T name) where
   sequenceA (Cons p t) = App.liftA (Cons p) $ sequenceA t


textFromCData ::
   (Name.Tag name, Chr.C char) =>
   T name [char] -> T name [char]
textFromCData = lift Tag.textFromCData


{- |
Merge adjacent Text sections.
-}
concatTexts ::
   Monoid string =>
   [T name string] -> [T name string]
concatTexts =
   foldr
      (\t ts ->
         case t of
            Cons pos (Tag.Text str0) ->
               uncurry (:) $
               mapFst (cons pos . Tag.Text . mappend str0) $
               case ts of
                  Cons _ (Tag.Text str1) : rest -> (str1,rest)
                  _ -> (mempty,ts)
            _ -> t:ts)
      []
