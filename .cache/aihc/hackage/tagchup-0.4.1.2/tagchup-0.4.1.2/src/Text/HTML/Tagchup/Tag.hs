module Text.HTML.Tagchup.Tag (
   T(..), Name(..),
   mapName,

   open,       isOpen,       maybeOpen,
   close,      isClose,      maybeClose,
   text,       isText,       maybeText,   innerText,
   comment,    isComment,    maybeComment,
   special,    isSpecial,    maybeSpecial,
   cdata,      isCData,      maybeCData,
   processing, isProcessing, maybeProcessing,
   warning,    isWarning,    maybeWarning,

   formatOpen, formatClose,

   textFromCData, concatTexts,
   mapText, mapTextA,
   ) where

import qualified Text.HTML.Tagchup.Character as Chr

import qualified Text.XML.Basic.ProcessingInstruction as PI
import qualified Text.XML.Basic.Attribute as Attr
import qualified Text.XML.Basic.Name as Name
import qualified Text.XML.Basic.Format as Fmt
import Text.XML.Basic.Tag (Name(Name), cdataName, )

import Data.Tuple.HT (mapFst, )
import Data.Maybe (mapMaybe, fromMaybe, )
import Data.Monoid (Monoid, mempty, mappend, mconcat, )
import Control.Monad (guard, )

import Data.Foldable (Foldable(foldMap), )
import Data.Traversable (Traversable(sequenceA), traverse, )
import Control.Applicative (Applicative, pure, liftA, )


-- * type definitions

{- |
An HTML element, a document is @[T]@.
There is no requirement for 'Open' and 'Close' to match.

The type parameter @string@ lets you choose between
@[Char]@ for interpreted HTML entity references and
@[HTMLChar.T]@ for uninterpreted HTML entities.
You will most oftenly want plain @Char@,
since @HTMLChar.T@ is only necessary if you want to know,
whether a non-ASCII character was encoded as HTML entity
or as non-ASCII Unicode character.
-}
data T name string =
     Open (Name name) [Attr.T name string]
        -- ^ An open tag with 'Attr.T's in their original order.
   | Close (Name name)
        -- ^ A closing tag
   | Text string
        -- ^ A text node, guaranteed not to be the empty string
   | Comment String
        -- ^ A comment
   | Special (Name name) String
        -- ^ A tag like @\<!DOCTYPE ...\>@
   | Processing (Name name) (PI.T name string)
        -- ^ A tag like @\<?xml ...\>@
   | Warning String
        -- ^ Mark a syntax error in the input file
     deriving (Show, Eq, Ord)


instance Functor (T name) where
   fmap f tag =
      case tag of
         Open name attrs      -> Open name $ map (fmap f) attrs
         Close name           -> Close name
         Text string          -> Text $ f string
         Comment string       -> Comment string
         Special name content -> Special name content
         Processing name proc -> Processing name $ fmap f proc
         Warning string       -> Warning string


instance Foldable (T name) where
   foldMap f tag =
      case tag of
         Open _name attrs       -> foldMap (foldMap f) attrs
         Close _name            -> mempty
         Text string            -> f string
         Comment _text          -> mempty
         Special _name _content -> mempty
         Processing _name proc  -> foldMap f proc
         Warning _text          -> mempty


instance Traversable (T name) where
   sequenceA tag =
      case tag of
         Open name attrs      -> liftA (Open name) $ traverse sequenceA attrs
         Close name           -> pure $ Close name
         Text string          -> liftA Text $ string
         Comment string       -> pure $ Comment string
         Special name content -> pure $ Special name content
         Processing name proc -> liftA (Processing name) $ sequenceA proc
         Warning string       -> pure $ Warning string


mapName ::
   (Name name0 -> Name name1) ->
   (Attr.Name name0 -> Attr.Name name1) ->
   T name0 string -> T name1 string
mapName f g tag =
   case tag of
      Open name attrs -> Open (f name) $ map (Attr.mapName g) attrs
      Close name      -> Close (f name)
      Text string     -> Text string
      Comment string  -> Comment string
      Special name content -> Special (f name) content
      Processing name proc -> Processing (f name) $ PI.mapName g proc
      Warning string       -> Warning string





instance (Name.Tag name, Name.Attribute name, Fmt.C string) =>
      Fmt.C (T name string) where
   run t =
      case t of
         Open name attrs -> formatOpen False name attrs
         Close name -> formatClose name
         Text str -> Fmt.run str
         Comment c ->
            showString "<!--" . showString c . showString "-->"
         Warning e ->
            showString "<!-- Warning: " . showString e . showString " -->"
         Special name str ->
            Fmt.angle $
            Fmt.exclam .
            Fmt.name name .
            if cdataName == name
              then showString str . showString "]]"
              else Fmt.blank . showString str
         Processing name p ->
            Fmt.angle $
            Fmt.quest .
            Fmt.name name .
            Fmt.run p .
            Fmt.quest


formatOpen :: (Name.Tag name, Name.Attribute name, Fmt.C string) =>
   Bool -> Name name -> [Attr.T name string] -> ShowS
formatOpen selfClosing name attrs =
   Fmt.angle $
   Fmt.name name .
   Attr.formatListBlankHead attrs .
   if selfClosing then Fmt.slash else id

formatClose :: (Name.Tag name) =>
   Name name -> ShowS
formatClose name =
   Fmt.angle $
   Fmt.slash . Fmt.name name


-- * constructors for the tag types

open :: Name name -> [Attr.T name string] -> T name string
open = Open

close :: Name name -> T name string
close = Close

text :: string -> T name string
text = Text

comment :: String -> T name string
comment = Comment

special :: Name name -> String -> T name string
special = Special

cdata :: (Name.Tag name) => String -> T name string
cdata = special cdataName

processing :: Name name -> PI.T name string -> T name string
processing = Processing

warning :: String -> T name string
warning = Warning



-- * check for the tag types

-- | Test if a 'T' is a 'Open'
isOpen :: T name string -> Bool
isOpen tag = case tag of (Open {}) -> True; _ -> False

maybeOpen :: T name string -> Maybe (Name name, [Attr.T name string])
maybeOpen tag = case tag of Open name attrs -> Just (name, attrs); _ -> Nothing


-- | Test if a 'T' is a 'Close'
isClose :: T name string -> Bool
isClose tag = case tag of (Close {}) -> True; _ -> False

maybeClose :: T name string -> Maybe (Name name)
maybeClose tag = case tag of Close x -> Just x; _ -> Nothing


-- | Test if a 'T' is a 'Text'
isText :: T name string -> Bool
isText tag = case tag of (Text {}) -> True; _ -> False

-- | Extract the string from within 'Text', otherwise 'Nothing'
maybeText :: T name string -> Maybe string
maybeText tag = case tag of Text x -> Just x; _ -> Nothing
-- maybeText tag = do Text x <- Just tag; return x

-- | Extract all text content from tags (similar to Verbatim found in HaXml)
innerText :: (Monoid string) => [T name string] -> string
innerText = mconcat . mapMaybe maybeText


isComment :: T name string -> Bool
isComment tag = case tag of (Comment {}) -> True; _ -> False

maybeComment :: T name string -> Maybe String
maybeComment tag = case tag of Comment x -> Just x; _ -> Nothing


isSpecial :: T name string -> Bool
isSpecial tag = case tag of (Special {}) -> True; _ -> False

maybeSpecial :: T name string -> Maybe (Name name, String)
maybeSpecial tag = case tag of Special name content -> Just (name, content); _ -> Nothing


isCData ::
   (Name.Tag name) =>
   T name string -> Bool
isCData tag = case tag of (Special name _) -> cdataName == name; _ -> False

maybeCData ::
   (Name.Tag name) =>
   T name string -> Maybe String
maybeCData tag =
   do (name, content) <- maybeSpecial tag
      guard (cdataName == name)
      return content


isProcessing :: T name string -> Bool
isProcessing tag = case tag of (Processing {}) -> True; _ -> False

maybeProcessing :: T name string -> Maybe (Name name, PI.T name string)
maybeProcessing tag = case tag of Processing target instr -> Just (target, instr); _ -> Nothing


isWarning :: T name string -> Bool
isWarning tag = case tag of (Warning {}) -> True; _ -> False

maybeWarning :: T name string -> Maybe String
maybeWarning tag = case tag of Warning x -> Just x; _ -> Nothing
-- maybeWarning tag = do Warning x <- Just tag; return x



-- * tag processing

{- |
Replace CDATA sections by plain text.
-}
textFromCData ::
   (Name.Tag name, Chr.C char) =>
   T name [char] -> T name [char]
textFromCData t =
   fromMaybe t $
      do (name, content) <- maybeSpecial t
         guard (cdataName == name)
         return $ Text $ map Chr.fromChar content

{-
textFromCData ::
   (Name.Tag name) =>
   T name String -> T name String
textFromCData t =
   fromMaybe t $
      do (name, content) <- maybeSpecial t
         guard (cdataName == name)
         return $ Text content
-}

{-
   case t of
      Special name text ->
         if cdataName == name
           then Text text
           else t
      _ -> t
-}

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
            Text str0 ->
               uncurry (:) $
               mapFst (Text . mappend str0) $
               case ts of
                  Text str1 : rest -> (str1,rest)
                  _ -> (mempty,ts)
            _ -> t:ts)
      []


{- |
Modify content of a Text or a CDATA part.
-}
mapText ::
   (Name.Tag name) =>
   (String -> String) ->
   T name String -> T name String
mapText f t =
   case t of
      Text s -> Text $ f s
      Special name s ->
         Special name $
            if cdataName == name
              then f s
              else s
      _ -> t

mapTextA ::
   (Name.Tag name, Applicative f) =>
   (String -> f String) ->
   T name String -> f (T name String)
mapTextA f t =
   case t of
      Text s -> liftA Text $ f s
      Special name s ->
         liftA (Special name) $
            if cdataName == name
              then f s
              else pure s
      _ -> pure t
