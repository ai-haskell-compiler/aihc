module Text.XML.WraXML.Document where

import qualified Text.XML.WraXML.Tree as XmlTree
import qualified Text.XML.Basic.Attribute as Attr
import qualified Text.XML.Basic.Name as Name
import qualified Text.XML.Basic.Format as Format

import Control.Applicative (Applicative, liftA2, )
import Data.Traversable (traverse, )


data T i name str =
   Cons {
      xmlDeclaration :: Maybe [Attr.T name str],
      docType :: Maybe String,
      content :: [XmlTree.T i name str]
   } deriving Show

instance
   (Name.Tag name, Name.Attribute name) =>
      Functor (T i name) where
   fmap f = lift f (map (fmap f))

lift ::
   (Name.Tag name, Name.Attribute name) =>
   (str0 -> str1) ->
   ([XmlTree.T i name str0] -> [XmlTree.T i name str1]) ->
   T i name str0 -> T i name str1
lift g f (Cons xml dtd trees) =
   Cons (fmap (map (fmap g)) xml) dtd $ f trees

liftA ::
   (Name.Tag name, Name.Attribute name, Applicative m) =>
   (str0 -> m str1) ->
   ([XmlTree.T i name str0] -> m [XmlTree.T i name str1]) ->
   T i name str0 -> m (T i name str1)
liftA g f (Cons xml dtd trees) =
   liftA2
      (\xmlDecl cnt -> Cons xmlDecl dtd cnt)
      (traverse (traverse (traverse g)) xml) (f trees)


formatXMLDeclaration ::
   (Name.Tag name, Name.Attribute name, Format.C string) =>
   [Attr.T name string] -> ShowS
formatXMLDeclaration attrs =
   showString "<?xml" . Attr.formatListBlankHead attrs . showString "?>" .
   Format.nl

formatDocType :: String -> ShowS
formatDocType dtdStr =
   Format.angle (showString "!DOCTYPE " . showString dtdStr) .
   Format.nl

format ::
   (Name.Tag name, Name.Attribute name, Format.C string) =>
   T i name string -> ShowS
format (Cons xml dtd trees) =
   maybe id formatXMLDeclaration xml .
   maybe id formatDocType dtd .
   XmlTree.formatMany trees

instance
   (Name.Tag name, Name.Attribute name, Format.C string) =>
      Format.C (T i name string) where
   run = format
