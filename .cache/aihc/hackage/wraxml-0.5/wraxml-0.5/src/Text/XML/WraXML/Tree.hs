module Text.XML.WraXML.Tree where

import qualified Text.HTML.WraXML.String as HtmlString
import qualified Text.XML.WraXML.String  as XmlString
import qualified Text.XML.WraXML.Element as Elem

import qualified Text.XML.Basic.Tag as Tag
import qualified Text.XML.Basic.Attribute as Attr
import qualified Text.XML.Basic.Name as Name
import qualified Text.XML.Basic.ProcessingInstruction as PI
import qualified Text.XML.Basic.Format as Format

import qualified Data.Tree.BranchLeafLabel as Tree
import qualified Data.String.Unicode as Unicode
import qualified Data.Char as Char

import qualified Control.Monad.Exception.Synchronous as Exc
import Control.Monad.Trans.Writer (Writer, writer, runWriter, censor)

import           Data.Foldable (Foldable(foldMap))
import           Data.Traversable (Traversable(traverse))
import           Control.Applicative (Applicative, )
import qualified Control.Applicative as App
import           Data.List.HT (unzipEithers, )
import           Data.Tuple.HT (mapFst, mapSnd, mapPair, swap, )
import           Data.Maybe (mapMaybe, fromMaybe, )
import           Data.Monoid (Monoid, mempty, mappend, mconcat, )


{- * data structures -}

newtype T i name str =
   Cons {unwrap :: Tree.T i (Branch name str) (Leaf name str)}
   deriving (Show)

data Branch name str =
     Tag    {getElement :: Elem.T name str}
   deriving (Show)

{-
It is disputable whether comments (and warnings)
shall have type String or 'str'.
Can a comment contain non-ASCII characters and XML entities?
This is important for finding the comment ending "-->" properly.
What about scripts enclosed in comment delimiters?
Mozilla and Firefox don't do any encoding in the SCRIPT tag at all.
This seems to be wrong, since scripts frequently contain strings
with tag descriptions, like '<BR>'.
-}
data Leaf name str =
     Text    Bool {- is whitespace significant -} str
   | Comment String  -- better 'str'?
   | CData   String
   | PI      (Tag.Name name) (PI.T name str)
   | Warning String  -- better 'str'?


{-# DEPRECATED AttributePlain "use Attribute type from xml-basic package instead" #-}
type AttributePlain = (String, String)


instance (Name.Attribute name, Show name, Show str) =>
      Show (Leaf name str) where
   showsPrec prec x =
      showParen (prec>=10) $
      case x of
         Text   _ str -> showString "Text "    . showsPrec 11 str
         Comment  str -> showString "Comment " . showsPrec 11 str
         CData    str -> showString "CData "   . showsPrec 11 str
         PI target p  ->
--            showString "ProcessingInstruction " .
            showString "PI " .
            showsPrec 11 target . showString " " .
            showsPrec 11 p
         {- Maybe it is better to attach warnings to malicious tags
            instead of throwing them into the tag soup.
            But how to tell the position of the error then? -}
         Warning  str -> showString "Warning " . showsPrec 11 str



{- * generators -}

wrap :: Tree.T i (Branch name str) (Leaf name str) -> T i name str
wrap = Cons

wrap2 :: i -> Tree.Elem i (Branch name str) (Leaf name str) -> T i name str
wrap2 = curry wrap

lift ::
   (Tree.T i (Branch name str0) (Leaf name str0) ->
    Tree.T j (Branch name str1) (Leaf name str1)) ->
   (T i name str0 -> T j name str1)
lift f = wrap . f . unwrap

liftA :: Applicative m =>
   (Tree.T i (Branch name str0) (Leaf name str0) ->
    m (Tree.T i (Branch name str1) (Leaf name str1))) ->
   (T i name str0 -> m (T i name str1))
liftA f = App.liftA wrap . f . unwrap


-- | Build some textual content.
literal :: str -> T i name str
literal = literalIndex (error "literal: no index given")

literalIndex :: i -> str -> T i name str
literalIndex i =
   wrap2 i .
   Tree.Leaf . Text False


comment :: String -> T i name str
comment = commentIndex (error "comment: no index given")

commentIndex :: i -> String -> T i name str
commentIndex i =
   wrap2 i .
   Tree.Leaf . Comment

warning :: String -> T i name str
warning = warningIndex (error "warning: no index given")

warningIndex :: i -> String -> T i name str
warningIndex i =
   wrap2 i .
   Tree.Leaf . Warning

cdata :: String -> T i name str
cdata = cdataIndex (error "cdata: no index given")

cdataIndex :: i -> String -> T i name str
cdataIndex i =
   wrap2 i .
   Tree.Leaf . CData

processing :: Tag.Name name -> PI.T name str -> T i name str
processing = processingIndex (error "processing: no index given")

processingIndex :: i -> Tag.Name name -> PI.T name str -> T i name str
processingIndex i target =
   wrap2 i .
   Tree.Leaf . PI target

tag :: Tag.Name name -> [T i name str] -> T i name str
tag name = tagAttr name []

tagAttr :: Tag.Name name -> [Attr.T name str] -> [T i name str] -> T i name str
tagAttr = tagIndexAttr (error "tagAttr: no index given")

tagIndexAttr :: i -> Tag.Name name -> [Attr.T name str] -> [T i name str] -> T i name str
tagIndexAttr index name attrs =
   wrap2 index .
   Tree.Branch (Tag (Elem.Cons name attrs)) .
   map unwrap


{- * Conversions -}

liftTrans :: (a -> b) -> (a -> [b])
liftTrans f = (:[]) . f

liftText :: (String -> String) -> (Leaf name String -> Leaf name String)
liftText f leaf =
   case leaf of
      Text b s -> Text b (f s)
      CData s  -> CData (f s)
      _ -> leaf

liftTextA :: Applicative m => (String -> m String) -> (Leaf name String -> m (Leaf name String))
liftTextA f leaf =
   case leaf of
      Text b s -> App.liftA (Text b) (f s)
      CData s  -> App.liftA CData $ f s
      _ -> App.pure leaf



instance Functor (Leaf name) where
   fmap f leaf =
      case leaf of
         Text b s  -> Text b (f s)
         Comment s -> Comment s
         Warning s -> Warning s
         CData s   -> CData s
         PI t p    -> PI t $ fmap f p

{- this instance is quite useless but required by Traversable -}
instance Foldable (Leaf name) where
   foldMap f leaf =
      case leaf of
         Text _b s -> f s
         PI _t p   -> foldMap f p
         _ -> mempty

instance Traversable (Leaf name) where
   traverse f leaf =
      case leaf of
         Text b s  -> App.liftA (Text b) (f s)
         Comment s -> App.pure $ Comment s
         Warning s -> App.pure $ Warning s
         CData s   -> App.pure $ CData s
         PI t p    -> App.liftA (PI t) $ traverse f p



liftElement :: (Elem.T name str0 -> Elem.T name str1) -> (Branch name str0 -> Branch name str1)
liftElement f (Tag elm) = Tag (f elm)

liftElementA :: Applicative m =>
   (Elem.T name str0 -> m (Elem.T name str1)) -> (Branch name str0 -> m (Branch name str1))
liftElementA f (Tag elm) = App.liftA Tag (f elm)


{- * Tests -}

{- |
If the Tree is a Leaf, then return False.
Otherwise return the result of the predicate.
-}
checkTag :: (Elem.T name str -> Bool) -> (T i name str -> Bool)
checkTag p =
   Tree.switch (flip const) (const . p . getElement) (const False)
    . unwrap

maybeTag :: T i name str -> Maybe (Elem.T name str, [T i name str])
maybeTag (Cons (_,t)) =
   case t of
      Tree.Branch (Tag elm) subTrees ->
           Just (elm, map wrap subTrees)
      _ -> Nothing

maybeText :: T i name str -> Maybe str
maybeText (Cons (_,t)) =
   case t of
      Tree.Leaf l -> maybeTextLeaf l
      _           -> Nothing

maybeTextLeaf :: Leaf name str -> Maybe str
maybeTextLeaf t =
   case t of
      Text _ s -> Just s
      _        -> Nothing

maybeCommentLeaf :: Leaf name str -> Maybe String
maybeCommentLeaf t =
   case t of
      Comment s -> Just s
      _         -> Nothing

maybeCDataLeaf :: Leaf name str -> Maybe String
maybeCDataLeaf t =
   case t of
      CData s -> Just s
      _       -> Nothing

maybeProcessingLeaf :: Leaf name str -> Maybe (Tag.Name name, PI.T name str)
maybeProcessingLeaf t =
   case t of
      PI n instr -> Just (n, instr)
      _          -> Nothing

maybeWarningLeaf :: Leaf name str -> Maybe String
maybeWarningLeaf t =
   case t of
      Warning s -> Just s
      _         -> Nothing


fold ::
   (i -> a -> b) ->
   (Elem.T name str -> [b] -> a) ->
   (Leaf name str -> a) ->
   (T i name str -> b)
fold iF branchF leafF =
   Tree.fold iF (branchF . getElement) leafF . unwrap

switch ::
   (i -> a -> b) ->
   (Elem.T name str -> [T i name str] -> a) ->
   (Leaf name str -> a) ->
   (T i name str -> b)
switch iF branchF leafF =
   Tree.switch iF
      (\b subTrees -> branchF (getElement b) (map wrap subTrees))
      leafF
    . unwrap



{- * types of processors -}

type Filter i name str = T i name str -> T i name str

type FilterA m i name str = T i name str -> m (T i name str)


{- * tree processors -}


instance Functor (T i name) where
   fmap f =
      lift $
      Tree.map
         (liftElement $ fmap f)
         (fmap f)


mapText ::
   (String -> String) ->
   (T i name String -> T i name String)
mapText f =
   lift $
   Tree.map
      (liftElement $ fmap f)
      (liftText f)


mapIndex :: (i -> j) -> T i name str -> T j name str
mapIndex f =
   lift $ Tree.mapLabel f


mapTag ::
   (Elem.Filter name str) ->
   (Filter i name str)
mapTag f =
   lift $
   Tree.map (liftElement f) id

{- |
Convert all CData sections to plain text.
-}
textFromCData :: T i name String -> T i name String
textFromCData =
   lift $
   Tree.map id
      (\leaf -> maybe leaf (Text False) $ maybeCDataLeaf leaf)


{- |
You can e.g. filter @text1 <b> text2 </b> text3@
to @text1  text2  text3@ by
@filterTag (checkTagName ("b"\/=))@.
-}
filterTag ::
   (Elem.T name str -> Bool) ->
   (T i name str -> [T i name str])
filterTag p =
   map wrap .
   Tree.filterBranch (p . getElement) .
   unwrap

mapCond ::
   (Elem.T name str -> Bool) ->
   (Elem.Filter name str) ->
   (Leaf name str -> Leaf name str) ->
   (Filter i name str)
mapCond descend elemF txtF =
   lift $
   Tree.mapCond (descend . getElement) (liftElement elemF) txtF

{-
mapTextCond ::
   (Elem.T name String -> Bool) ->
   (Elem.T name String -> Elem.T name String) ->
   (String -> String) ->
   (Filter i name String)
mapTextCond descend elemF txtF =
   lift $
   Tree.mapCond (descend . getElement) (liftElement elemF) (liftText txtF)
-}


{- |
Find all branches where the predicate applies and
return a list of matching sub-trees in depth-first order.

Example: @filterTagsFlatten (checkTagName ("meta"==))@
-}
filterTagsFlatten ::
   (Elem.T name str -> Bool) ->
   T i name str ->
   [(Elem.T name str, [T i name str])]
filterTagsFlatten p =
   filter (p . fst) .
   mapMaybe maybeTag .
   allSubTrees

filterElementsFlatten ::
   (Elem.T name str -> Bool) ->
   T i name str ->
   [Elem.T name str]
filterElementsFlatten p =
   Tree.fold
      (flip const)
      (\branch xs -> filter p [getElement branch] ++ concat xs)
      (const []) .
   unwrap

allSubTrees :: T i name str -> [T i name str]
allSubTrees =
   map wrap . Tree.allSubTrees . unwrap

{- | merge subsequent string leafs -}
mergeStrings :: (Monoid str) => Filter i name str
mergeStrings =
   processAllSubTrees mergeTopStrings


mergeTopStrings :: (Monoid str) => [T i name str] -> [T i name str]
mergeTopStrings =
   let prepend (i, Tree.Leaf (Text w0 t0)) rest =
          mapFst (\ ~(w1,t1) -> (i, Tree.Leaf $ Text (w0||w1) (mappend t0 t1))) $
          case rest of
             (_, Tree.Leaf (Text w1 t1)) : ss -> ((w1,t1), ss)
             _ -> ((False,mempty), rest)
       prepend x rest = (x, rest)
   in  map wrap.
       foldr (\x -> uncurry (:) . prepend x) [] .
       map unwrap


{- |
Process all sub-tree lists in bottom-up order.
-}
processAllSubTrees ::
   ([T i name str] -> [T i name str]) ->
   Filter i name str
processAllSubTrees f =
   lift $
   Tree.fold
      (,)
      (\branch -> Tree.Branch branch . map unwrap . f . map wrap)
      Tree.Leaf

processSubTrees ::
   (Tag.Name name -> Bool) ->
   ([T i name str] -> [T i name str]) ->
   Filter i name str
processSubTrees p f =
   lift $
   Tree.mapSubTrees
      (\(Tag (Elem.Cons name _)) -> p name)
      (mapSnd (map unwrap . f . map wrap))

processSubTreesAttrs ::
   (Tag.Name name -> Bool) ->
   (([Attr.T name str], [T i name str]) ->
    ([Attr.T name str], [T i name str])) ->
   Filter i name str
processSubTreesAttrs p f =
   lift $
   Tree.mapSubTrees
      (\(Tag (Elem.Cons name _)) -> p name)
      (\(Tag (Elem.Cons name attrs), subTrees) ->
          mapPair (Tag . Elem.Cons name, map unwrap) $
          f (attrs, map wrap subTrees))



{- * applicative functor tree processors -}

instance Foldable (T i name) where
   foldMap f =
      Tree.fold
         (const id) (const mconcat) (maybe mempty f . maybeTextLeaf) .
      unwrap


instance Traversable (T i name) where
   traverse f =
      liftA $
      Tree.mapA
         (liftElementA $ traverse  f)
         (traverse f)


mapTextA :: Applicative m =>
   (String -> m String) ->
   (FilterA m i name String)
mapTextA f =
   liftA $
   Tree.mapA
      (liftElementA $ traverse  f)
      (liftTextA f)


mapCondA :: Applicative m =>
   (Elem.T name str -> Bool) ->
   (Elem.T name str -> m (Elem.T name str)) ->
   (Leaf name str -> m (Leaf name str)) ->
   (FilterA m i name str)
mapCondA descend elemF txtF =
   liftA $
   Tree.mapCondA (descend . getElement) (liftElementA elemF) txtF

{-
mapTextCondA :: Applicative m =>
   (Elem.T name String -> Bool) ->
   (Elem.T name String -> m (Elem.T name String)) ->
   (String -> m String) ->
   (FilterA m i name String)
mapTextCondA descend elemF txtF =
   liftA $
   Tree.mapCondA (descend . getElement) (liftElementA elemF) (liftTextA txtF)
-}


{- * Character decoding -}

unescape :: T i name XmlString.T -> T i name String
unescape = fmap XmlString.toUnicodeString

{- |
Use ASCII characters, XML entity references and character references
for representing strings.
That's not human readable, but portable.
-}
escape :: T i name String -> T i name XmlString.T
escape = fmap XmlString.fromUnicodeString


{-# DEPRECATED decodeSpecialChars, maybeDecodeSpecialChars, decodeSpecialCharsDecoder, decodeAttrs, decodeAttr, maybeDecodeUTF8Chars "XmlChar.Unicode constructors must contain unicode characters and not encoded ones. Decode characters before parsing!" #-}

{- |
Decode characters like those from UTF-8 scheme.
-}
decodeSpecialChars ::
   (Name.Tag name, Name.Attribute name) =>
   String -> T i name XmlString.T -> [T i name String]
decodeSpecialChars enc tree =
   fromMaybe
      [unescape tree]
      (maybeDecodeSpecialChars enc tree)

maybeDecodeSpecialChars ::
   (Name.Tag name, Name.Attribute name) =>
   String -> T i name XmlString.T -> Maybe [T i name String]
maybeDecodeSpecialChars enc tree =
   fmap (flip decodeSpecialCharsDecoder tree) $
   Unicode.getDecodingFctEmbedErrors enc

{- test:
-- decodeSpecialChars "utf-8" $ literalIndex 0 (XmlString.fromEntString "tr&am;e")

traverse (putStrLn . showHTML . escape) $ decodeSpecialChars "utf-8" $ mapText XmlString.fromEntString $ tagIndexAttr 0 "br" [("href","urlö"), ("target","_blank")] [literalIndex 0 "\195tr&am;eü"]
-}

{- |
Conversion errors are appended as warnings to the tree.
-}
decodeSpecialCharsDecoder ::
   (Name.Tag name, Name.Attribute name) =>
   Unicode.DecodingFctEmbedErrors -> T i name XmlString.T -> [T i name String]
decodeSpecialCharsDecoder decode =
   let xmlDecode =
          HtmlString.toUnicodeStringDecodingEmbedError decode
       mergeDecode =
          XmlString.uStringWithErrorsMergePlainChars . xmlDecode
   in  Tree.foldLabel
          (\i branch subTrees ->
              case branch of
                 Tag (Elem.Cons name attrs) ->
                    let (newAttrs,warnings) =
                           runWriter $
                           decodeAttrs xmlDecode attrs
                    in  [tagIndexAttr i name newAttrs
                           (map (warningIndex i) warnings ++ concat subTrees)])
          (\i leaf -> map (wrap2 i . Tree.Leaf) $
              case leaf of
                 Text  b str ->
                    map
                      (Exc.switch Warning (Text b))
                      (mergeDecode str)
                 Comment   cmt   -> [Comment cmt]
                 Warning   str   -> [Warning str]
                 CData     str   -> [CData str]
                 PI target instr0 ->
                    let (instr1,warnings) =
                           runWriter $
                           PI.mapAttributesA (decodeAttrs xmlDecode) instr0
                    in  PI target instr1 : map Warning warnings) .
       unwrap

decodeAttrs ::
   (Name.Tag name, Name.Attribute name) =>
   (XmlString.T -> XmlString.EmbeddedExceptions) ->
   [Attr.T name XmlString.T] -> Writer [String] [Attr.T name String]
decodeAttrs xmlDecode =
   traverse
      (\attr ->
         traverse  (decodeAttr xmlDecode (Attr.name_ attr)) attr)

decodeAttr ::
   (Name.Tag name, Name.Attribute name) =>
   (XmlString.T -> XmlString.EmbeddedExceptions) ->
   Attr.Name name -> XmlString.T -> Writer [String] String
decodeAttr decode name =
   censor (map (showString $ "in attribute \"" ++ Name.toString name ++ "\": ")) .
   writer . swap . unzipEithers . map Exc.toEither . decode


maybeDecodeUTF8Chars :: String -> T i name XmlString.T -> Maybe (T i name String)
maybeDecodeUTF8Chars enc tree =
   case map Char.toLower enc of
      "utf-8" -> Just (fmap XmlString.utf8ToUnicodeString tree)
      _ -> Nothing


{- * Formatting -}

{-
show ::
   (Name.Tag name, Name.Attribute name) =>
   T i name XmlString.T -> String
show leaf = shows leaf ""
-}

formatMany ::
   (Name.Tag name, Name.Attribute name, Format.C string) =>
   [T i name string] -> ShowS
formatMany = Format.many format

-- cf. src/Text/XML/HXT/DOM/XmlTreeFunctions.hs
format ::
   (Name.Tag name, Name.Attribute name, Format.C string) =>
   T i name string -> ShowS
format =
   Tree.fold (flip const) formatBranch formatLeaf . unwrap

formatBranch ::
   (Name.Tag name, Name.Attribute name, Format.C string) =>
   Branch name string -> [ShowS] -> ShowS
formatBranch branch formatSubTrees =
   case branch of
      Tag elm ->
         Elem.format
            (\_tagName -> null formatSubTrees)
            Format.slash
            elm formatSubTrees

formatLeaf ::
   (Name.Tag name, Name.Attribute name, Format.C string) =>
   Leaf name string -> ShowS
formatLeaf leaf =
   case leaf of
      Text _ str -> Format.run str
      Comment c ->
         showString "<!--" . showString c . showString "-->"
      Warning e ->
         showString "<!-- Warning: " . showString e . showString " -->"
      CData str ->
         showString "<![CDATA[" . showString str . showString "]]>"
      PI target p ->
         Format.angle $
         Format.quest .
         Format.name target .
         Format.run p .
         Format.quest

instance
   (Name.Tag name, Name.Attribute name, Format.C string) =>
      Format.C (T i name string) where
   run = format

instance
   (Name.Tag name, Name.Attribute name, Format.C string) =>
      Format.C (Leaf name string) where
   run = formatLeaf
