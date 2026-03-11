{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
-- hexpat, a Haskell wrapper for expat
-- Copyright (C) 2008 Evan Martin <martine@danga.com>
-- Copyright (C) 2009 Stephen Blackheath <http://blacksapphire.com/antispam>

-- | This module provides functions to format a tree
-- structure or SAX stream as UTF-8 encoded XML.
--
-- The formatting functions always outputs only UTF-8, regardless
-- of what encoding is specified in the document's 'Doc.XMLDeclaration'.
-- If you want to output a document in another encoding, then make sure the
-- 'Doc.XMLDeclaration' agrees with the final output encoding, then format the
-- document, and convert from UTF-8 to your desired encoding using some text
-- conversion library.
--
-- The lazy 'L.ByteString' representation of the output in generated with very
-- small chunks, so in some applications you may want to combine them into
-- larger chunks to get better efficiency.
module Text.XML.Expat.Format (
        -- * High level
        format,
        format',
        formatG,
        formatNode,
        formatNode',
        formatNodeG,
        -- * Format document (for use with Extended.hs)
        formatDocument,
        formatDocument',
        formatDocumentG,
        -- * Low level
        xmlHeader,
        treeToSAX,
        documentToSAX,
        formatSAX,
        formatSAX',
        formatSAXG,
        -- * Indentation
        indent,
        indent_
    ) where

import qualified Text.XML.Expat.Internal.DocumentClass as Doc
import Text.XML.Expat.Internal.NodeClass
import Text.XML.Expat.SAX

import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Internal (c2w, w2c)
import Data.Char (isSpace)
import Data.List.Class (List(..), ListItem(..), fromList)
import Data.Monoid
import Data.Word
import Data.Text (Text)
import Text.XML.Expat.Tree (UNode)

-- | Format document with <?xml.. header - lazy variant that returns lazy ByteString.
format :: (NodeClass n [], GenericXMLString tag, GenericXMLString text) =>
          n [] tag text
       -> L.ByteString
format node = L.fromChunks (xmlHeader : formatNodeG node)
{-# SPECIALIZE format :: UNode Text -> L.ByteString #-}

-- | Format document with <?xml.. header - generalized variant that returns a generic
-- list of strict ByteStrings.
formatG :: (NodeClass n c, GenericXMLString tag, GenericXMLString text) =>
          n c tag text
       -> c B.ByteString
formatG node = cons xmlHeader $ formatNodeG node

-- | Format document with <?xml.. header - strict variant that returns strict ByteString.
format' :: (NodeClass n [], GenericXMLString tag, GenericXMLString text) =>
           n [] tag text
        -> B.ByteString
format' = B.concat . L.toChunks . format

-- | Format XML node with no header - lazy variant that returns lazy ByteString.
formatNode :: (NodeClass n [], GenericXMLString tag, GenericXMLString text) =>
              n [] tag text
           -> L.ByteString
formatNode = formatSAX . treeToSAX

-- | Format XML node with no header - strict variant that returns strict ByteString.
formatNode' :: (NodeClass n [], GenericXMLString tag, GenericXMLString text) =>
               n [] tag text
            -> B.ByteString
formatNode' = B.concat . L.toChunks . formatNode

-- | Format XML node with no header - generalized variant that returns a generic
-- list of strict ByteStrings.
formatNodeG :: (NodeClass n c, GenericXMLString tag, GenericXMLString text) =>
              n c tag text
           -> c B.ByteString
formatNodeG = formatSAXG . treeToSAX
{-# SPECIALIZE formatNodeG :: UNode Text -> [B.ByteString] #-}

-- | Format an XML document - lazy variant that returns lazy ByteString.
formatDocument :: (Doc.DocumentClass d [], GenericXMLString tag, GenericXMLString text) =>
                  d [] tag text
               -> L.ByteString
formatDocument = formatSAX . documentToSAX

-- | Format an XML document - strict variant that returns strict ByteString.
formatDocument' :: (Doc.DocumentClass d [], GenericXMLString tag, GenericXMLString text) =>
                   d [] tag text
                -> B.ByteString
formatDocument' = B.concat . L.toChunks . formatDocument

-- | Format an XML document - generalized variant that returns a generic
-- list of strict ByteStrings.
formatDocumentG :: (Doc.DocumentClass d c, GenericXMLString tag, GenericXMLString text) =>
                   d c tag text
                -> c B.ByteString
formatDocumentG = formatSAXG . documentToSAX

-- | The standard XML header with UTF-8 encoding.
xmlHeader :: B.ByteString
xmlHeader = B.pack $ map c2w "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"

documentToSAX :: forall tag text d c . (GenericXMLString tag, GenericXMLString text,
                     Monoid text, Doc.DocumentClass d c) =>
                 d c tag text -> c (SAXEvent tag text)
documentToSAX doc =
    (case Doc.getXMLDeclaration doc of
        Just (Doc.XMLDeclaration ver mEnc sd) -> fromList [
                  XMLDeclaration ver mEnc sd, CharacterData (gxFromString "\n")]
        Nothing                               -> mzero) `mplus`
    join (fmap (\misc -> fromList [case misc of
            Doc.ProcessingInstruction target text -> ProcessingInstruction target text
            Doc.Comment text                      -> Comment text,
            CharacterData (gxFromString "\n")]
        ) (Doc.getTopLevelMiscs doc)) `mplus`
    treeToSAX (Doc.getRoot doc)

-- | Flatten a tree structure into SAX events, monadic version.
treeToSAX :: forall tag text n c . (GenericXMLString tag, GenericXMLString text,
                 Monoid text, NodeClass n c) =>
             n c tag text -> c (SAXEvent tag text)
treeToSAX node
    | isElement node =
        let name = getName node
            atts = getAttributes node
            children = getChildren node
            postpend :: c (SAXEvent tag text) -> c (SAXEvent tag text)
            postpend l = joinL $ do
                li <- runList l
                return $ case li of
                    Nil -> singleton (EndElement name)
                    Cons n l' -> cons n (postpend l')
        in  cons (StartElement name atts) $
            postpend (concatL $ fmap treeToSAX children)
    | isCData node =
        cons StartCData (cons (CharacterData $ getText node) (singleton EndCData))
    | isText node =
        singleton (CharacterData $ getText node)        
    | isProcessingInstruction node =
        singleton (ProcessingInstruction (getTarget node) (getText node))
    | isComment node =
        singleton (Comment $ getText node)    
    | otherwise = mzero
  where
    singleton = return
    concatL = join
{-# SPECIALIZE treeToSAX :: UNode Text -> [(SAXEvent Text Text)] #-}

-- | Format SAX events with no header - lazy variant that returns lazy ByteString.
formatSAX :: (GenericXMLString tag, GenericXMLString text) =>
             [SAXEvent tag text]
          -> L.ByteString
formatSAX = L.fromChunks . formatSAXG

-- | Format SAX events with no header - strict variant that returns strict ByteString.
formatSAX' :: (GenericXMLString tag, GenericXMLString text) =>
              [SAXEvent tag text]
           -> B.ByteString
formatSAX' = B.concat . formatSAXG

-- Do start tag and attributes but omit closing >
startTagHelper :: (GenericXMLString tag, GenericXMLString text) =>
                  tag
               -> [(tag, text)]
               -> [B.ByteString]
startTagHelper name atts =
    B.singleton (c2w '<'):
    gxToByteString name:
    Prelude.concatMap (
            \(aname, avalue) ->
                B.singleton (c2w ' '):
                gxToByteString aname:
                pack "=\"":
                escapeText (gxToByteString avalue)++
                [B.singleton (c2w '"')]
        ) atts

-- | Format SAX events with no header - generalized variant that uses generic
-- list.
formatSAXG :: forall c tag text . (List c, GenericXMLString tag,
              GenericXMLString text) =>
          c (SAXEvent tag text)    -- ^ SAX events
       -> c B.ByteString
formatSAXG l1 = formatSAXGb l1 False
{-# SPECIALIZE formatSAXG :: [SAXEvent Text Text] -> [B.ByteString] #-}

formatSAXGb :: forall c tag text . (List c, GenericXMLString tag,
              GenericXMLString text) =>
          c (SAXEvent tag text)    -- ^ SAX events
       -> Bool                     -- ^ True if processing CDATA
       -> c B.ByteString
formatSAXGb l1 cd = joinL $ do
    it1 <- runList l1
    return $ formatItem it1
  where
    formatItem it1 = case it1 of
        Nil -> mzero
        Cons (XMLDeclaration ver mEnc mSD) l2 ->
            return (pack "<?xml version=\"") `mplus`
            fromList (escapeText (gxToByteString ver)) `mplus`
            return (pack "\"") `mplus`
            (
                case mEnc of
                    Nothing -> mzero
                    Just enc ->
                        return (pack " encoding=\"") `mplus`
                        fromList (escapeText (gxToByteString enc)) `mplus`
                        return (pack "\"")
            ) `mplus`
            (
                case mSD of
                    Nothing -> mzero
                    Just True  -> return (pack " standalone=\"yes\"")
                    Just False -> return (pack " standalone=\"no\"")
            ) `mplus`
            return (pack ("?>"))
            `mplus`
            formatSAXGb l2 cd
        Cons (StartElement name attrs) l2 ->
            fromList (startTagHelper name attrs)
            `mplus` (
                joinL $ do
                    it2 <- runList l2
                    return $ case it2 of
                        Cons (EndElement _) l3 ->
                            cons (pack "/>") $
                            formatSAXGb l3 cd
                        _ ->
                            cons (B.singleton (c2w '>')) $
                            formatItem it2
            )
        Cons (EndElement name) l2 ->
            cons (pack "</") $
            cons (gxToByteString name) $
            cons (B.singleton (c2w '>')) $
            formatSAXGb l2 cd
        Cons (CharacterData txt) l2 ->
            (if cd then
                fromList [gxToByteString txt]
             else
                fromList (escapeText (gxToByteString txt))
            ) `mplus` (formatSAXGb l2 cd)
        Cons StartCData l2 ->
            cons(pack "<![CDATA[") $
            formatSAXGb l2 True
        Cons EndCData l2 ->
            cons(pack "]]>") $
            formatSAXGb l2 False
        Cons (ProcessingInstruction target txt) l2 ->
            cons (pack "<?") $
            cons (gxToByteString target) $
            cons (pack " ") $
            cons (gxToByteString txt) $
            cons (pack "?>") $
            formatSAXGb l2 cd
        Cons (Comment txt) l2 ->
            cons (pack "<!--") $
            cons (gxToByteString txt) $
            cons (pack "-->") $
            formatSAXGb l2 cd
        Cons (FailDocument _) l2 ->
            formatSAXGb l2 cd
{-# SPECIALIZE formatSAXGb :: [SAXEvent Text Text] -> Bool -> [B.ByteString] #-}

pack :: String -> B.ByteString
pack = B.pack . map c2w

isSafeChar :: Word8 -> Bool
isSafeChar c =
     (c /= c2w '&')
  && (c /= c2w '<')
  && (c /= c2w '>')
  && (c /= c2w '"')
  && (c /= c2w '\'')
{-# INLINE isSafeChar #-}

escapeText :: B.ByteString -> [B.ByteString]
escapeText str | B.null str = []
escapeText str =
    let (good, bad) = B.span isSafeChar str
    in  if B.null good
            then case w2c $ B.head str of
                '&'  -> pack "&amp;":escapeText rema
                '<'  -> pack "&lt;":escapeText rema
                '>'  -> pack "&gt;":escapeText rema
                '"'  -> pack "&quot;":escapeText rema
                '\'' -> pack "&apos;":escapeText rema
                _        -> error "hexpat: impossible"
            else good:escapeText bad
  where
    rema = B.tail str

-- | Make the output prettier by adding indentation.
indent :: (NodeClass n c, GenericXMLString tag, GenericXMLString text) =>
          Int   -- ^ Number of indentation spaces per nesting level
       -> n c tag text
       -> n c tag text
indent = indent_ 0

-- | Make the output prettier by adding indentation, specifying initial indent.
indent_ :: forall n c tag text . (NodeClass n c, GenericXMLString tag, GenericXMLString text) =>
           Int   -- ^ Initial indent (spaces)
        -> Int   -- ^ Number of indentation spaces per nesting level
        -> n c tag text
        -> n c tag text
indent_ cur perLevel elt | isElement elt =
    flip modifyChildren elt $ \chs -> joinL $ do
        (anyElts, chs') <- anyElements [] chs
        -- The new list chs' is the same as the old list chs, but some of its
        -- nodes have been loaded into memory.  This is to avoid evaluating
        -- list elements twice.
        if anyElts
            then addSpace True chs'
            else return chs'
  where
    addSpace :: Bool -> c (n c tag text) -> ItemM c (c (n c tag text))
    addSpace startOfText l = do
        ch <- runList l
        case ch of
            Nil -> return $ singleton (mkText $ gxFromString ('\n':replicate cur ' '))
            Cons elt l' | isElement elt -> do
                let cur' = cur + perLevel
                return $
                    cons (mkText $ gxFromString ('\n':replicate cur' ' ')) $
                    cons (indent_ cur' perLevel elt) $
                    joinL (addSpace True l')

            Cons tx l' | isText tx && startOfText ->
                case strip (getText tx) of
                    Nothing -> addSpace True l'
                    Just t' -> return $
                        cons (mkText t') $
                        joinL $ addSpace False l'
            Cons n l' ->
                return $
                    cons n $
                    joinL $ addSpace False l'

    -- acc is used to keep the nodes we've scanned into memory.
    -- We then construct a new list that looks the same as the old list, but
    -- which starts with the nodes in memory, to prevent the list being
    -- demanded more than once (in case it's monadic and it's expensive to
    -- evaluate).
    anyElements :: [n c tag text]   -- ^ Accumulator for tags we've looked at.
                -> c (n c tag text)
                -> ItemM c (Bool, c (n c tag text))
    anyElements acc l = do
        n <- runList l
        case n of
            Nil                     -> return (False, instantiatedList acc mzero)
            Cons n l' | isElement n -> return (True,  instantiatedList (n:acc) l')
            Cons n l'               -> anyElements (n:acc) l'
      where
        instantiatedList :: [n c tag text] -> c (n c tag text) -> c (n c tag text)
        instantiatedList acc l' = reverse acc `prepend` l'

        prepend :: forall a . [a] -> c a -> c a
        prepend xs l = foldr cons l xs

    strip t | gxNullString t = Nothing
    strip t | isSpace (gxHead t) = strip (gxTail t)
    strip t = Just t

    singleton = return
indent_ _ _ n = n

