{-# LANGUAGE FlexibleContexts #-}
module Text.XML.Expat.Internal.Namespaced
      ( NName (..)
      , NAttributes
      , mkNName
      , mkAnNName
      , toNamespaced
      , fromNamespaced
      , xmlnsUri
      , xmlns
      ) where

import Text.XML.Expat.Internal.NodeClass
import Text.XML.Expat.Internal.Qualified
import Text.XML.Expat.SAX
import Control.DeepSeq
import qualified Data.Map as M
import qualified Data.Maybe as DM
import qualified Data.List as L

-- | A namespace-qualified tag.
--
-- NName has two components, a local part and an optional namespace. The local part is the
-- name of the tag. The namespace is the URI identifying collections of declared tags.
-- Tags with the same local part but from different namespaces are distinct. Unqualified tags
-- are those with no namespace. They are in the default namespace, and all uses of an
-- unqualified tag are equivalent.
data NName text =
    NName {
        nnNamespace :: Maybe text,
        nnLocalPart :: !text
    }
    deriving (Eq,Show)

instance NFData text => NFData (NName text) where
    rnf (NName ns loc) = rnf (ns, loc)

-- | Type shortcut for attributes with namespaced names
type NAttributes text = Attributes (NName text) text

-- | Make a new NName from a prefix and localPart.
mkNName :: text -> text -> NName text
mkNName prefix localPart = NName (Just prefix) localPart

-- | Make a new NName with no prefix.
mkAnNName :: text -> NName text
mkAnNName localPart = NName Nothing localPart

type NsPrefixMap text = M.Map (Maybe text) (Maybe text)
type PrefixNsMap text = M.Map (Maybe text) (Maybe text)

xmlUri :: (GenericXMLString text) => text
xmlUri = gxFromString "http://www.w3.org/XML/1998/namespace"
xml :: (GenericXMLString text) => text
xml = gxFromString "xml"

xmlnsUri :: (GenericXMLString text) => text
xmlnsUri = gxFromString "http://www.w3.org/2000/xmlns/"
xmlns :: (GenericXMLString text) => text
xmlns = gxFromString "xmlns"

baseNsBindings :: (GenericXMLString text, Ord text)
               => NsPrefixMap text
baseNsBindings = M.fromList
  [ (Nothing, Nothing) 
  , (Just xml, Just xmlUri)
  , (Just xmlns, Just xmlnsUri)
  ]

basePfBindings :: (GenericXMLString text, Ord text)
               => PrefixNsMap text
basePfBindings = M.fromList
   [ (Nothing, Nothing)
   , (Just xmlUri, Just xml)
   , (Just xmlnsUri, Just xmlns)
   ]

toNamespaced :: (NodeClass n c, GenericXMLString text, Ord text, Show text)
               => n c (QName text) text -> n c (NName text) text
toNamespaced = nodeWithNamespaces baseNsBindings

nodeWithNamespaces :: (NodeClass n c, GenericXMLString text, Ord text, Show text)
                   => NsPrefixMap text -> n c (QName text) text -> n c (NName text) text
nodeWithNamespaces bindings = modifyElement namespaceify
  where
    namespaceify (qname, qattrs, qchildren) = (nname, nattrs, nchildren)
      where
        for = flip map
        ffor = flip fmap
        (nsAtts, otherAtts) = L.partition ((== Just xmlns) . qnPrefix . fst) qattrs
        (dfAtt, normalAtts) = L.partition ((== QName Nothing xmlns) . fst) otherAtts
        nsMap  = M.fromList $ for nsAtts $ \((QName _ lp), uri) -> (Just lp, Just uri)
        -- fixme: when snd q is null, use Nothing
        dfMap  = M.fromList $ for dfAtt $ \q -> (Nothing, Just $ snd q)
        chldBs = M.unions [dfMap, nsMap, bindings]
    
        trans bs (QName pref qual) = case pref `M.lookup` bs of
          Nothing -> error 
                  $  "Namespace prefix referenced but never bound: '"
                  ++ (show . DM.fromJust) pref
                  ++ "'"
          Just mUri -> NName mUri qual
        nname       = trans chldBs qname
    
        -- attributes with no prefix are in the same namespace as the element
        attBs = M.insert Nothing (nnNamespace nname) chldBs
    
        transAt (qn, v) = (trans attBs qn, v)
    
        nNsAtts     = map transAt nsAtts
        nDfAtt      = map transAt dfAtt
        nNormalAtts = map transAt normalAtts
        nattrs      = concat [nNsAtts, nDfAtt, nNormalAtts]

        nchildren   = ffor qchildren $ nodeWithNamespaces chldBs

fromNamespaced :: (NodeClass n c, GenericXMLString text, Ord text, Functor c) =>
                  n c (NName text) text -> n c (QName text) text
fromNamespaced = nodeWithQualifiers 1 basePfBindings

nodeWithQualifiers :: (NodeClass n c, GenericXMLString text, Ord text, Functor c) =>
                      Int
                   -> PrefixNsMap text
                   -> n c (NName text) text
                   -> n c (QName text) text
nodeWithQualifiers cntr bindings = modifyElement namespaceify
  where
    namespaceify (nname, nattrs, nchildren) = (qname, qattrs, qchildren) 
      where
        for = flip map
        ffor = flip fmap
        (nsAtts, otherAtts) = L.partition ((== Just xmlnsUri) . nnNamespace . fst) nattrs
        (dfAtt, normalAtts) = L.partition ((== NName Nothing xmlns) . fst) otherAtts
        nsMap = M.fromList $ for nsAtts $ \((NName _ lp), uri) -> (Just uri, Just lp)
        dfMap = M.fromList $ for dfAtt  $ \(_, uri) -> (Just uri, Just xmlns)
        chldBs = M.unions [dfMap, nsMap, bindings]
    
        trans (i, bs, as) (NName nspace qual) =
          case nspace `M.lookup` bs of
               Nothing -> let
                            pfx = gxFromString $ "ns" ++ show i
                            bsN = M.insert nspace (Just pfx) bs
                            asN = (NName (Just xmlnsUri) pfx, DM.fromJust nspace) : as
                          in trans (i+1, bsN, asN) (NName nspace qual)
               Just pfx -> ((i, bs, as), QName pfx qual)
        transAt ibs (nn, v) = let (ibs', qn) = trans ibs nn
                              in  (ibs', (qn, v))
    
        ((i', bs', as'), qname) = trans (cntr, chldBs, []) nname
    
        ((i'',   bs'',   as''),   qNsAtts)     = L.mapAccumL transAt (i',    bs',    as')    nsAtts
        ((i''',  bs''',  as'''),  qDfAtt)      = L.mapAccumL transAt (i'',   bs'',   as'')   dfAtt
        ((i'''', bs'''', as''''), qNormalAtts) = L.mapAccumL transAt (i''',  bs''',  as''')  normalAtts
        (_,                       qas)         = L.mapAccumL transAt (i'''', bs'''', as'''') as''''
        qattrs = concat [qNsAtts, qDfAtt, qNormalAtts, qas]
    
        qchildren = ffor nchildren $ nodeWithQualifiers i'''' bs''''
