module XmlParser.NameMap
  ( NameMap,
    fromNodes,
    fromAttributes,
    empty,
    insert,
    fetch,
    extractNames,
  )
where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import qualified Text.XML as Xml
import qualified XmlParser.NamespaceRegistry as NamespaceRegistry
import XmlParser.Prelude hiding (empty, fromList, insert, toList)
import qualified XmlParser.TupleHashMap as TupleHashMap

data NameMap a
  = NameMap
      -- | Namespaced
      (TupleHashMap.TupleHashMap Text Text [a])
      -- | Unnamespaced
      (HashMap Text [a])

fromNodes :: NamespaceRegistry.NamespaceRegistry -> [Xml.Node] -> NameMap Xml.Element
fromNodes nreg =
  fromReverseList (flip NamespaceRegistry.resolveElementName nreg) . foldl' appendIfElement []
  where
    appendIfElement list = \case
      Xml.NodeElement element -> (Xml.elementName element, element) : list
      _ -> list

fromAttributes :: NamespaceRegistry.NamespaceRegistry -> Map Xml.Name Text -> NameMap Text
fromAttributes nreg =
  fromList (flip NamespaceRegistry.resolveAttributeName nreg) . Map.toList

fromList :: (Xml.Name -> Maybe (Maybe Text, Text)) -> [(Xml.Name, a)] -> NameMap a
fromList resolve =
  fromReverseList resolve . reverse

fromReverseList :: (Xml.Name -> Maybe (Maybe Text, Text)) -> [(Xml.Name, a)] -> NameMap a
fromReverseList resolve list =
  foldr step NameMap list TupleHashMap.empty HashMap.empty
  where
    step (name, contents) next !map1 !map2 =
      case resolve name of
        Nothing -> next map1 map2
        Just (ns, name) ->
          case ns of
            Just ns -> next (TupleHashMap.insertSemigroup ns name [contents] map1) map2
            Nothing -> next map1 (HashMap.insertWith (++) name [contents] map2)

empty :: NameMap a
empty =
  NameMap TupleHashMap.empty HashMap.empty

insert :: Maybe Text -> Text -> a -> NameMap a -> NameMap a
insert ns name contents (NameMap map1 map2) =
  case ns of
    Just ns ->
      NameMap (TupleHashMap.insertSemigroup ns name [contents] map1) map2
    Nothing ->
      NameMap map1 (HashMap.insertWith (++) name [contents] map2)

fetch :: Maybe Text -> Text -> NameMap a -> Maybe (a, NameMap a)
fetch ns name (NameMap map1 map2) =
  case ns of
    Just ns ->
      TupleHashMap.alterF
        ( \case
            Just list ->
              case list of
                head : tail -> case tail of
                  [] -> Compose (Just (head, Nothing))
                  _ -> Compose (Just (head, Just tail))
                _ -> Compose Nothing
            Nothing ->
              Compose Nothing
        )
        ns
        name
        map1
        & getCompose
        & fmap (fmap (flip NameMap map2))
    Nothing ->
      HashMap.alterF
        ( \case
            Just list ->
              case list of
                head : tail -> case tail of
                  [] -> Compose (Just (head, Nothing))
                  _ -> Compose (Just (head, Just tail))
                _ -> Compose Nothing
            Nothing ->
              Compose Nothing
        )
        name
        map2
        & getCompose
        & fmap (fmap (NameMap map1))

extractNames :: NameMap a -> [(Maybe Text, Text)]
extractNames (NameMap map1 map2) =
  fmap (\(a, b, _) -> (Just a, b)) (TupleHashMap.toList map1)
    <> fmap ((Nothing,) . fst) (HashMap.toList map2)
