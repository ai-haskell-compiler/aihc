{-# LANGUAGE OverloadedStrings #-}

-- | An abridged parse tree.
--
-- The tree shows the shape of a module to a reader who has never seen a
-- parser. Thus it leaves out what the compiler keeps for itself: source
-- spans, annotations, the exact numeric type of a literal, and the other
-- small tags on a node. Lists and 'Maybe' values are transparent, so a
-- node has its real children and no cons cells.
--
-- The walk is generic over the 'Data' instances of the syntax tree. A new
-- constructor in the parser shows up here without a change.
module Aihc.Play.Tree
  ( Tree (..),
    abridgedTree,
    treeJson,
  )
where

import Aihc.Parser.Syntax (Module, Name (..), UnqualifiedName (..))
import Aihc.Play.Json (Json (..))
import Data.Data (Data, cast, gmapQ, showConstr, toConstr, tyConName, typeRepTyCon)
import Data.List (isSuffixOf)
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (typeOf)

data Tree = Tree
  { treeLabel :: !Text,
    treeText :: !(Maybe Text),
    treeChildren :: ![Tree]
  }
  deriving (Eq, Show)

abridgedTree :: Module -> Tree
abridgedTree modu =
  case nodes modu of
    [tree] -> tree
    trees -> Tree "Module" Nothing trees

treeJson :: Tree -> Json
treeJson tree =
  JObject
    ( [("label", JString (treeLabel tree))]
        <> [("text", JString text) | Just text <- [treeText tree]]
        <> [("children", JArray (map treeJson (treeChildren tree))) | not (null (treeChildren tree))]
    )

-- | A leaf that holds a piece of text of its parent, such as the spelling
-- of a literal. The parent absorbs it.
textLeaf :: Text -> Tree
textLeaf text = Tree textLabel (Just text) []

textLabel :: Text
textLabel = "\0text"

nodes :: forall a. (Data a) => a -> [Tree]
nodes value
  | Just (name :: Name) <- cast value = [Tree "Name" (Just (qualifiedName name)) []]
  | Just (name :: UnqualifiedName) <- cast value = [Tree "Name" (Just (unqualifiedNameText name)) []]
  | Just (leaf :: Text) <- cast value = [textLeaf leaf]
  | Set.member typeName hiddenTypes = []
  | Set.member constructor transparentConstructors = children
  | "Ann" `isSuffixOf` constructor = children
  | otherwise = [absorb (Tree (T.pack constructor) text others)]
  where
    typeName = tyConName (typeRepTyCon (typeOf value))
    constructor = showConstr (toConstr value)
    children = concat (gmapQ nodes value)
    texts = mapMaybe treeText (filter ((== textLabel) . treeLabel) children)
    others = filter ((/= textLabel) . treeLabel) children
    -- A literal keeps its value and its spelling. The spelling comes last
    -- and is what the reader wrote.
    text = case texts of
      [] -> Nothing
      _ -> Just (last texts)

-- | A node whose only child is a name takes the name as its own text.
-- @EVar x@ then shows as one node, not as two.
absorb :: Tree -> Tree
absorb tree =
  case (treeText tree, treeChildren tree) of
    (Nothing, [Tree "Name" (Just name) []]) -> tree {treeText = Just name, treeChildren = []}
    _ -> tree

qualifiedName :: Name -> Text
qualifiedName name =
  case nameQualifier name of
    Just qualifier -> qualifier <> "." <> nameText name
    Nothing -> nameText name

-- | Types that the tree leaves out. Most are tags that the parser keeps for
-- later phases, such as the syntactic category of a name.
hiddenTypes :: Set.Set String
hiddenTypes =
  Set.fromList
    [ "Annotation",
      "SourceSpan",
      "NameType",
      "NumericType",
      "FloatType",
      "TupleFlavor",
      "MultiplicityTag",
      "MatchHeadForm",
      "TyVarBSpecificity",
      "TyVarBVisibility",
      "ExtensionSetting",
      "Extension",
      "Bool",
      "Int",
      "Integer",
      "Ratio",
      "Char",
      "Double"
    ]

-- | Containers whose structure is not part of the syntax.
transparentConstructors :: Set.Set String
transparentConstructors =
  Set.fromList ["(:)", "[]", "Just", "Nothing", "(,)", "(,,)", "Left", "Right"]
