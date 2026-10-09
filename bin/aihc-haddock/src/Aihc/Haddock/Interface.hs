{-# LANGUAGE OverloadedStrings #-}

-- | JSON adapters for resolver identities and interfaces.
module Aihc.Haddock.Interface
  ( DocName (..),
    DocInterface (..),
    interfaceNames,
  )
where

import Aihc.Parser.Syntax (FixityAssoc (..))
import Aihc.Resolve
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

newtype DocName = DocName {unDocName :: GlobalName}
  deriving (Eq, Ord, Show)

newtype DocInterface = DocInterface {unDocInterface :: Exports}
  deriving (Eq)

instance Show DocInterface where
  showsPrec precedence (DocInterface exports) = showsPrec precedence (exportsEntries exports)

instance ToJSON DocName where
  toJSON (DocName name) =
    object
      [ "package" .= packageIdText (globalNamePackage name),
        "module" .= globalNameModule name,
        "name" .= globalNameText name,
        "namespace" .= namespaceText (globalNameNamespace name)
      ]

instance FromJSON DocName where
  parseJSON = withObject "declaration identity" $ \obj -> do
    namespace <- obj .: "namespace" >>= parseNamespace
    DocName <$> (GlobalName <$> obj .: "name" <*> (PackageId <$> obj .: "package") <*> obj .: "module" <*> pure namespace)

namespaceText :: ResolutionNamespace -> Text
namespaceText namespace = case namespace of
  ResolutionNamespaceTerm -> "value"
  ResolutionNamespaceType -> "type"
  ResolutionNamespaceModule -> "module"

parseNamespace :: Text -> Parser ResolutionNamespace
parseNamespace text = case text of
  "value" -> pure ResolutionNamespaceTerm
  "type" -> pure ResolutionNamespaceType
  "module" -> pure ResolutionNamespaceModule
  _ -> fail "Unknown resolver namespace"

instance ToJSON DocInterface where
  toJSON (DocInterface exports) = toJSON (map entryJson (exportsEntries exports))
    where
      entryJson entry = case entry of
        ExportTerm name entity -> named "term" name ["entity" .= entityJson entity]
        ExportType name entity -> named "type" name ["entity" .= entityJson entity]
        ExportConstructors name members -> membersJson "constructors" name members
        ExportRecordFields name members -> membersJson "fields" name members
        ExportMethods name members -> membersJson "methods" name members
        ExportAssociatedTypes name members -> membersJson "associated_types" name members
        ExportFixity name (OperatorFixity assoc precedence) -> named "fixity" name ["associativity" .= assocText assoc, "precedence" .= precedence]
      membersJson tag name members = named tag name ["members" .= members]
      named tag name fields = object (["tag" .= (tag :: Text), "name" .= name] <> fields)
      entityJson entity = case entity of
        EntityGlobal name -> object ["global" .= DocName name]
        EntityLocal (LocalId unique) -> object ["local" .= unique]
        EntitySyntax -> object ["syntax" .= True]

instance FromJSON DocInterface where
  parseJSON value = DocInterface . exportsFromEntries <$> (parseJSON value >>= traverse parseEntry)
    where
      parseEntry = withObject "resolver export" $ \obj -> do
        tag <- obj .: "tag" :: Parser Text
        name <- obj .: "name"
        case tag of
          "term" -> ExportTerm name <$> (obj .: "entity" >>= parseEntity)
          "type" -> ExportType name <$> (obj .: "entity" >>= parseEntity)
          "constructors" -> ExportConstructors name <$> obj .: "members"
          "fields" -> ExportRecordFields name <$> obj .: "members"
          "methods" -> ExportMethods name <$> obj .: "members"
          "associated_types" -> ExportAssociatedTypes name <$> obj .: "members"
          "fixity" -> ExportFixity name <$> (OperatorFixity <$> (obj .: "associativity" >>= parseAssoc) <*> obj .: "precedence")
          _ -> fail "Unknown resolver export"
      parseEntity = withObject "resolver entity" $ \obj -> do
        global <- obj .:? "global"
        local <- obj .:? "local"
        syntax <- obj .:? "syntax" .!= False
        case (global, local, syntax) of
          (Just name, Nothing, False) -> pure (EntityGlobal (unDocName name))
          (Nothing, Just unique, False) -> pure (EntityLocal (LocalId unique))
          (Nothing, Nothing, True) -> pure EntitySyntax
          _ -> fail "Invalid resolver entity"

-- | Keep the resolver's term order, then its type order.
interfaceNames :: Exports -> [GlobalName]
interfaceNames exports =
  [name | EntityGlobal name <- Map.elems (exportedTerms exports) <> Map.elems (exportedTypes exports)]

assocText :: FixityAssoc -> Text
assocText assoc = case assoc of
  Infix -> "infix"
  InfixL -> "infixl"
  InfixR -> "infixr"

parseAssoc :: Text -> Parser FixityAssoc
parseAssoc text = case text of
  "infix" -> pure Infix
  "infixl" -> pure InfixL
  "infixr" -> pure InfixR
  _ -> fail "Unknown fixity association"
