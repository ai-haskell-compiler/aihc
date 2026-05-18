{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
--
-- Module      : Aihc.Parser.Syntax
-- Description : Abstract Syntax Tree
-- License     : Unlicense
--
-- Abstract Syntax Tree (AST) covering Haskell2010 plus all language extensions.
module Aihc.Parser.Syntax
  ( ArithSeq (..),
    ArrowKind (..),
    ArrAppType (..),
    BangType (..),
    BinderName,
    BinderHead (..),
    CallConv (..),
    CaseAlt (..),
    ClassDecl (..),
    ClassDeclItem (..),
    Cmd (..),
    CompStmt (..),
    FunctionalDependency (..),
    TypeFamilyResultSig (..),
    TypeFamilyInjectivity (..),
    TypeHeadForm (..),
    DataConDecl (..),
    DataDecl (..),
    Decl (..),
    DerivingClause (..),
    DerivingStrategy (..),
    DoFlavor (..),
    DoStmt (..),
    Expr (..),
    Extension (..),
    ExtensionSetting (..),
    ExportSpec (..),
    IEEntityNamespace (..),
    IEBundledNamespace (..),
    IEBundledMember (..),
    FieldDecl (..),
    FixityAssoc (..),
    ForeignDecl (..),
    ForeignDirection (..),
    ForeignEntitySpec (..),
    ForeignSafety (..),
    GadtBody (..),
    GuardQualifier (..),
    GuardedRhs (..),
    ImportDecl (..),
    ImportLevel (..),
    ImportItem (..),
    ImportSpec (..),
    InstanceDecl (..),
    InstanceDeclItem (..),
    InstanceOverlapPragma (..),
    LanguageEdition (..),
    LambdaCaseAlt (..),
    Literal (..),
    Match (..),
    MatchHeadForm (..),
    MultiplicityTag (..),
    Module (..),
    ModuleHead (..),
    ModuleHeaderPragmas (..),
    Name (..),
    NameType (..),
    UnqualifiedName (..),
    InstanceHeadType,
    Annotation,
    NewtypeDecl (..),
    OperatorName,
    PatSynArgs (..),
    PatSynDecl (..),
    PatSynDir (..),
    Pattern (..),
    RecordField (..),
    Pragma (..),
    PragmaType (..),
    PragmaUnpackKind (..),
    Role (..),
    RoleAnnotation (..),
    Rhs (..),
    SourceSpan (..),
    StandaloneDerivingDecl (..),
    Type (..),
    TupleFlavor (..),
    TypeSyntaxForm (..),
    FloatType (..),
    NumericType (..),
    TypeLiteral (..),
    TypeBuiltinCon (..),
    TypePromotion (..),
    ForallVis (..),
    ForallTelescope (..),
    TyVarBSpecificity (..),
    TyVarBVisibility (..),
    TyVarBinder (..),
    TypeSynDecl (..),
    TypeFamilyDecl (..),
    TypeFamilyEq (..),
    DataFamilyDecl (..),
    TypeFamilyInst (..),
    DataFamilyInst (..),
    ValueDecl (..),
    allKnownExtensions,
    applyExtensionSetting,
    applyImpliedExtensions,
    binderHeadForm,
    binderHeadName,
    binderHeadParams,
    effectiveExtensions,
    extensionName,
    extensionSettingName,
    gadtBodyResultType,
    languageEditionExtensions,
    editionFromExtensionSettings,
    noSourceSpan,
    mergeSourceSpans,
    mkName,
    mkUnqualifiedName,
    nameFromText,
    parseExtensionName,
    parseExtensionSettingName,
    parseLanguageEdition,
    qualifyName,
    renderName,
    renderUnqualifiedName,
    unqualifiedNameFromText,
    moduleName,
    moduleWarningPragma,
    moduleExports,
    instanceHeadName,
    instanceHeadTypes,
    mkAnnotation,
    fromAnnotation,
    stripAnnotations,
    peelArithSeqAnn,
    peelClassDeclItemAnn,
    peelCmdAnn,
    peelCompStmtAnn,
    peelDataConAnn,
    peelDeclAnn,
    peelDoStmtAnn,
    peelExprAnn,
    peelGuardQualifierAnn,
    peelInstanceDeclItemAnn,
    peelPatternAnn,
    peelTypeAnn,
    peelTypeHead,
    literalAnnSpan,
    peelLiteralAnn,
    typeAnnSpan,
  )
where

import Control.DeepSeq (NFData (..))
import Data.Char (GeneralCategory (..), generalCategory)
import Data.Data (Constr, Data (..), DataType, Fixity (Prefix), mkConstr, mkDataType)
import Data.Dynamic (Dynamic, Typeable, fromDynamic, toDyn)
import Data.List (sort)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (cast)
import GHC.Generics (Generic)

-- | A @LANGUAGE@ pragma name.
-- Examples: @OverloadedStrings@ in @{-# LANGUAGE OverloadedStrings #-}@ and
-- @LambdaCase@ in @{-# LANGUAGE LambdaCase #-}@.
data Extension
  = AllowAmbiguousTypes
  | AlternativeLayoutRule
  | AlternativeLayoutRuleTransitional
  | ApplicativeDo
  | Arrows
  | AutoDeriveTypeable
  | BangPatterns
  | BinaryLiterals
  | BlockArguments
  | CApiFFI
  | ConstrainedClassMethods
  | ConstraintKinds
  | CPP
  | CUSKs
  | DataKinds
  | DatatypeContexts
  | DeepSubsumption
  | DefaultSignatures
  | DeriveAnyClass
  | DeriveDataTypeable
  | DeriveFoldable
  | DeriveFunctor
  | DeriveGeneric
  | DeriveLift
  | DeriveTraversable
  | DerivingStrategies
  | DerivingViaExtension
  | DisambiguateRecordFields
  | DoAndIfThenElse
  | DoRec
  | DuplicateRecordFields
  | EmptyCase
  | EmptyDataDecls
  | EmptyDataDeriving
  | ExistentialQuantification
  | ExplicitForAll
  | ExplicitLevelImports
  | ExplicitNamespaces
  | ExtensibleRecords
  | ExtendedDefaultRules
  | ExtendedLiterals
  | FieldSelectors
  | FlexibleContexts
  | FlexibleInstances
  | ForeignFunctionInterface
  | FunctionalDependencies
  | GADTs
  | GADTSyntax
  | Generics
  | GeneralizedNewtypeDeriving
  | GHC2021
  | GHC2024
  | GHCForeignImportPrim
  | Haskell2010
  | Haskell98
  | HereDocuments
  | HexFloatLiterals
  | ImplicitParams
  | ImplicitPrelude
  | ImplicitStagePersistence
  | ImportQualifiedPost
  | ImpredicativeTypes
  | IncoherentInstances
  | InstanceSigs
  | InterruptibleFFI
  | JavaScriptFFI
  | KindSignatures
  | LambdaCase
  | LexicalNegation
  | LiberalTypeSynonyms
  | LinearTypes
  | ListTuplePuns
  | MagicHash
  | MonadComprehensions
  | MonadFailDesugaring
  | MonoLocalBinds
  | MonoPatBinds
  | MonomorphismRestriction
  | MultilineStrings
  | MultiParamTypeClasses
  | MultiWayIf
  | NamedDefaults
  | NamedFieldPuns
  | NamedWildCards
  | NewQualifiedOperators
  | NegativeLiterals
  | NondecreasingIndentation
  | NPlusKPatterns
  | NullaryTypeClasses
  | NumDecimals
  | NumericUnderscores
  | OrPatterns
  | OverlappingInstances
  | OverloadedLabels
  | OverloadedLists
  | OverloadedRecordDot
  | OverloadedRecordUpdate
  | OverloadedStrings
  | PackageImports
  | ParallelArrays
  | ParallelListComp
  | PartialTypeSignatures
  | PatternSignatures
  | PatternGuards
  | PatternSynonyms
  | PolymorphicComponents
  | PolyKinds
  | PostfixOperators
  | QualifiedDo
  | QualifiedStrings
  | QuantifiedConstraints
  | QuasiQuotes
  | Rank2Types
  | RankNTypes
  | RebindableSyntax
  | RecordPuns
  | RecordWildCards
  | RecursiveDo
  | RegularPatterns
  | RelaxedLayout
  | RelaxedPolyRec
  | RestrictedTypeSynonyms
  | RequiredTypeArguments
  | RoleAnnotations
  | SafeImports
  | SafeHaskell
  | ScopedTypeVariables
  | StandaloneDeriving
  | StandaloneKindSignatures
  | StarIsType
  | StaticPointers
  | Strict
  | StrictData
  | TemplateHaskell
  | TemplateHaskellQuotes
  | TraditionalRecordSyntax
  | TransformListComp
  | Trustworthy
  | TupleSections
  | TypeAbstractions
  | TypeApplications
  | TypeData
  | TypeFamilies
  | TypeFamilyDependencies
  | TypeInType
  | TypeOperators
  | TypeSynonymInstances
  | UnboxedSums
  | UnboxedTuples
  | UndecidableInstances
  | UndecidableSuperClasses
  | UnicodeSyntax
  | UnliftedDatatypes
  | UnliftedFFITypes
  | UnliftedNewtypes
  | UnsafeHaskell
  | ViewPatterns
  | XmlSyntax
  deriving (Data, Eq, Ord, Show, Read, Enum, Bounded, Generic, NFData)

-- | A single @LANGUAGE@ pragma entry.
-- Examples: @EnableExtension LambdaCase@ for @{-# LANGUAGE LambdaCase #-}@ and
-- @DisableExtension ImplicitPrelude@ for @{-# LANGUAGE NoImplicitPrelude #-}@.
data ExtensionSetting
  = -- | @{-# LANGUAGE LambdaCase #-}@
    EnableExtension Extension
  | -- | @{-# LANGUAGE NoImplicitPrelude #-}@
    DisableExtension Extension
  deriving (Data, Eq, Ord, Show, Read, Generic, NFData)

-- | The Haskell language edition/standard.
-- Each edition implies a set of language extensions.
-- Examples: @{-# LANGUAGE Haskell2010 #-}@ and @{-# LANGUAGE GHC2021 #-}@.
data LanguageEdition
  = Haskell98Edition
  | Haskell2010Edition
  | GHC2021Edition
  | GHC2024Edition
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic, NFData)

-- | Pragmas extracted from a module header.
-- Contains the last language edition pragma (if any) and all extension settings.
-- Example: @{-# LANGUAGE Haskell2010, LambdaCase, NoImplicitPrelude #-}@.
data ModuleHeaderPragmas = ModuleHeaderPragmas
  { headerLanguageEdition :: Maybe LanguageEdition,
    headerExtensionSettings :: [ExtensionSetting]
  }
  deriving (Eq, Show, Generic, NFData)

allKnownExtensions :: [Extension]
allKnownExtensions = [minBound .. maxBound]

extensionName :: Extension -> Text
extensionName ext =
  case ext of
    DerivingViaExtension -> T.pack "DerivingVia"
    SafeHaskell -> T.pack "Safe"
    UnsafeHaskell -> T.pack "Unsafe"
    _ -> T.pack (show ext)

extensionSettingName :: ExtensionSetting -> Text
extensionSettingName setting =
  case setting of
    EnableExtension ext -> extensionName ext
    DisableExtension ext -> T.pack "No" <> extensionName ext

parseExtensionName :: Text -> Maybe Extension
parseExtensionName raw =
  Map.lookup trimmed extensionMap
  where
    extensionMap :: Map.Map Text Extension
    extensionMap = Map.fromList $ aliases ++ [(T.pack (show ext), ext) | ext <- [minBound .. maxBound]]
    trimmed = T.strip raw
    aliases =
      [ ("Cpp", CPP),
        ("DerivingVia", DerivingViaExtension),
        ("GeneralisedNewtypeDeriving", GeneralizedNewtypeDeriving),
        ("Safe", SafeHaskell),
        ("Unsafe", UnsafeHaskell)
      ]

parseExtensionSettingName :: Text -> Maybe ExtensionSetting
parseExtensionSettingName raw =
  case T.stripPrefix (T.pack "No") trimmed of
    Just rest
      | not (T.null rest) ->
          case parseExtensionName rest of
            Just ext -> Just (DisableExtension ext)
            Nothing -> EnableExtension <$> parseExtensionName trimmed
    _ -> EnableExtension <$> parseExtensionName trimmed
  where
    trimmed = T.strip raw

-- | Parse a language edition name (e.g., "Haskell2010", "GHC2021").
parseLanguageEdition :: Text -> Maybe LanguageEdition
parseLanguageEdition raw
  | trimmed == T.pack "Haskell98" = Just Haskell98Edition
  | trimmed == T.pack "Haskell2010" = Just Haskell2010Edition
  | trimmed == T.pack "GHC2021" = Just GHC2021Edition
  | trimmed == T.pack "GHC2024" = Just GHC2024Edition
  | otherwise = Nothing
  where
    trimmed = T.strip raw

editionFromExtensionSettings :: [ExtensionSetting] -> Maybe LanguageEdition
editionFromExtensionSettings = worker Nothing
  where
    worker acc [] = acc
    worker _ (EnableExtension Haskell98 : rest) = worker (Just Haskell98Edition) rest
    worker _ (EnableExtension Haskell2010 : rest) = worker (Just Haskell2010Edition) rest
    worker _ (EnableExtension GHC2021 : rest) = worker (Just GHC2021Edition) rest
    worker _ (EnableExtension GHC2024 : rest) = worker (Just GHC2024Edition) rest
    worker acc (_ : rest) = worker acc rest

-- | Get the set of extensions enabled by a language edition.
-- These lists are derived from GHC's DynFlags.languageExtensions.
-- IMPORTANT: Keep these in sync with GHC. The test suite validates this.
languageEditionExtensions :: LanguageEdition -> [Extension]
languageEditionExtensions edition =
  case edition of
    Haskell98Edition ->
      -- Haskell98 has minimal extensions, mostly implicitly enabled features
      [ CUSKs,
        DeepSubsumption,
        DatatypeContexts,
        FieldSelectors,
        ImplicitPrelude,
        ImplicitStagePersistence,
        ListTuplePuns,
        MonomorphismRestriction,
        NondecreasingIndentation,
        NPlusKPatterns,
        StarIsType,
        TraditionalRecordSyntax
      ]
    Haskell2010Edition ->
      -- Haskell2010 adds a few more extensions over Haskell98
      [ CUSKs,
        DeepSubsumption,
        DatatypeContexts,
        DoAndIfThenElse,
        EmptyDataDecls,
        FieldSelectors,
        ForeignFunctionInterface,
        ImplicitPrelude,
        ImplicitStagePersistence,
        ListTuplePuns,
        MonomorphismRestriction,
        PatternGuards,
        RelaxedPolyRec,
        StarIsType,
        TraditionalRecordSyntax
      ]
    GHC2021Edition ->
      -- GHC2021 enables many modern convenience extensions
      [ BangPatterns,
        BinaryLiterals,
        ConstrainedClassMethods,
        ConstraintKinds,
        DeriveDataTypeable,
        DeriveFoldable,
        DeriveFunctor,
        DeriveGeneric,
        DeriveLift,
        DeriveTraversable,
        DoAndIfThenElse,
        EmptyCase,
        EmptyDataDecls,
        EmptyDataDeriving,
        ExistentialQuantification,
        ExplicitForAll,
        FieldSelectors,
        FlexibleContexts,
        FlexibleInstances,
        ForeignFunctionInterface,
        GADTSyntax,
        GeneralizedNewtypeDeriving,
        HexFloatLiterals,
        ImplicitPrelude,
        ImplicitStagePersistence,
        ImportQualifiedPost,
        InstanceSigs,
        KindSignatures,
        ListTuplePuns,
        MonomorphismRestriction,
        MultiParamTypeClasses,
        NamedFieldPuns,
        NamedWildCards,
        NumericUnderscores,
        PatternGuards,
        PolyKinds,
        PostfixOperators,
        RankNTypes,
        RelaxedPolyRec,
        ScopedTypeVariables,
        StandaloneDeriving,
        StandaloneKindSignatures,
        StarIsType,
        TraditionalRecordSyntax,
        TupleSections,
        TypeApplications,
        TypeOperators,
        TypeSynonymInstances
      ]
    GHC2024Edition ->
      -- GHC2024 adds more extensions on top of GHC2021
      [ BangPatterns,
        BinaryLiterals,
        ConstrainedClassMethods,
        ConstraintKinds,
        DataKinds,
        DeriveDataTypeable,
        DeriveFoldable,
        DeriveFunctor,
        DeriveGeneric,
        DeriveLift,
        DeriveTraversable,
        DerivingStrategies,
        DisambiguateRecordFields,
        DoAndIfThenElse,
        EmptyCase,
        EmptyDataDecls,
        EmptyDataDeriving,
        ExistentialQuantification,
        ExplicitForAll,
        ExplicitNamespaces,
        FieldSelectors,
        FlexibleContexts,
        FlexibleInstances,
        ForeignFunctionInterface,
        GADTs,
        GADTSyntax,
        GeneralizedNewtypeDeriving,
        HexFloatLiterals,
        ImplicitPrelude,
        ImplicitStagePersistence,
        ImportQualifiedPost,
        InstanceSigs,
        KindSignatures,
        LambdaCase,
        ListTuplePuns,
        MonoLocalBinds,
        MonomorphismRestriction,
        MultiParamTypeClasses,
        NamedFieldPuns,
        NamedWildCards,
        NumericUnderscores,
        PatternGuards,
        PolyKinds,
        PostfixOperators,
        RankNTypes,
        RelaxedPolyRec,
        RoleAnnotations,
        ScopedTypeVariables,
        StandaloneDeriving,
        StandaloneKindSignatures,
        StarIsType,
        TraditionalRecordSyntax,
        TupleSections,
        TypeApplications,
        TypeOperators,
        TypeSynonymInstances
      ]

-- FIXME: Verify against GHC's impliedXFlags
impliedExtensions :: [(Extension, [ExtensionSetting])]
impliedExtensions =
  [ (DeriveTraversable, [EnableExtension DeriveFunctor, EnableExtension DeriveFoldable]),
    (DerivingViaExtension, [EnableExtension DerivingStrategies]),
    (DuplicateRecordFields, [EnableExtension DisambiguateRecordFields]),
    (ExistentialQuantification, [EnableExtension ExplicitForAll]),
    (ExplicitLevelImports, [DisableExtension ImplicitStagePersistence]),
    (FlexibleInstances, [EnableExtension TypeSynonymInstances]),
    (FunctionalDependencies, [EnableExtension MultiParamTypeClasses]),
    (GADTs, [EnableExtension GADTSyntax, EnableExtension MonoLocalBinds]),
    (ImpredicativeTypes, [EnableExtension RankNTypes]),
    (IncoherentInstances, [EnableExtension OverlappingInstances]),
    (LiberalTypeSynonyms, [EnableExtension ExplicitForAll]),
    (LinearTypes, [EnableExtension MonoLocalBinds]),
    (MonadComprehensions, [EnableExtension ParallelListComp]),
    (MultiParamTypeClasses, [EnableExtension ConstrainedClassMethods]),
    (PolymorphicComponents, [EnableExtension RankNTypes]),
    (PolyKinds, [EnableExtension KindSignatures]),
    (QuantifiedConstraints, [EnableExtension ExplicitForAll]),
    (Rank2Types, [EnableExtension RankNTypes]),
    (RankNTypes, [EnableExtension ExplicitForAll]),
    (RebindableSyntax, [DisableExtension ImplicitPrelude]),
    (RecordWildCards, [EnableExtension DisambiguateRecordFields]),
    (ScopedTypeVariables, [EnableExtension ExplicitForAll]),
    (StandaloneKindSignatures, [DisableExtension CUSKs]),
    (Strict, [EnableExtension StrictData]),
    (TemplateHaskell, [EnableExtension TemplateHaskellQuotes]),
    (TypeFamilies, [EnableExtension ExplicitNamespaces, EnableExtension KindSignatures, EnableExtension MonoLocalBinds]),
    (TypeAbstractions, [EnableExtension TypeApplications]),
    (TypeFamilyDependencies, [EnableExtension TypeFamilies]),
    (TypeInType, [EnableExtension PolyKinds, EnableExtension DataKinds, EnableExtension KindSignatures]),
    (TypeOperators, [EnableExtension ExplicitNamespaces]),
    (RequiredTypeArguments, [EnableExtension TypeApplications]),
    (UnboxedTuples, [EnableExtension UnboxedSums]),
    (UnliftedDatatypes, [EnableExtension DataKinds, EnableExtension StandaloneKindSignatures])
  ]

applyExtensionSetting :: ExtensionSetting -> [Extension] -> [Extension]
applyExtensionSetting setting extensions =
  case setting of
    EnableExtension ext -> ext : filter (/= ext) extensions
    DisableExtension ext -> filter (/= ext) extensions

applyImpliedExtensions :: [Extension] -> [Extension]
applyImpliedExtensions extensions =
  let settings = concat $ mapMaybe (`lookup` impliedExtensions) extensions
      newExtensions = foldr applyExtensionSetting extensions settings
   in if sort newExtensions == sort extensions then extensions else applyImpliedExtensions newExtensions

effectiveExtensions :: LanguageEdition -> [ExtensionSetting] -> [Extension]
effectiveExtensions edition extensionSettings =
  applyImpliedExtensions $
    foldr applyExtensionSetting (languageEditionExtensions edition) extensionSettings

-- | Source location metadata for parsed syntax.
-- Example: the span covering @map@ in @map f xs@.
data SourceSpan
  = -- | No location information is available.
    NoSourceSpan
  | -- | A concrete span such as the token range for @map@ in @map f xs@.
    SourceSpan
      { sourceSpanSourceName :: !FilePath,
        sourceSpanStartLine :: !Int,
        sourceSpanStartCol :: !Int,
        sourceSpanEndLine :: !Int,
        sourceSpanEndCol :: !Int,
        sourceSpanStartOffset :: !Int,
        sourceSpanEndOffset :: !Int
      }
  deriving (Data, Eq, Ord, Generic, NFData)

instance Show SourceSpan where
  show NoSourceSpan = "NoSourceSpan"
  show SourceSpan {sourceSpanStartLine, sourceSpanStartCol, sourceSpanEndLine, sourceSpanEndCol} =
    "SourceSpan "
      ++ show sourceSpanStartLine
      ++ " "
      ++ show sourceSpanStartCol
      ++ " "
      ++ show sourceSpanEndLine
      ++ " "
      ++ show sourceSpanEndCol

noSourceSpan :: SourceSpan
noSourceSpan = NoSourceSpan

mergeSourceSpans :: SourceSpan -> SourceSpan -> SourceSpan
mergeSourceSpans left right =
  case (left, right) of
    ( SourceSpan name l1 c1 _ _ startOffset _,
      SourceSpan _ _ _ l2 c2 _ endOffset
      ) ->
        SourceSpan name l1 c1 l2 c2 startOffset endOffset
    (NoSourceSpan, span') -> span'
    (span', NoSourceSpan) -> span'

-- | A qualified or unqualified name with type information.
--
-- The 'nameQualifier' is the module path (e.g., \"Data.List\"), and
-- 'nameText' is the local name (e.g., \"map\"). For unqualified names,
-- 'nameQualifier' is 'Nothing'. Example: @Data.List.map@.
data Name = Name
  { -- | Module qualifier (e.g., @\"Data.List\"@ for @Data.List.map@)
    nameQualifier :: Maybe Text,
    -- | Whether this is a variable, constructor, or operator
    nameType :: NameType,
    -- | The local name (e.g., @\"map\"@, @\".+.\"@)
    nameText :: Text,
    -- | Metadata attached to this name occurrence.
    nameAnns :: [Annotation]
  }
  deriving (Eq, Show, Generic, NFData, Data)

-- | An unqualified identifier or operator.
-- Examples: @map@, @Just@, @:+:@.
data UnqualifiedName = UnqualifiedName
  { unqualifiedNameType :: NameType,
    unqualifiedNameText :: Text,
    unqualifiedNameAnns :: [Annotation]
  }
  deriving (Eq, Show, Generic, NFData, Data)

-- | The syntactic category of a name.
data NameType
  = -- | Variable identifier (e.g., @x@, @map@, @id@)
    NameVarId
  | -- | Constructor identifier (e.g., @Just@, @Maybe@)
    NameConId
  | -- | Variable operator (e.g., @+@, @.&.@)
    NameVarSym
  | -- | Constructor operator (e.g., @:@, @:++@)
    NameConSym
  deriving (Eq, Show, Generic, NFData, Enum, Bounded, Data)

mkName :: Maybe Text -> NameType -> Text -> Name
mkName qualifier ty txt = Name qualifier ty txt []

qualifyName :: Maybe Text -> UnqualifiedName -> Name
qualifyName qualifier name =
  Name qualifier (unqualifiedNameType name) (unqualifiedNameText name) (unqualifiedNameAnns name)

mkUnqualifiedName :: NameType -> Text -> UnqualifiedName
mkUnqualifiedName ty txt = UnqualifiedName ty txt []

renderName :: Name -> Text
renderName name =
  case nameQualifier name of
    Just qualifier -> qualifier <> "." <> nameText name
    Nothing -> nameText name

renderUnqualifiedName :: UnqualifiedName -> Text
renderUnqualifiedName = unqualifiedNameText

instance IsString Name where
  fromString = nameFromText . T.pack

instance IsString UnqualifiedName where
  fromString = unqualifiedNameFromText . T.pack

nameFromText :: Text -> Name
nameFromText txt =
  let (qualifier, localName) = splitQualifiedIdentifierText txt
   in Name qualifier (inferNameType localName) localName []
  where
    splitQualifiedIdentifierText fullName =
      case T.splitOn "." fullName of
        [] -> (Nothing, fullName)
        [_] -> (Nothing, fullName)
        parts
          | all isModuleSegment (init parts) && isIdentifierSegment (last parts) ->
              (Just (T.intercalate "." (init parts)), last parts)
          | otherwise ->
              (Nothing, fullName)

    isModuleSegment segment =
      case T.uncons segment of
        Just (c, rest) -> isConIdentifierStartChar c && T.all isIdentChar rest
        Nothing -> False

    isIdentifierSegment segment =
      let magicHashes = T.takeWhileEnd (== '#') segment
          baseSegment = T.dropEnd (T.length magicHashes) segment
       in case T.uncons baseSegment of
            Just (c, rest) -> isIdentifierStartChar c && T.all isIdentChar rest
            Nothing -> False

unqualifiedNameFromText :: Text -> UnqualifiedName
unqualifiedNameFromText txt = UnqualifiedName (inferNameType txt) txt []

inferNameType :: Text -> NameType
inferNameType localName
  | isOperatorLikeText localName =
      if T.isPrefixOf ":" localName
        then NameConSym
        else NameVarSym
  | otherwise =
      case T.uncons localName of
        Just (c, _) | isConIdentifierStartChar c -> NameConId
        Just (c, _) | isIdentifierStartChar c -> NameVarId
        _ -> NameConId

isIdentChar :: Char -> Bool
isIdentChar c = isIdentifierStartChar c || isIdentifierContinueChar c || c == '\''

isIdentifierStartChar :: Char -> Bool
isIdentifierStartChar c = c == '_' || generalCategory c == LowercaseLetter || isConIdentifierStartChar c

isConIdentifierStartChar :: Char -> Bool
isConIdentifierStartChar c = generalCategory c `elem` [UppercaseLetter, TitlecaseLetter]

isIdentifierNumberChar :: Char -> Bool
isIdentifierNumberChar c =
  case generalCategory c of
    DecimalNumber -> True
    OtherNumber -> True
    _ -> False

isIdentifierContinueChar :: Char -> Bool
isIdentifierContinueChar c =
  case generalCategory c of
    LetterNumber -> True
    ModifierLetter -> True
    NonSpacingMark -> True
    _ -> isIdentifierNumberChar c

isOperatorLikeText :: Text -> Bool
isOperatorLikeText op =
  not (T.null op) && T.all isOperatorChar op

isOperatorChar :: Char -> Bool
isOperatorChar c = c `elem` (":!#$%&*+./<=>?@\\^|-~" :: String) || isUnicodeOperatorChar c

isUnicodeOperatorChar :: Char -> Bool
isUnicodeOperatorChar c =
  case generalCategory c of
    MathSymbol -> True
    CurrencySymbol -> True
    ModifierSymbol -> True
    OtherSymbol -> True
    ConnectorPunctuation -> c > '\x7f'
    DashPunctuation -> c > '\x7f'
    OtherPunctuation -> c > '\x7f'
    _ -> False

type BinderName = UnqualifiedName

type OperatorName = UnqualifiedName

-- | Strictness field pragmas.
-- Examples: @{-# UNPACK #-}@ and @{-# NOUNPACK #-}@.
data PragmaUnpackKind
  = -- | @{-# UNPACK #-}@
    UnpackPragma
  | -- | @{-# NOUNPACK #-}@
    NoUnpackPragma
  deriving (Data, Eq, Ord, Show, Read, Generic, NFData)

-- | Parsed pragma payload.
-- Examples include @{-# LANGUAGE LambdaCase #-}@, @{-# WARNING "use g" #-}@,
-- @{-# INLINE [1] f #-}@, and @{-# SCC loop #-}@.
data PragmaType
  = -- | @{-# LANGUAGE LambdaCase, NoImplicitPrelude #-}@
    PragmaLanguage [ExtensionSetting]
  | -- | @{-# OVERLAPPING #-}@ on an instance declaration.
    PragmaInstanceOverlap InstanceOverlapPragma
  | -- | @{-# WARNING "use g" #-}@
    PragmaWarning Text
  | -- | @{-# DEPRECATED "use g" #-}@
    PragmaDeprecated Text
  | -- | @{-# INLINE [1] f #-}@ or @{-# NOINLINE f #-}@.
    PragmaInline Text Text
  | -- | @{-# UNPACK #-}@ or @{-# NOUNPACK #-}@.
    PragmaUnpack PragmaUnpackKind
  | -- | @{-# SOURCE #-}@ or @{-# SOURCE "phase" #-}@.
    PragmaSource Text Text
  | -- | @{-# SCC loop #-}@
    PragmaSCC Text
  | -- | Any pragma preserved as raw text but not otherwise classified.
    PragmaUnknown Text
  deriving (Data, Eq, Ord, Show, Read, Generic, NFData)

-- | A full pragma, including its original source text.
-- Example: @{-# INLINE map #-}@.
data Pragma = Pragma
  { pragmaType :: PragmaType,
    pragmaRawText :: Text
  }
  deriving (Data, Eq, Ord, Show, Read, Generic, NFData)

-- | A whole Haskell module.
-- Example: @module M (x) where import Data.List; x = 1@.
data Module = Module
  { moduleAnns :: [Annotation],
    moduleHead :: Maybe ModuleHead,
    moduleLanguagePragmas :: [ExtensionSetting],
    moduleImports :: [ImportDecl],
    moduleDecls :: [Decl]
  }
  deriving (Data, Eq, Show, Generic, NFData)

-- | The optional @module ... where@ header.
-- Example: @module M (x, type T) where@.
data ModuleHead = ModuleHead
  { moduleHeadAnns :: [Annotation],
    moduleHeadName :: Text,
    moduleHeadWarningPragma :: Maybe Pragma,
    moduleHeadExports :: Maybe [ExportSpec]
  }
  deriving (Data, Eq, Show, Generic, NFData)

moduleName :: Module -> Maybe Text
moduleName modu = moduleHeadName <$> moduleHead modu

moduleWarningPragma :: Module -> Maybe Pragma
moduleWarningPragma modu = moduleHeadWarningPragma =<< moduleHead modu

moduleExports :: Module -> Maybe [ExportSpec]
moduleExports modu = moduleHeadExports =<< moduleHead modu

-- | Optional namespace qualifiers on import or export items.
-- Examples: @type T@, @pattern P@, @data T@.
data IEEntityNamespace
  = -- | @type T@
    IEEntityNamespaceType
  | -- | @pattern P@
    IEEntityNamespacePattern
  | -- | @data T@
    IEEntityNamespaceData
  deriving (Data, Eq, Show, Generic, NFData)

-- | Namespace qualifiers for bundled members in import and export lists.
-- Examples: @T(type (+))@ and @T(data MkT)@.
data IEBundledNamespace
  = -- | @T(type (+))@
    IEBundledNamespaceType
  | -- | @T(data MkT)@
    IEBundledNamespaceData
  deriving (Data, Eq, Show, Generic, NFData)

-- | A member bundled with a parent import or export.
-- Example: @Just@ in @Maybe(Just, Nothing)@.
data IEBundledMember = IEBundledMember
  { ieBundledMemberNamespace :: Maybe IEBundledNamespace,
    ieBundledMemberName :: Name
  }
  deriving (Data, Eq, Show, Generic, NFData)

-- | One item in a module export list.
data ExportSpec
  = -- | @module Data.List@
    ExportModule (Maybe Pragma) Text
  | -- | @map@ or @type T@
    ExportVar (Maybe Pragma) (Maybe IEEntityNamespace) Name
  | -- | @Maybe@
    ExportAbs (Maybe Pragma) (Maybe IEEntityNamespace) Name
  | -- | @Maybe(..)@
    ExportAll (Maybe Pragma) (Maybe IEEntityNamespace) Name
  | -- | @Maybe(Just, Nothing)@
    ExportWith (Maybe Pragma) (Maybe IEEntityNamespace) Name [IEBundledMember]
  | -- | @Maybe(Just, .., Nothing)@
    ExportWithAll (Maybe Pragma) (Maybe IEEntityNamespace) Name Int [IEBundledMember]
  | -- | Metadata for the whole export item.
    ExportAnn Annotation ExportSpec
  deriving (Data, Eq, Show, Generic, NFData)

-- | A module import declaration.
-- Example: @import qualified Data.Map as Map hiding (lookup)@.
data ImportDecl = ImportDecl
  { importDeclAnns :: [Annotation],
    importDeclLevel :: Maybe ImportLevel,
    importDeclPackage :: Maybe Text,
    importDeclSourcePragma :: Maybe Pragma,
    importDeclSafe :: Bool,
    importDeclQualified :: Bool,
    importDeclQualifiedPost :: Bool,
    importDeclModule :: Text,
    importDeclAs :: Maybe Text,
    importDeclSpec :: Maybe ImportSpec
  }
  deriving (Data, Eq, Show, Generic, NFData)

-- | Explicit level imports.
-- Examples: @import quote M@ and @import splice M@.
data ImportLevel
  = -- | @import quote M@
    ImportLevelQuote
  | -- | @import splice M@
    ImportLevelSplice
  deriving (Data, Eq, Show, Generic, NFData)

-- | An optional import list or hiding list.
-- Examples: @(map, foldr)@ and @hiding (map)@.
data ImportSpec = ImportSpec
  { importSpecAnns :: [Annotation],
    importSpecHiding :: Bool,
    importSpecItems :: [ImportItem]
  }
  deriving (Data, Eq, Show, Generic, NFData)

-- | One item in an import list.
data ImportItem
  = -- | @map@ or @type T@
    ImportItemVar (Maybe IEEntityNamespace) UnqualifiedName
  | -- | @Maybe@
    ImportItemAbs (Maybe IEEntityNamespace) UnqualifiedName
  | -- | @Maybe(..)@
    ImportItemAll (Maybe IEEntityNamespace) UnqualifiedName
  | -- | @Maybe(Just, Nothing)@
    ImportItemWith (Maybe IEEntityNamespace) UnqualifiedName [IEBundledMember]
  | -- | @Maybe(Just, .., Nothing)@
    ImportItemAllWith (Maybe IEEntityNamespace) UnqualifiedName Int [IEBundledMember]
  | -- | Metadata for the whole import item.
    ImportAnn Annotation ImportItem
  deriving (Data, Eq, Show, Generic, NFData)

-- | A top-level declaration.
data Decl
  = -- | Metadata for the whole declaration.
    DeclAnn Annotation Decl
  | -- | @f x = x@ or @x = 1@
    DeclValue ValueDecl
  | -- | @f, g :: Int -> Int@
    DeclTypeSig [BinderName] Type
  | -- | @pattern P x = Just x@
    DeclPatSyn PatSynDecl
  | -- | @pattern P :: a -> Maybe a@
    DeclPatSynSig [BinderName] Type
  | -- | @type T :: Type -> Type@
    DeclStandaloneKindSig BinderName Type
  | -- | @infixr 5 :+:@
    DeclFixity FixityAssoc (Maybe IEEntityNamespace) (Maybe Int) [OperatorName]
  | -- | @type role T nominal representational@
    DeclRoleAnnotation RoleAnnotation
  | -- | @type Pair a = (a, a)@
    DeclTypeSyn TypeSynDecl
  | -- | @type data Nat = Z | S Nat@
    DeclTypeData DataDecl
  | -- | @data Maybe a = Nothing | Just a@
    DeclData DataDecl
  | -- | @newtype Identity a = Identity a@
    DeclNewtype NewtypeDecl
  | -- | @class Functor f where fmap :: (a -> b) -> f a -> f b@
    DeclClass ClassDecl
  | -- | @instance Show a => Show [a] where ...@
    DeclInstance InstanceDecl
  | -- | @deriving stock instance Show a => Show (T a)@
    DeclStandaloneDeriving StandaloneDerivingDecl
  | -- | @default (Integer, Double)@
    DeclDefault [Type]
  | -- | @$decl@ or @$(decl)@
    DeclSplice Expr
  | -- | @foreign import ccall "puts" puts :: CString -> IO CInt@
    DeclForeign ForeignDecl
  | -- | @type family F a :: Type@
    DeclTypeFamilyDecl TypeFamilyDecl
  | -- | @data family DF a@
    DeclDataFamilyDecl DataFamilyDecl
  | -- | @type instance F Int = Bool@
    DeclTypeFamilyInst TypeFamilyInst
  | -- | @data instance DF Int = DFInt@
    DeclDataFamilyInst DataFamilyInst
  | -- | A standalone pragma declaration such as @{-# INLINE f #-}@.
    DeclPragma Pragma
  deriving (Data, Eq, Show, Generic, NFData)

-- | Peel nested 'DeclAnn' wrappers.
peelDeclAnn :: Decl -> Decl
peelDeclAnn (DeclAnn _ inner) = peelDeclAnn inner
peelDeclAnn d = d

-- | Multiplicity tag on a let/where pattern binding.
-- Corresponds to @x = e@ (no tag), @%1 x = e@ (linear), @%m x = e@ (explicit).
data MultiplicityTag
  = -- | @x = e@
    NoMultiplicityTag
  | -- | @%1 x = e@
    LinearMultiplicityTag
  | -- | @%m x = e@
    ExplicitMultiplicityTag Type
  deriving (Data, Eq, Show, Generic, NFData)

-- | A value binding.
data ValueDecl
  = -- | @f x y = body@
    FunctionBind BinderName [Match]
  | -- | @pat = body@ or @%1 pat = body@
    PatternBind MultiplicityTag Pattern (Rhs Expr)
  deriving (Data, Eq, Show, Generic, NFData)

-- | One equation of a function or pattern synonym.
-- Examples: @f x = x@ and @x :+: y = z@.
data Match = Match
  { matchAnns :: [Annotation],
    matchHeadForm :: MatchHeadForm,
    matchPats :: [Pattern],
    matchRhs :: Rhs Expr
  }
  deriving (Data, Eq, Show, Generic, NFData)

-- | Whether a binding head is prefix or infix.
-- Examples: @f x@ and @x :+: y@.
data MatchHeadForm
  = -- | @f x@
    MatchHeadPrefix
  | -- | @x :+: y@
    MatchHeadInfix
  deriving (Data, Eq, Show, Generic, NFData)

-- | Pattern synonym declaration direction.
data PatSynDir
  = -- | @pattern P x <- pat@
    PatSynUnidirectional
  | -- | @pattern P x = pat@
    PatSynBidirectional
  | -- | @pattern P x <- pat where P x = expr@
    PatSynExplicitBidirectional [Match]
  deriving (Data, Eq, Show, Generic, NFData)

-- | Pattern synonym argument form.
data PatSynArgs
  = -- | @pattern Name arg1 arg2 ...@
    PatSynPrefixArgs [Text]
  | -- | @pattern arg1 \`Name\` arg2@ or @pattern arg1 :+: arg2@
    PatSynInfixArgs Text Text
  | -- | @pattern Name {field1, field2}@
    PatSynRecordArgs [Text]
  deriving (Data, Eq, Show, Generic, NFData)

-- | Pattern synonym declaration.
-- Example: @pattern JustOne x = Just [x]@.
data PatSynDecl = PatSynDecl
  { patSynDeclName :: UnqualifiedName,
    patSynDeclArgs :: PatSynArgs,
    patSynDeclPat :: Pattern,
    patSynDeclDir :: PatSynDir
  }
  deriving (Data, Eq, Show, Generic, NFData)

-- | The right-hand side of a binding, case alternative, or lambda case.
data Rhs body
  = -- | @= body@ with an optional @where@ clause.
    UnguardedRhs [Annotation] body (Maybe [Decl])
  | -- | Guarded equations such as @| x > 0 = body@.
    GuardedRhss [Annotation] [GuardedRhs body] (Maybe [Decl])
  deriving (Data, Eq, Show, Generic, NFData)

-- | One guarded branch.
-- Example: @| x > 0 = x@.
data GuardedRhs body = GuardedRhs
  { guardedRhsAnns :: [Annotation],
    guardedRhsGuards :: [GuardQualifier],
    guardedRhsBody :: body
  }
  deriving (Data, Eq, Show, Generic, NFData)

-- | A single qualifier inside a guarded right-hand side.
data GuardQualifier
  = -- | Metadata for the whole guard qualifier (typically a 'SourceSpan' via 'mkAnnotation').
    GuardAnn Annotation GuardQualifier
  | -- | @x > 0@
    GuardExpr Expr
  | -- | @Just y <- mx@
    GuardPat Pattern Expr
  | -- | @let y = f x@
    GuardLet [Decl]
  deriving (Data, Eq, Show, Generic, NFData)

peelGuardQualifierAnn :: GuardQualifier -> GuardQualifier
peelGuardQualifierAnn (GuardAnn _ inner) = peelGuardQualifierAnn inner
peelGuardQualifierAnn q = q

-- | A literal token.
data Literal
  = -- | Metadata for the whole literal.
    LitAnn Annotation Literal
  | -- | @1@, @1#@, @1#Word8@
    LitInt Integer NumericType Text
  | -- | @1.0@, @1.0#@, @1.0##@
    LitFloat Rational FloatType Text
  | -- | @'x'@
    LitChar Char Text
  | -- | @'x'#@
    LitCharHash Char Text
  | -- | @"hello"@
    LitString Text Text
  | -- | @"hello"#@
    LitStringHash Text Text
  deriving (Data, Eq, Show, Generic, NFData)

literalAnnSpan :: SourceSpan -> Literal -> Literal
literalAnnSpan sp = LitAnn (mkAnnotation sp)

peelLiteralAnn :: Literal -> Literal
peelLiteralAnn (LitAnn _ inner) = peelLiteralAnn inner
peelLiteralAnn lit = lit

-- | Whether tuple syntax is boxed or unboxed.
-- Examples: @(a, b)@ and @(# a, b #)@.
data TupleFlavor
  = -- | @(a, b)@
    Boxed
  | -- | @(# a, b #)@
    Unboxed
  deriving (Data, Eq, Show, Generic, NFData)

-- | One record field occurrence.
-- Examples: @x = 1@ in @T { x = 1 }@ and @x@ in @T { x }@.
data RecordField a = RecordField
  { recordFieldName :: Name,
    recordFieldValue :: a,
    recordFieldPun :: Bool
  }
  deriving (Data, Eq, Show, Generic, NFData)

-- | A pattern.
data Pattern
  = -- | Metadata for the whole pattern.
    PAnn Annotation Pattern
  | -- | @x@
    PVar UnqualifiedName
  | -- | @\@a@ in a required type argument pattern.
    PTypeBinder TyVarBinder
  | -- | @type T@ in a term-level pattern position.
    PTypeSyntax TypeSyntaxForm Type
  | -- | @_@
    PWildcard
  | -- | @1@ or @'x'@
    PLit Literal
  | -- | @[p| body |]@
    PQuasiQuote Text Text
  | -- | @(p1, p2)@ or @(# p1, p2 #)@
    PTuple TupleFlavor [Pattern]
  | -- | @(# | p | #)@
    PUnboxedSum Int Int Pattern
  | -- | @[p1, p2]@
    PList [Pattern]
  | -- | @Just x@ or @Proxy \@Type@
    PCon Name [Type] [Pattern]
  | -- | @x :+: y@
    PInfix Pattern Name Pattern
  | -- | @(view -> pat)@
    PView Expr Pattern
  | -- | @name\@pat@
    PAs UnqualifiedName Pattern
  | -- | @!pat@
    PStrict Pattern
  | -- | @~pat@
    PIrrefutable Pattern
  | -- | @-1@
    PNegLit Literal
  | -- | @(pat)@
    PParen Pattern
  | -- | @T { x = p, y, .. }@
    PRecord Name [RecordField Pattern] Bool -- Bool: wildcard present
  | -- | @(pat :: ty)@
    PTypeSig Pattern Type
  | -- | @$pat@ or @$(pat)@
    PSplice Expr
  -- \$pat or $(pat) (TH pattern splice)
  deriving (Data, Eq, Show, Generic, NFData)

-- | Peel nested 'PAnn' wrappers.
peelPatternAnn :: Pattern -> Pattern
peelPatternAnn (PAnn _ inner) = peelPatternAnn inner
peelPatternAnn p = p

-- | How type syntax appears in a term or pattern position.
-- Examples: @type T@ and @f (type T)@.
data TypeSyntaxForm
  = -- | The explicit @type@ namespace, as in @type T@.
    TypeSyntaxExplicitNamespace
  | -- | Type syntax embedded in term syntax, as in @f (type T)@.
    TypeSyntaxInTerm
  deriving (Data, Eq, Show, Generic, NFData)

-- | Whether a @forall@ telescope is invisible or visible.
-- Examples: @forall a. ...@ and @forall a -> ...@.
data ForallVis
  = -- | @forall a. ...@
    ForallInvisible
  | -- | @forall a -> ...@
    ForallVisible
  deriving (Data, Eq, Show, Generic, NFData)

-- | The binders introduced by a @forall@.
-- Example: @forall a b. a -> b -> a@.
data ForallTelescope = ForallTelescope
  { forallTelescopeVisibility :: ForallVis,
    forallTelescopeBinders :: [TyVarBinder]
  }
  deriving (Data, Eq, Show, Generic, NFData)

-- | The kind of a function arrow.
-- @ArrowUnrestricted@ is the ordinary @->@.
-- @ArrowLinear@ is @%1 ->@ (or its Unicode equivalent @⊸@).
-- @ArrowExplicit ty@ is @%m ->@ for an arbitrary multiplicity type @m@.
data ArrowKind
  = ArrowUnrestricted
  | ArrowLinear
  | ArrowExplicit Type
  deriving (Data, Eq, Show, Generic, NFData)

-- | A type.
data Type
  = -- | Metadata for the whole type.
    TAnn Annotation Type
  | -- | @a@
    TVar UnqualifiedName
  | -- | @Maybe@ or @'Just@
    TCon Name TypePromotion
  | -- | @(,)@, @(->)@, @[]@, or @(:)@
    TBuiltinCon TypeBuiltinCon
  | -- | @(?x :: Int) => Int@
    TImplicitParam Text Type
  | -- | @1@, @"x"@, or @'c'@ at the type level.
    TTypeLit TypeLiteral
  | -- | @*@ as written source syntax.
    TStar Text
  | -- | @[t| body |]@
    TQuasiQuote Text Text
  | -- | @forall a. ty@
    TForall ForallTelescope Type
  | -- | @Maybe a@
    TApp Type Type
  | -- | @Proxy \@Type@
    TTypeApp Type Type
  | -- | @a :+: b@
    TInfix Type Name TypePromotion Type
  | -- | @a -> b@, @a %1 -> b@, or @a %m -> b@
    TFun ArrowKind Type Type
  | -- | @(a, b)@, @'(a, b)@, or @(# a, b #)@
    TTuple TupleFlavor TypePromotion [Type]
  | -- | @(# a | b #)@
    TUnboxedSum [Type]
  | -- | @[a]@ or @'[a, b]@
    TList TypePromotion [Type]
  | -- | @(ty)@
    TParen Type
  | -- | @(a :: Type)@
    TKindSig Type Type
  | -- | @(Show a, Eq a) => a -> String@
    TContext [Type] Type
  | -- | @$typ@ or @$(typ)@
    TSplice Expr
  | -- \$typ or $(typ) (TH type splice)
    -- \_ (wildcard type, used in type family instance patterns)
    TWildcard
  deriving (Data, Eq, Show, Generic, NFData)

-- | Built-in type constructors that have dedicated surface syntax.
-- Examples: @(,)@, @(->)@, @[]@, and @(:)@.
data TypeBuiltinCon
  = -- | An @n@-tuple constructor like @(,)@ or @(,,)@.
    TBuiltinTuple Int
  | -- | @(->)@
    TBuiltinArrow
  | -- | @[]@
    TBuiltinList
  | -- | @(:)@
    TBuiltinCons
  deriving (Data, Eq, Show, Generic, NFData)

typeAnnSpan :: SourceSpan -> Type -> Type
typeAnnSpan sp = TAnn (mkAnnotation sp)

-- | Peel nested 'TAnn' wrappers (e.g. span-only dynamic annotations).
peelTypeAnn :: Type -> Type
peelTypeAnn (TAnn _ inner) = peelTypeAnn inner
peelTypeAnn t = t

-- | Peel 'TAnn' then 'TParen' (used when matching on type shape under annotations
-- and explicit parentheses).
peelTypeHead :: Type -> Type
peelTypeHead (TAnn _ inner) = peelTypeHead inner
peelTypeHead (TParen inner) = peelTypeHead inner
peelTypeHead ty = ty

-- | A type-level literal.
-- Examples: @1@, @"name"@, and @'x'@ in types.
data TypeLiteral
  = -- | @1@
    TypeLitInteger Integer Text
  | -- | @"name"@
    TypeLitSymbol Text Text
  | -- | @'x'@
    TypeLitChar Char Text
  deriving (Data, Eq, Show, Generic, NFData)

-- | Numeric type suffix for integer literals.
-- Corresponds to the ExtendedLiterals extension (GHC 9.8+).
-- Examples: @1@ -> TInteger, @1#@ -> TIntHash, @1#Word8@ -> TWord8Hash
data NumericType
  = TInteger
  | TIntHash
  | TWordHash
  | TInt8Hash
  | TInt16Hash
  | TInt32Hash
  | TInt64Hash
  | TWord8Hash
  | TWord16Hash
  | TWord32Hash
  | TWord64Hash
  deriving (Data, Eq, Ord, Show, Read, Generic, NFData)

-- | Float type suffix for fractional literals.
-- Examples: @1.0@, @1.0#@, @1.0##@.
data FloatType
  = TFractional
  | TFloatHash
  | TDoubleHash
  deriving (Data, Eq, Ord, Show, Read, Generic, NFData)

-- | Whether list, tuple, and constructor syntax is promoted with a leading quote.
-- Examples: @Maybe@ versus @'Just@, @[a]@ versus @'[a]@.
data TypePromotion
  = -- | Ordinary type syntax such as @Maybe@ or @[a]@.
    Unpromoted
  | -- | Promoted syntax such as @'Just@ or @'[a]@.
    Promoted
  deriving (Data, Eq, Show, Generic, NFData)

-- | Whether a type-variable binder was written as inferred or specified.
-- Examples: @{a}@ versus @a@.
data TyVarBSpecificity
  = -- | @{a}@ or @{a :: k}@
    TyVarBInferred
  | -- | @a@ or @(a :: k)@
    TyVarBSpecified
  deriving (Data, Eq, Show, Generic, NFData)

-- | Whether a type-variable binder is visible or invisible.
-- Examples: @a@ versus @@a@.
data TyVarBVisibility
  = -- | @a@
    TyVarBVisible
  | -- | @@a@
    TyVarBInvisible
  deriving (Data, Eq, Show, Generic, NFData)

-- | One type-variable binder.
-- Examples: @a@, @(a :: Type)@, @{a}@, and @@k@.
data TyVarBinder = TyVarBinder
  { tyVarBinderAnns :: [Annotation],
    tyVarBinderName :: Text,
    -- | Optional kind annotation. Examples: @(a :: Type)@ and @{a :: Type}@.
    tyVarBinderKind :: Maybe Type,
    -- | Whether the binder was written as specified (@a@, @(a :: k)@)
    -- or inferred (@{a}@, @{a :: k}@).
    tyVarBinderSpecificity :: TyVarBSpecificity,
    -- | Whether the binder was written visibly (@a@) or invisibly (@@a@).
    tyVarBinderVisibility :: TyVarBVisibility
  }
  deriving (Data, Eq, Show, Generic, NFData)

-- | Whether a type or binder head is prefix or infix.
-- Examples: @T a@ versus @a :+: b@.
data TypeHeadForm
  = -- | @T a@
    TypeHeadPrefix
  | -- | @a :+: b@
    TypeHeadInfix
  deriving (Data, Eq, Show, Generic, NFData)

-- | The head of a declaration that binds a type constructor or class.
data BinderHead name
  = -- | @T a b@
    PrefixBinderHead name [TyVarBinder]
  | -- | @a :+: b@
    InfixBinderHead TyVarBinder name TyVarBinder [TyVarBinder]
  deriving (Data, Eq, Show, Generic, NFData)

binderHeadForm :: BinderHead name -> TypeHeadForm
binderHeadForm head' =
  case head' of
    PrefixBinderHead {} -> TypeHeadPrefix
    InfixBinderHead {} -> TypeHeadInfix

binderHeadName :: BinderHead name -> name
binderHeadName head' =
  case head' of
    PrefixBinderHead name _ -> name
    InfixBinderHead _ name _ _ -> name

binderHeadParams :: BinderHead name -> [TyVarBinder]
binderHeadParams head' =
  case head' of
    PrefixBinderHead _ params -> params
    InfixBinderHead lhs _ rhs tailParams -> lhs : rhs : tailParams

-- | Instance heads are stored as ordinary types.  Parenthesisation is
-- captured naturally via 'TParen' nodes, so a separate 'Bool' flag is
-- unnecessary.  The class name and type arguments can be extracted from
-- the spine when needed (see 'instanceHeadName', 'instanceHeadTypes').
type InstanceHeadType = Type

-- | Extract the class name from an instance head type by walking the
-- application spine down to the leftmost atom, peeling through 'TParen'
-- and 'TAnn' nodes.
--
-- >>> fmap renderName $ instanceHeadName (TApp (TCon "C" Unpromoted) (TVar "a"))
-- Just "C"
-- >>> fmap renderName $ instanceHeadName (TInfix (TVar "a") ":=>" Unpromoted (TVar "b"))
-- Just ":=>"
instanceHeadName :: Type -> Maybe Name
instanceHeadName = go
  where
    go (TAnn _ t) = go t
    go (TApp f _) = go f
    go (TTypeApp f _) = go f
    go (TParen t) = go t
    go (TCon name _) = Just name
    go (TInfix _ name _ _) = Just name
    go _ = Nothing

-- | Extract the type arguments from an instance head type.
--
-- For prefix application spines like @TApp (TApp (TCon C) a) b@, returns
-- @[a, b]@.  For infix heads like @TInfix a op b@, returns @[a, b]@.
instanceHeadTypes :: Type -> [Type]
instanceHeadTypes = go []
  where
    go acc (TAnn _ t) = go acc t
    go acc (TApp f arg) = go (arg : acc) f
    go acc (TTypeApp f _) = go acc f
    go acc (TParen t) = go acc t
    go _ (TInfix lhs _ _ rhs) = [lhs, rhs]
    go acc _ = acc

-- | Roles used in @type role@ annotations.
-- Examples: @nominal@, @representational@, @phantom@, and @_@.
data Role
  = -- | @nominal@
    RoleNominal
  | -- | @representational@
    RoleRepresentational
  | -- | @phantom@
    RolePhantom
  | -- | @_@
    RoleInfer
  deriving (Data, Eq, Show, Generic, NFData)

-- | A @type role@ declaration.
-- Example: @type role T nominal representational@.
data RoleAnnotation = RoleAnnotation
  { roleAnnotationName :: UnqualifiedName,
    roleAnnotationRoles :: [Role]
  }
  deriving (Data, Eq, Show, Generic, NFData)

-- | A @type@ synonym declaration.
-- Example: @type Pair a = (a, a)@.
data TypeSynDecl = TypeSynDecl
  { typeSynHead :: BinderHead UnqualifiedName,
    typeSynBody :: Type
  }
  deriving (Data, Eq, Show, Generic, NFData)

-- | Open or closed type synonym family declaration.
-- Used for top-level @type family F a@ and associated @type F a :: Kind@ in class bodies.
-- Examples: @type family F a :: Type@ and
-- @type family F a where F Int = Bool@.
data TypeFamilyDecl = TypeFamilyDecl
  { typeFamilyDeclHeadForm :: TypeHeadForm,
    typeFamilyDeclExplicitFamilyKeyword :: Bool,
    -- | Family head type. For simple families like @type family F a@, this is @TCon "F"@.
    -- For infix families like @type family l `And` r@, this is the full infix type.
    typeFamilyDeclHead :: Type,
    typeFamilyDeclParams :: [TyVarBinder],
    -- | Optional result signature, either an unnamed kind annotation (@:: Kind@)
    -- or a named result variable with injectivity annotation (@= r | r -> a@).
    typeFamilyDeclResultSig :: Maybe TypeFamilyResultSig,
    -- | @Nothing@ = open family; @Just eqs@ = closed family with equations
    typeFamilyDeclEquations :: Maybe [TypeFamilyEq]
  }
  deriving (Data, Eq, Show, Generic, NFData)

-- | The result signature on a type family head.
data TypeFamilyResultSig
  = -- | @type family F a :: Type@
    TypeFamilyKindSig Type
  | -- | @type family F a = r@
    TypeFamilyTyVarSig TyVarBinder
  | -- | @type family F a = r | r -> a@
    TypeFamilyInjectiveSig TyVarBinder TypeFamilyInjectivity
  deriving (Data, Eq, Show, Generic, NFData)

-- | The injectivity annotation in a type family result signature.
-- Example: @r -> a b@ in @type family F a b = r | r -> a b@.
data TypeFamilyInjectivity = TypeFamilyInjectivity
  { typeFamilyInjectivityAnns :: [Annotation],
    typeFamilyInjectivityResult :: Text,
    typeFamilyInjectivityDetermined :: [Text]
  }
  deriving (Data, Eq, Show, Generic, NFData)

-- | One equation in a closed type family: @[forall binders.] LhsType = RhsType@
-- Example: @forall a. F [a] = Maybe a@.
data TypeFamilyEq = TypeFamilyEq
  { typeFamilyEqAnns :: [Annotation],
    typeFamilyEqForall :: [TyVarBinder],
    typeFamilyEqHeadForm :: TypeHeadForm,
    typeFamilyEqLhs :: Type,
    typeFamilyEqRhs :: Type
  }
  deriving (Data, Eq, Show, Generic, NFData)

-- | Data family declaration (standalone or associated in a class body).
-- Example: @data family DF a :: Type@.
data DataFamilyDecl = DataFamilyDecl
  { dataFamilyDeclHead :: BinderHead UnqualifiedName,
    -- | Optional result kind annotation (@:: Kind@)
    dataFamilyDeclKind :: Maybe Type
  }
  deriving (Data, Eq, Show, Generic, NFData)

-- | Type family instance: @type [instance] [forall binders.] LhsType = RhsType@
-- Example: @type instance F Int = Bool@.
data TypeFamilyInst = TypeFamilyInst
  { typeFamilyInstForall :: [TyVarBinder],
    typeFamilyInstHeadForm :: TypeHeadForm,
    typeFamilyInstLhs :: Type,
    typeFamilyInstRhs :: Type
  }
  deriving (Data, Eq, Show, Generic, NFData)

-- | Data or newtype family instance (standalone or in an instance body).
-- Examples: @data instance DF Int = DFInt@ and
-- @newtype instance DF Bool = DFBool Bool@.
data DataFamilyInst = DataFamilyInst
  { -- | @True@ when declared with @newtype instance@
    dataFamilyInstIsNewtype :: Bool,
    dataFamilyInstForall :: [TyVarBinder],
    -- | The LHS type-application pattern (e.g. @GMap (Either a b) v@)
    dataFamilyInstHead :: Type,
    -- | Optional inline result kind annotation (@:: Kind@) before @=@ or @where@
    dataFamilyInstKind :: Maybe Type,
    dataFamilyInstConstructors :: [DataConDecl],
    dataFamilyInstDeriving :: [DerivingClause]
  }
  deriving (Data, Eq, Show, Generic, NFData)

-- | A @data@ or @type data@ declaration body.
-- Examples: @data Maybe a = Nothing | Just a@ and @type data Nat = Z | S Nat@.
data DataDecl = DataDecl
  { dataDeclCTypePragma :: Maybe Pragma,
    dataDeclHead :: BinderHead UnqualifiedName,
    dataDeclContext :: [Type],
    -- | Optional inline kind annotation (@:: Kind@) before @=@ or @where@
    dataDeclKind :: Maybe Type,
    dataDeclConstructors :: [DataConDecl],
    dataDeclDeriving :: [DerivingClause]
  }
  deriving (Data, Eq, Show, Generic, NFData)

-- | A @newtype@ declaration.
-- Example: @newtype Identity a = Identity a@.
data NewtypeDecl = NewtypeDecl
  { newtypeDeclCTypePragma :: Maybe Pragma,
    newtypeDeclHead :: BinderHead UnqualifiedName,
    newtypeDeclContext :: [Type],
    -- | Optional inline kind annotation (@:: Kind@) before @=@ or @where@
    newtypeDeclKind :: Maybe Type,
    newtypeDeclConstructor :: Maybe DataConDecl,
    newtypeDeclDeriving :: [DerivingClause]
  }
  deriving (Data, Eq, Show, Generic, NFData)

-- | A data constructor declaration.
data DataConDecl
  = -- | Metadata for the whole constructor declaration (typically a 'SourceSpan' via 'mkAnnotation').
    DataConAnn Annotation DataConDecl
  | -- | @Just a@
    PrefixCon [TyVarBinder] [Type] UnqualifiedName [BangType]
  | -- | @a :+: b@
    InfixCon [TyVarBinder] [Type] BangType UnqualifiedName BangType
  | -- | @MkT { x :: Int, y :: Bool }@
    RecordCon [TyVarBinder] [Type] UnqualifiedName [FieldDecl]
  | -- | GADT-style constructor: @Con :: forall a. Ctx => Type@
    -- The list of names supports multiple constructors: @T1, T2 :: Type@
    GadtCon [ForallTelescope] [Type] [UnqualifiedName] GadtBody
  | -- | Tuple-style constructor: @()@, @(a,b)@, @(# #)@, @(# a,b #)@
    TupleCon [TyVarBinder] [Type] TupleFlavor [BangType]
  | -- | Unboxed sum constructor: @(# a | #)@, @(# | b #)@
    -- Fields: forall vars, context, position (1-based), total arity, field type
    UnboxedSumCon [TyVarBinder] [Type] Int Int BangType
  | -- | List constructor: @[]@
    ListCon [TyVarBinder] [Type]
  deriving (Data, Eq, Show, Generic, NFData)

-- | Strip nested 'DataConAnn' wrappers.
peelDataConAnn :: DataConDecl -> DataConDecl
peelDataConAnn (DataConAnn _ inner) = peelDataConAnn inner
peelDataConAnn d = d

-- | Body of a GADT constructor after the @::@ and optional forall/context
-- Examples: @MkT :: a -> T a@ and @MkT :: { field :: a } -> T a@.
data GadtBody
  = -- | Prefix body: each argument is paired with its outgoing arrow kind.
    -- E.g. @a -> b %1 -> T a@ gives @[(a, ArrowUnrestricted), (b, ArrowLinear)]@ with result @T a@.
    GadtPrefixBody [(BangType, ArrowKind)] Type
  | -- | Record body: @{ field :: Type } -> T a@
    GadtRecordBody [FieldDecl] Type
  deriving (Data, Eq, Show, Generic, NFData)

-- | Get the result type from a GADT body
gadtBodyResultType :: GadtBody -> Type
gadtBodyResultType body =
  case body of
    GadtPrefixBody _ ty -> ty
    GadtRecordBody _ ty -> ty

-- | A field or constructor argument type with strictness/laziness markers.
-- Examples: @a@, @!a@, @~a@, and @{-# UNPACK #-} !Int@.
data BangType = BangType
  { bangAnns :: [Annotation],
    bangPragmas :: [Pragma],
    bangStrict :: Bool,
    bangLazy :: Bool,
    bangType :: Type
  }
  deriving (Data, Eq, Show, Generic, NFData)

-- | A record field declaration.
-- Example: @x, y :: Int@ in @data T = MkT { x, y :: Int }@.
data FieldDecl = FieldDecl
  { fieldAnns :: [Annotation],
    fieldNames :: [UnqualifiedName],
    -- | Optional multiplicity annotation, e.g. @x %'Many :: a@.
    fieldMultiplicity :: Maybe Type,
    fieldType :: BangType
  }
  deriving (Data, Eq, Show, Generic, NFData)

-- | One @deriving@ clause.
-- Examples: @deriving Show@ and @deriving stock (Eq, Ord)@.
data DerivingClause = DerivingClause
  { derivingStrategy :: Maybe DerivingStrategy,
    derivingClasses :: Either Name [Type]
  }
  deriving (Data, Eq, Show, Generic, NFData)

-- | An explicit deriving strategy.
data DerivingStrategy
  = -- | @deriving stock Show@
    DerivingStock
  | -- | @deriving newtype Num@
    DerivingNewtype
  | -- | @deriving anyclass C@
    DerivingAnyclass
  | -- | @deriving via Wrapper T@
    DerivingVia Type
  deriving (Data, Eq, Show, Generic, NFData)

-- | A standalone deriving declaration.
-- Example: @deriving stock instance Show a => Show (T a)@.
data StandaloneDerivingDecl = StandaloneDerivingDecl
  { standaloneDerivingStrategy :: Maybe DerivingStrategy,
    standaloneDerivingPragmas :: [Pragma],
    standaloneDerivingWarning :: Maybe Pragma,
    standaloneDerivingForall :: [TyVarBinder],
    standaloneDerivingContext :: [Type],
    standaloneDerivingHead :: InstanceHeadType
  }
  deriving (Data, Eq, Show, Generic, NFData)

-- | A class declaration.
-- Example: @class (Eq a) => C a where f :: a -> Bool@.
data ClassDecl = ClassDecl
  { classDeclContext :: Maybe [Type],
    classDeclHead :: BinderHead UnqualifiedName,
    classDeclFundeps :: [FunctionalDependency],
    classDeclItems :: [ClassDeclItem]
  }
  deriving (Data, Eq, Show, Generic, NFData)

-- | A functional dependency.
-- Example: @a -> b@ in @class C a b | a -> b@.
data FunctionalDependency = FunctionalDependency
  { functionalDependencyAnns :: [Annotation],
    functionalDependencyDeterminers :: [Text],
    functionalDependencyDetermined :: [Text]
  }
  deriving (Data, Eq, Show, Generic, NFData)

-- | An item inside a class body.
data ClassDeclItem
  = -- | Metadata for the whole class item.
    ClassItemAnn Annotation ClassDeclItem
  | -- | @f :: a -> Bool@
    ClassItemTypeSig [BinderName] Type
  | -- | @default f :: Show a => a -> String@
    ClassItemDefaultSig BinderName Type
  | -- | @infixr 5 :+:@
    ClassItemFixity FixityAssoc (Maybe IEEntityNamespace) (Maybe Int) [OperatorName]
  | -- | A default method body such as @f x = True@.
    ClassItemDefault ValueDecl
  | -- | @type family F a@
    ClassItemTypeFamilyDecl TypeFamilyDecl
  | -- | @data family DF a@
    ClassItemDataFamilyDecl DataFamilyDecl
  | -- | @type F Int = Bool@
    ClassItemDefaultTypeInst TypeFamilyInst
  | -- | A pragma inside the class body, such as @{-# MINIMAL f #-}@.
    ClassItemPragma Pragma
  deriving (Data, Eq, Show, Generic, NFData)

peelClassDeclItemAnn :: ClassDeclItem -> ClassDeclItem
peelClassDeclItemAnn (ClassItemAnn _ inner) = peelClassDeclItemAnn inner
peelClassDeclItemAnn item = item

-- | An instance declaration.
-- Example: @instance Show a => Show [a] where show = ...@.
data InstanceDecl = InstanceDecl
  { instanceDeclPragmas :: [Pragma],
    instanceDeclWarning :: Maybe Pragma,
    instanceDeclForall :: [TyVarBinder],
    instanceDeclContext :: [Type],
    instanceDeclHead :: InstanceHeadType,
    instanceDeclItems :: [InstanceDeclItem]
  }
  deriving (Data, Eq, Show, Generic, NFData)

-- | The pragma controlling instance overlap.
-- Examples: @{-# OVERLAPPING #-}@ and @{-# INCOHERENT #-}@.
data InstanceOverlapPragma
  = -- | @{-# OVERLAPPING #-}@
    Overlapping
  | -- | @{-# OVERLAPPABLE #-}@
    Overlappable
  | -- | @{-# OVERLAPS #-}@
    Overlaps
  | -- | @{-# INCOHERENT #-}@
    Incoherent
  deriving (Data, Eq, Ord, Show, Read, Generic, NFData)

-- | An item inside an instance body.
data InstanceDeclItem
  = -- | Metadata for the whole instance item (typically a 'SourceSpan' via 'mkAnnotation').
    InstanceItemAnn Annotation InstanceDeclItem
  | -- | A method body such as @show x = ...@
    InstanceItemBind ValueDecl
  | -- | @show :: T -> String@
    InstanceItemTypeSig [BinderName] Type
  | -- | @infixr 5 :+:@
    InstanceItemFixity FixityAssoc (Maybe IEEntityNamespace) (Maybe Int) [OperatorName]
  | -- | @type F Int = Bool@
    InstanceItemTypeFamilyInst TypeFamilyInst
  | -- | @data instance DF Int = DFInt@
    InstanceItemDataFamilyInst DataFamilyInst
  | -- pragma inside instance body (e.g. {-# SPECIALIZE instance ... #-})
    InstanceItemPragma Pragma
  deriving (Data, Eq, Show, Generic, NFData)

peelInstanceDeclItemAnn :: InstanceDeclItem -> InstanceDeclItem
peelInstanceDeclItemAnn (InstanceItemAnn _ inner) = peelInstanceDeclItemAnn inner
peelInstanceDeclItemAnn item = item

-- | Fixity associativity declarations.
-- Examples: @infix@, @infixl@, and @infixr@.
data FixityAssoc
  = -- | @infix@
    Infix
  | -- | @infixl@
    InfixL
  | -- | @infixr@
    InfixR
  deriving (Data, Eq, Show, Generic, NFData)

-- | A foreign import or export declaration.
-- Example: @foreign import ccall "puts" puts :: CString -> IO CInt@.
data ForeignDecl = ForeignDecl
  { foreignDirection :: ForeignDirection,
    foreignCallConv :: CallConv,
    foreignSafety :: Maybe ForeignSafety,
    foreignEntity :: ForeignEntitySpec,
    foreignName :: UnqualifiedName,
    foreignType :: Type
  }
  deriving (Data, Eq, Show, Generic, NFData)

-- | The entity part of a foreign declaration.
data ForeignEntitySpec
  = -- | @foreign import ccall "dynamic" ...@
    ForeignEntityDynamic
  | -- | @foreign import ccall "wrapper" ...@
    ForeignEntityWrapper
  | -- | @foreign import ccall "static foo" ...@
    ForeignEntityStatic (Maybe Text)
  | -- | @foreign import ccall "&foo" ...@
    ForeignEntityAddress (Maybe Text)
  | -- | @foreign import ccall "foo" ...@
    ForeignEntityNamed Text
  | -- | No explicit entity string, as in @foreign import ccall foo :: ...@.
    ForeignEntityOmitted
  deriving (Data, Eq, Show, Generic, NFData)

-- | Whether a foreign declaration imports or exports a symbol.
-- Examples: @foreign import@ and @foreign export@.
data ForeignDirection
  = -- | @foreign import ...@
    ForeignImport
  | -- | @foreign export ...@
    ForeignExport
  deriving (Data, Eq, Show, Generic, NFData)

-- | The calling convention used by a foreign declaration.
-- Examples: @ccall@, @stdcall@, @capi@, @prim@, and @javascript@.
data CallConv
  = -- | @ccall@
    CCall
  | -- | @stdcall@
    StdCall
  | -- | @capi@
    CApi
  | -- | @prim@
    CPrim
  | -- | @javascript@
    JavaScript
  deriving (Data, Eq, Show, Generic, NFData)

-- | Optional foreign-call safety annotations.
-- Examples: @safe@, @unsafe@, and @interruptible@.
data ForeignSafety
  = -- | @safe@
    Safe
  | -- | @unsafe@
    Unsafe
  | -- | @interruptible@
    Interruptible
  deriving (Data, Eq, Show, Generic, NFData)

-- | Opaque metadata attached to syntax nodes.
-- Example: a stored 'SourceSpan' created with 'mkAnnotation'.
newtype Annotation = Annotation Dynamic
  deriving (Show, Generic)

mkAnnotation :: (Typeable a) => a -> Annotation
mkAnnotation = Annotation . toDyn

fromAnnotation :: (Typeable a) => Annotation -> Maybe a
fromAnnotation (Annotation value) = fromDynamic value

instance Data Annotation where
  gfoldl _ z = z
  gunfold _ z _ = z (Annotation (toDyn ()))
  toConstr _ = annotationConstr
  dataTypeOf _ = annotationDataType

annotationConstr :: Constr
annotationConstr = mkConstr annotationDataType "Annotation" [] Prefix

annotationDataType :: DataType
annotationDataType = mkDataType "Aihc.Parser.Syntax.Annotation" [annotationConstr]

instance Eq Annotation where
  _ == _ = True

instance NFData Annotation where
  rnf (Annotation _) = ()

-- | An expression.
data Expr
  = -- | Metadata for the whole expression.
    EAnn Annotation Expr
  | -- | @x@ or @Data.List.map@
    EVar Name
  | -- | @type T@ in a term position.
    ETypeSyntax TypeSyntaxForm Type
  | -- | @1@, @1#@, @1#Word8@
    EInt Integer NumericType Text
  | -- | @1.0@, @1.0#@, @1.0##@
    EFloat Rational FloatType Text
  | -- | @'x'@
    EChar Char Text
  | -- | @'x'#@
    ECharHash Char Text
  | -- | @"hello"@
    EString Text Text
  | -- | @"hello"#@
    EStringHash Text Text
  | -- | @#name@
    EOverloadedLabel Text Text
  | -- | @[qq| body |]@
    EQuasiQuote Text Text
  | -- | @if c then t else e@
    EIf Expr Expr Expr
  | -- | @if | g1 -> e1 | g2 -> e2@
    EMultiWayIf [GuardedRhs Expr]
  | -- | @\x y -> body@
    ELambdaPats [Pattern] Expr
  | -- | @\case { p -> e }@
    ELambdaCase [CaseAlt Expr]
  | -- | @\cases { p q -> e }@
    ELambdaCases [LambdaCaseAlt]
  | -- | @a + b@
    EInfix Expr Name Expr
  | -- | @-x@
    ENegate Expr
  | -- | @(x +)@
    ESectionL Expr Name
  | -- | @(+ x)@
    ESectionR Name Expr
  | -- | @let x = 1 in x@
    ELetDecls [Decl] Expr
  | -- | @case x of { Just y -> y }@
    ECase Expr [CaseAlt Expr]
  | -- | @do { x <- mx; pure x }@, @mdo { ... }@, or @M.do { ... }@
    EDo [DoStmt Expr] DoFlavor
  | -- | @[x | x <- xs, x > 0]@
    EListComp Expr [CompStmt]
  | -- | @[x + y | x <- xs | y <- ys]@
    EListCompParallel Expr [[CompStmt]]
  | -- | @[1,3 .. 9]@
    EArithSeq ArithSeq
  | -- | @T { x = 1, y, .. }@
    ERecordCon Name [RecordField Expr] Bool -- Bool: wildcard present
  | -- | @r { x = 1 }@
    ERecordUpd Expr [RecordField Expr]
  | -- | @a.b@
    EGetField Expr Name -- a.b (OverloadedRecordDot)
  | -- | @(.b)@ or @(.b.c)@
    EGetFieldProjection [Name] -- (.b) or (.b.c) projection section (OverloadedRecordDot)
  | -- | @(expr :: ty)@
    ETypeSig Expr Type
  | -- | @(expr)@
    EParen Expr
  | -- | @[x, y]@
    EList [Expr]
  | -- | @(x, y)@, @(, y)@, or @(# x | #)@
    ETuple TupleFlavor [Maybe Expr]
  | -- | @(# | x | #)@
    EUnboxedSum Int Int Expr
  | -- | @f \@Type@
    ETypeApp Expr Type
  | -- | @f x@
    EApp Expr Expr
  | -- Template Haskell quotes

    -- | @[| expr |]@ or @[e| expr |]@
    ETHExpQuote Expr
  | -- | @[|| expr ||]@ or @[e|| expr ||]@
    ETHTypedQuote Expr
  | -- | @[d| decls |]@
    ETHDeclQuote [Decl]
  | -- | @[t| type |]@
    ETHTypeQuote Type
  | -- | @[p| pat |]@
    ETHPatQuote Pattern
  | -- | @'expr@ in the term namespace.
    ETHNameQuote Expr
  | -- | @''Type@ in the type namespace.
    ETHTypeNameQuote Type
  | -- Template Haskell splices

    -- | @$expr@ or @$(expr)@
    ETHSplice Expr
  | -- | @$$expr@ or @$$(expr)@
    ETHTypedSplice Expr
  | -- Arrow notation (Arrows extension)

    -- | @proc pat -> cmd@
    EProc Pattern Cmd
  | -- | @{-# SCC foo #-} expr@
    EPragma Pragma Expr
  deriving (Data, Eq, Show, Generic, NFData)

-- | Peel nested 'EAnn' layers (e.g. span-only dynamic annotations).
peelExprAnn :: Expr -> Expr
peelExprAnn (EAnn _ x) = peelExprAnn x
peelExprAnn x = x

-- | One @case@ or @\case@ alternative.
-- Example: @Just x -> x@.
data CaseAlt body = CaseAlt
  { caseAltAnns :: [Annotation],
    caseAltPattern :: Pattern,
    caseAltRhs :: Rhs body
  }
  deriving (Data, Eq, Show, Generic, NFData)

-- | One multi-argument @\cases@ alternative.
-- Example: @Just x y -> x + y@.
data LambdaCaseAlt = LambdaCaseAlt
  { lambdaCaseAltAnns :: [Annotation],
    lambdaCaseAltPats :: [Pattern],
    lambdaCaseAltRhs :: Rhs Expr
  }
  deriving (Data, Eq, Show, Generic, NFData)

-- | The keyword that introduces a @do@-like block.
-- Examples: @do@, @mdo@, @M.do@, and @M.mdo@.
data DoFlavor
  = -- | @do { stmts }@
    DoPlain
  | -- | @mdo { stmts }@
    DoMdo
  | -- | @M.do { stmts }@ (QualifiedDo)
    DoQualified Text
  | -- | @M.mdo { stmts }@ (QualifiedDo + RecursiveDo)
    DoQualifiedMdo Text
  deriving (Data, Eq, Show, Generic, NFData)

-- | A statement inside a @do@ block.
data DoStmt body
  = -- | Metadata for the whole do-statement (typically a 'SourceSpan' via 'mkAnnotation').
    DoAnn Annotation (DoStmt body)
  | -- | @pat <- expr@
    DoBind Pattern body
  | -- | @let decls@
    DoLetDecls [Decl]
  | -- | A bare expression statement such as @print x@.
    DoExpr body
  | -- | @rec { stmts }@
    DoRecStmt [DoStmt body] -- rec { stmts }
  deriving (Data, Eq, Show, Generic, NFData)

peelDoStmtAnn :: DoStmt body -> DoStmt body
peelDoStmtAnn (DoAnn _ inner) = peelDoStmtAnn inner
peelDoStmtAnn s = s

-- | Arrow command type (used inside 'proc' expressions).
-- Commands mirror expressions but live in a separate namespace so the
-- pretty-printer knows not to parenthesise @do { … }@ as an infix LHS.
data Cmd
  = -- | Metadata for the whole command (typically a 'SourceSpan' via 'mkAnnotation').
    CmdAnn Annotation Cmd
  | -- | @exp -\< exp@ or @exp -\<\< exp@
    CmdArrApp Expr ArrAppType Expr
  | -- | Command-level infix: @cmd1 op cmd2@
    CmdInfix Cmd Name Cmd
  | -- | @do { cstmts }@
    CmdDo [DoStmt Cmd]
  | -- | @if exp then cmd else cmd@
    CmdIf Expr Cmd Cmd
  | -- | @case exp of { calts }@
    CmdCase Expr [CaseAlt Cmd]
  | -- | @let decls in cmd@
    CmdLet [Decl] Cmd
  | -- | @\\pats -> cmd@
    CmdLam [Pattern] Cmd
  | -- | Command application: @cmd exp@
    CmdApp Cmd Expr
  | -- | Parenthesised command: @(cmd)@
    CmdPar Cmd
  deriving (Data, Eq, Show, Generic, NFData)

peelCmdAnn :: Cmd -> Cmd
peelCmdAnn (CmdAnn _ inner) = peelCmdAnn inner
peelCmdAnn c = c

-- | Arrow application type: first-order (@-\<@) or higher-order (@-\<\<@).
data ArrAppType
  = -- | @f -< x@
    HsFirstOrderApp
  | -- | @f -<< x@
    HsHigherOrderApp
  deriving (Data, Eq, Show, Generic, NFData)

-- | A statement inside a list comprehension.
data CompStmt
  = -- | Metadata for the whole comprehension statement (typically a 'SourceSpan' via 'mkAnnotation').
    CompAnn Annotation CompStmt
  | -- | @x <- xs@
    CompGen Pattern Expr
  | -- | @x > 0@
    CompGuard Expr
  | -- | @let y = f x@
    CompLetDecls [Decl]
  | -- | @then f@ (TransformListComp)
    CompThen Expr
  | -- | @then f by e@ (TransformListComp)
    CompThenBy Expr Expr
  | -- | @then group using f@ (TransformListComp)
    CompGroupUsing Expr
  | -- | @then group by e using f@ (TransformListComp)
    CompGroupByUsing Expr Expr
  deriving (Data, Eq, Show, Generic, NFData)

peelCompStmtAnn :: CompStmt -> CompStmt
peelCompStmtAnn (CompAnn _ inner) = peelCompStmtAnn inner
peelCompStmtAnn s = s

-- | An arithmetic sequence expression.
data ArithSeq
  = -- | Metadata for the whole arithmetic sequence (typically a 'SourceSpan' via 'mkAnnotation').
    ArithSeqAnn Annotation ArithSeq
  | -- | @[expr ..]@
    ArithSeqFrom Expr
  | -- | @[start, thenExpr ..]@
    ArithSeqFromThen Expr Expr
  | -- | @[start .. end]@
    ArithSeqFromTo Expr Expr
  | -- | @[start, thenExpr .. end]@
    ArithSeqFromThenTo Expr Expr Expr
  deriving (Data, Eq, Show, Generic, NFData)

peelArithSeqAnn :: ArithSeq -> ArithSeq
peelArithSeqAnn (ArithSeqAnn _ inner) = peelArithSeqAnn inner
peelArithSeqAnn s = s

-- | Recursively strip all annotations from an AST value.
--
-- Removes annotation wrapper constructors (e.g. 'EAnn', 'TAnn', 'PAnn',
-- 'DeclAnn') and replaces annotation lists with @[]@.  The traversal is
-- bottom-up: children are stripped before their parent, so nested annotation
-- wrappers are fully removed.
stripAnnotations :: (Data a) => a -> a
stripAnnotations x = applyStrip (gmapT stripAnnotations x)
  where
    applyStrip :: (Data c) => c -> c
    applyStrip =
      id
        `extT` sExpr
        `extT` sType
        `extT` sPattern
        `extT` sDecl
        `extT` sDataConDecl
        `extT` sLiteral
        `extT` sGuardQualifier
        `extT` (sDoStmt :: DoStmt Expr -> DoStmt Expr)
        `extT` (sDoStmt :: DoStmt Cmd -> DoStmt Cmd)
        `extT` sCompStmt
        `extT` sArithSeq
        `extT` sClassDeclItem
        `extT` sInstanceDeclItem
        `extT` sCmd
        `extT` sName
        `extT` sUnqualifiedName
        `extT` sExportSpec
        `extT` sImportItem
        `extT` sAnnotations
        `extT` sPragma

    -- \| Extend a generic transformation with a type-specific case.
    extT :: (Typeable c, Typeable d) => (c -> c) -> (d -> d) -> c -> c
    extT f g y = fromMaybe (f y) (cast . g =<< cast y)

    sExpr :: Expr -> Expr
    sExpr (EAnn _ e) = e
    sExpr e = e

    sType :: Type -> Type
    sType (TAnn _ t) = t
    sType t = t

    sPattern :: Pattern -> Pattern
    sPattern (PAnn _ p) = p
    sPattern p = p

    sDecl :: Decl -> Decl
    sDecl (DeclAnn _ d) = d
    sDecl d = d

    sDataConDecl :: DataConDecl -> DataConDecl
    sDataConDecl (DataConAnn _ d) = d
    sDataConDecl d = d

    sLiteral :: Literal -> Literal
    sLiteral (LitAnn _ l) = l
    sLiteral l = l

    sGuardQualifier :: GuardQualifier -> GuardQualifier
    sGuardQualifier (GuardAnn _ q) = q
    sGuardQualifier q = q

    sDoStmt :: DoStmt body -> DoStmt body
    sDoStmt (DoAnn _ s) = s
    sDoStmt s = s

    sCompStmt :: CompStmt -> CompStmt
    sCompStmt (CompAnn _ s) = s
    sCompStmt s = s

    sArithSeq :: ArithSeq -> ArithSeq
    sArithSeq (ArithSeqAnn _ s) = s
    sArithSeq s = s

    sClassDeclItem :: ClassDeclItem -> ClassDeclItem
    sClassDeclItem (ClassItemAnn _ i) = i
    sClassDeclItem i = i

    sInstanceDeclItem :: InstanceDeclItem -> InstanceDeclItem
    sInstanceDeclItem (InstanceItemAnn _ i) = i
    sInstanceDeclItem i = i

    sCmd :: Cmd -> Cmd
    sCmd (CmdAnn _ c) = c
    sCmd c = c

    sName :: Name -> Name
    sName name = name {nameAnns = []}

    sUnqualifiedName :: UnqualifiedName -> UnqualifiedName
    sUnqualifiedName name = name {unqualifiedNameAnns = []}

    sExportSpec :: ExportSpec -> ExportSpec
    sExportSpec (ExportAnn _ e) = e
    sExportSpec e = e

    sImportItem :: ImportItem -> ImportItem
    sImportItem (ImportAnn _ i) = i
    sImportItem i = i

    sAnnotations :: [Annotation] -> [Annotation]
    sAnnotations _ = []

    sPragma :: Pragma -> Pragma
    sPragma p = p {pragmaRawText = ""}
