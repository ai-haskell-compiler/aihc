{-# LANGUAGE DeriveAnyClass #-}

-- |
--
-- Module      : Aihc.Parser.Syntax
-- Description : Abstract Syntax Tree
-- License     : Unlicense
--
-- Abstract Syntax Tree (AST) covering Haskell2010 plus all language extensions.
module Aihc.Parser.Syntax
  ( ArithSeq (..),
    ArrAppType (..),
    BangType (..),
    BinderName,
    CallConv (..),
    CaseAlt (..),
    ClassDecl (..),
    ClassDeclItem (..),
    Cmd (..),
    CmdCaseAlt (..),
    CompStmt (..),
    FunctionalDependency (..),
    TypeHeadForm (..),
    DataConDecl (..),
    DataDecl (..),
    Decl (..),
    DerivingClause (..),
    DerivingStrategy (..),
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
    Literal (..),
    Match (..),
    MatchHeadForm (..),
    Module (..),
    ModuleHead (..),
    ModuleHeaderPragmas (..),
    WarningText (..),
    NewtypeDecl (..),
    OperatorName,
    PatSynArgs (..),
    PatSynDecl (..),
    PatSynDir (..),
    Pattern (..),
    Pragma (..),
    PragmaUnpackKind (..),
    Role (..),
    RoleAnnotation (..),
    Rhs (..),
    HasSourceSpan (..),
    SourceSpan (..),
    SourceUnpackedness (..),
    StandaloneDerivingDecl (..),
    Type (..),
    TupleFlavor (..),
    TypeLiteral (..),
    TypePromotion (..),
    TyVarBinder (..),
    TypeSynDecl (..),
    TypeFamilyDecl (..),
    TypeFamilyEq (..),
    DataFamilyDecl (..),
    TypeFamilyInst (..),
    DataFamilyInst (..),
    ValueDecl (..),
    declValueBinderNames,
    allKnownExtensions,
    applyExtensionSetting,
    applyImpliedExtensions,
    effectiveExtensions,
    extensionName,
    extensionSettingName,
    gadtBodyResultType,
    languageEditionExtensions,
    editionFromExtensionSettings,
    mergeSourceSpans,
    noSourceSpan,
    parseExtensionName,
    parseExtensionSettingName,
    parseLanguageEdition,
    sourceSpanEnd,
    valueDeclBinderName,
    moduleName,
    moduleWarningText,
    moduleExports,
  )
where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData (..))
import Data.Data (Constr, Data (..), DataType, Fixity (Prefix), mkConstr, mkDataType)
import Data.Dynamic
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Text.Read (readMaybe)

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
  | DerivingVia
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

data ExtensionSetting
  = EnableExtension Extension
  | DisableExtension Extension
  deriving (Data, Eq, Ord, Show, Read, Generic, NFData)

-- | The Haskell language edition/standard.
-- Each edition implies a set of language extensions.
data LanguageEdition
  = Haskell98Edition
  | Haskell2010Edition
  | GHC2021Edition
  | GHC2024Edition
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic, NFData)

-- | Pragmas extracted from a module header.
-- Contains the last language edition pragma (if any) and all extension settings.
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
  readMaybe (T.unpack trimmed) <|> lookup (T.unpack trimmed) aliases
  where
    trimmed = T.strip raw
    aliases =
      [ ("Cpp", CPP),
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
    (DerivingVia, [EnableExtension DerivingStrategies]),
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
    (TypeFamilyDependencies, [EnableExtension TypeFamilies]),
    (TypeInType, [EnableExtension PolyKinds, EnableExtension DataKinds, EnableExtension KindSignatures]),
    (TypeOperators, [EnableExtension ExplicitNamespaces]),
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

data SourceSpan
  = NoSourceSpan
  | SourceSpan
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

class HasSourceSpan a where
  getSourceSpan :: a -> SourceSpan

mergeSourceSpans :: SourceSpan -> SourceSpan -> SourceSpan
mergeSourceSpans left right =
  case (left, right) of
    ( SourceSpan name l1 c1 _ _ startOffset _,
      SourceSpan _ _ _ l2 c2 _ endOffset
      ) ->
        SourceSpan name l1 c1 l2 c2 startOffset endOffset
    (NoSourceSpan, span') -> span'
    (span', NoSourceSpan) -> span'

sourceSpanEnd :: (HasSourceSpan a) => [a] -> SourceSpan
sourceSpanEnd xs =
  case reverse xs of
    [] -> NoSourceSpan
    x : _ -> getSourceSpan x

type BinderName = Text

type OperatorName = Text

data WarningText
  = DeprText SourceSpan Text
  | WarnText SourceSpan Text
  deriving (Data, Eq, Show, Generic, NFData)

data PragmaUnpackKind
  = UnpackPragma
  | NoUnpackPragma
  deriving (Data, Eq, Ord, Show, Read, Generic, NFData)

data Pragma
  = PragmaLanguage [ExtensionSetting]
  | PragmaInstanceOverlap InstanceOverlapPragma
  | PragmaWarning Text
  | PragmaDeprecated Text
  | PragmaInline Text Text
  | PragmaUnpack PragmaUnpackKind
  | PragmaSource Text Text
  | PragmaUnknown Text
  deriving (Data, Eq, Ord, Show, Read, Generic, NFData)

instance HasSourceSpan WarningText where
  getSourceSpan warningText =
    case warningText of
      DeprText span' _ -> span'
      WarnText span' _ -> span'

data Module = Module
  { moduleSpan :: SourceSpan,
    moduleHead :: Maybe ModuleHead,
    moduleLanguagePragmas :: [ExtensionSetting],
    moduleImports :: [ImportDecl],
    moduleDecls :: [Decl]
  }
  deriving (Eq, Show, Generic, NFData)

instance HasSourceSpan Module where
  getSourceSpan = moduleSpan

data ModuleHead = ModuleHead
  { moduleHeadSpan :: SourceSpan,
    moduleHeadName :: Text,
    moduleHeadWarningText :: Maybe WarningText,
    moduleHeadExports :: Maybe [ExportSpec]
  }
  deriving (Eq, Show, Generic, NFData)

instance HasSourceSpan ModuleHead where
  getSourceSpan = moduleHeadSpan

moduleName :: Module -> Maybe Text
moduleName modu = moduleHeadName <$> moduleHead modu

moduleWarningText :: Module -> Maybe WarningText
moduleWarningText modu = moduleHeadWarningText =<< moduleHead modu

moduleExports :: Module -> Maybe [ExportSpec]
moduleExports modu = moduleHeadExports =<< moduleHead modu

data IEEntityNamespace
  = IEEntityNamespaceType
  | IEEntityNamespacePattern
  | IEEntityNamespaceData
  deriving (Data, Eq, Show, Generic, NFData)

data IEBundledNamespace
  = IEBundledNamespaceData
  deriving (Data, Eq, Show, Generic, NFData)

data IEBundledMember = IEBundledMember
  { ieBundledMemberNamespace :: Maybe IEBundledNamespace,
    ieBundledMemberName :: Text
  }
  deriving (Data, Eq, Show, Generic, NFData)

data ExportSpec
  = ExportModule SourceSpan (Maybe WarningText) Text
  | ExportVar SourceSpan (Maybe WarningText) (Maybe IEEntityNamespace) Text
  | ExportAbs SourceSpan (Maybe WarningText) (Maybe IEEntityNamespace) Text
  | ExportAll SourceSpan (Maybe WarningText) (Maybe IEEntityNamespace) Text
  | ExportWith SourceSpan (Maybe WarningText) (Maybe IEEntityNamespace) Text [IEBundledMember]
  deriving (Eq, Show, Generic, NFData)

instance HasSourceSpan ExportSpec where
  getSourceSpan spec =
    case spec of
      ExportModule span' _ _ -> span'
      ExportVar span' _ _ _ -> span'
      ExportAbs span' _ _ _ -> span'
      ExportAll span' _ _ _ -> span'
      ExportWith span' _ _ _ _ -> span'

data ImportDecl = ImportDecl
  { importDeclSpan :: SourceSpan,
    importDeclLevel :: Maybe ImportLevel,
    importDeclPackage :: Maybe Text,
    importDeclSource :: Bool,
    importDeclQualified :: Bool,
    importDeclQualifiedPost :: Bool,
    importDeclModule :: Text,
    importDeclAs :: Maybe Text,
    importDeclSpec :: Maybe ImportSpec
  }
  deriving (Eq, Show, Generic, NFData)

instance HasSourceSpan ImportDecl where
  getSourceSpan = importDeclSpan

data ImportLevel
  = ImportLevelQuote
  | ImportLevelSplice
  deriving (Eq, Show, Generic, NFData)

data ImportSpec = ImportSpec
  { importSpecSpan :: SourceSpan,
    importSpecHiding :: Bool,
    importSpecItems :: [ImportItem]
  }
  deriving (Eq, Show, Generic, NFData)

instance HasSourceSpan ImportSpec where
  getSourceSpan = importSpecSpan

data ImportItem
  = ImportItemVar SourceSpan (Maybe IEEntityNamespace) Text
  | ImportItemAbs SourceSpan (Maybe IEEntityNamespace) Text
  | ImportItemAll SourceSpan (Maybe IEEntityNamespace) Text
  | ImportItemWith SourceSpan (Maybe IEEntityNamespace) Text [IEBundledMember]
  deriving (Eq, Show, Generic, NFData)

data Decl
  = DeclAnn Annotation Decl
  | DeclValue SourceSpan ValueDecl
  | DeclTypeSig SourceSpan [BinderName] Type
  | DeclPatSyn SourceSpan PatSynDecl
  | DeclPatSynSig SourceSpan [BinderName] Type
  | DeclStandaloneKindSig SourceSpan BinderName Type
  | DeclFixity SourceSpan FixityAssoc (Maybe IEEntityNamespace) (Maybe Int) [OperatorName]
  | DeclRoleAnnotation SourceSpan RoleAnnotation
  | DeclTypeSyn SourceSpan TypeSynDecl
  | DeclTypeData SourceSpan DataDecl
  | DeclData SourceSpan DataDecl
  | DeclNewtype SourceSpan NewtypeDecl
  | DeclClass SourceSpan ClassDecl
  | DeclInstance SourceSpan InstanceDecl
  | DeclStandaloneDeriving SourceSpan StandaloneDerivingDecl
  | DeclDefault SourceSpan [Type]
  | -- \$decl or $(decl) (TH top-level splice)
    DeclSplice SourceSpan Expr
  | DeclForeign SourceSpan ForeignDecl
  | DeclTypeFamilyDecl SourceSpan TypeFamilyDecl
  | DeclDataFamilyDecl SourceSpan DataFamilyDecl
  | DeclTypeFamilyInst SourceSpan TypeFamilyInst
  | DeclDataFamilyInst SourceSpan DataFamilyInst
  | -- pragma declaration (e.g. {-# INLINE f #-}, {-# SPECIALIZE ... #-})
    DeclPragma SourceSpan Pragma
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan Decl where
  getSourceSpan decl =
    case decl of
      DeclAnn _ sub -> getSourceSpan sub
      DeclValue span' _ -> span'
      DeclTypeSig span' _ _ -> span'
      DeclPatSyn span' _ -> span'
      DeclPatSynSig span' _ _ -> span'
      DeclStandaloneKindSig span' _ _ -> span'
      DeclFixity span' _ _ _ _ -> span'
      DeclRoleAnnotation span' _ -> span'
      DeclTypeSyn span' _ -> span'
      DeclTypeData span' _ -> span'
      DeclData span' _ -> span'
      DeclNewtype span' _ -> span'
      DeclClass span' _ -> span'
      DeclInstance span' _ -> span'
      DeclStandaloneDeriving span' _ -> span'
      DeclDefault span' _ -> span'
      DeclForeign span' _ -> span'
      DeclSplice span' _ -> span'
      DeclTypeFamilyDecl span' _ -> span'
      DeclDataFamilyDecl span' _ -> span'
      DeclTypeFamilyInst span' _ -> span'
      DeclDataFamilyInst span' _ -> span'
      DeclPragma span' _ -> span'

data ValueDecl
  = FunctionBind SourceSpan BinderName [Match]
  | PatternBind SourceSpan Pattern Rhs
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan ValueDecl where
  getSourceSpan valueDecl =
    case valueDecl of
      FunctionBind span' _ _ -> span'
      PatternBind span' _ _ -> span'

data Match = Match
  { matchSpan :: SourceSpan,
    matchHeadForm :: MatchHeadForm,
    matchPats :: [Pattern],
    matchRhs :: Rhs
  }
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan Match where
  getSourceSpan = matchSpan

data MatchHeadForm
  = MatchHeadPrefix
  | MatchHeadInfix
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
data PatSynDecl = PatSynDecl
  { patSynDeclSpan :: SourceSpan,
    patSynDeclName :: Text,
    patSynDeclArgs :: PatSynArgs,
    patSynDeclPat :: Pattern,
    patSynDeclDir :: PatSynDir
  }
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan PatSynDecl where
  getSourceSpan = patSynDeclSpan

data Rhs
  = UnguardedRhs SourceSpan Expr
  | GuardedRhss SourceSpan [GuardedRhs]
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan Rhs where
  getSourceSpan rhs =
    case rhs of
      UnguardedRhs span' _ -> span'
      GuardedRhss span' _ -> span'

data GuardedRhs = GuardedRhs
  { guardedRhsSpan :: SourceSpan,
    guardedRhsGuards :: [GuardQualifier],
    guardedRhsBody :: Expr
  }
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan GuardedRhs where
  getSourceSpan = guardedRhsSpan

data GuardQualifier
  = GuardExpr SourceSpan Expr
  | GuardPat SourceSpan Pattern Expr
  | GuardLet SourceSpan [Decl]
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan GuardQualifier where
  getSourceSpan qualifier =
    case qualifier of
      GuardExpr span' _ -> span'
      GuardPat span' _ _ -> span'
      GuardLet span' _ -> span'

data Literal
  = LitInt SourceSpan Integer Text
  | LitIntHash SourceSpan Integer Text
  | LitIntBase SourceSpan Integer Text
  | LitIntBaseHash SourceSpan Integer Text
  | LitFloat SourceSpan Double Text
  | LitFloatHash SourceSpan Double Text
  | LitChar SourceSpan Char Text
  | LitCharHash SourceSpan Char Text
  | LitString SourceSpan Text Text
  | LitStringHash SourceSpan Text Text
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan Literal where
  getSourceSpan literal =
    case literal of
      LitInt span' _ _ -> span'
      LitIntHash span' _ _ -> span'
      LitIntBase span' _ _ -> span'
      LitIntBaseHash span' _ _ -> span'
      LitFloat span' _ _ -> span'
      LitFloatHash span' _ _ -> span'
      LitChar span' _ _ -> span'
      LitCharHash span' _ _ -> span'
      LitString span' _ _ -> span'
      LitStringHash span' _ _ -> span'

data TupleFlavor
  = Boxed
  | Unboxed
  deriving (Data, Eq, Show, Generic, NFData)

data Pattern
  = PAnn Annotation Pattern
  | PVar SourceSpan Text
  | PWildcard SourceSpan
  | PLit SourceSpan Literal
  | PQuasiQuote SourceSpan Text Text
  | PTuple SourceSpan TupleFlavor [Pattern]
  | PUnboxedSum SourceSpan Int Int Pattern
  | PList SourceSpan [Pattern]
  | PCon SourceSpan Text [Pattern]
  | PInfix SourceSpan Pattern Text Pattern
  | PView SourceSpan Expr Pattern
  | PAs SourceSpan Text Pattern
  | PStrict SourceSpan Pattern
  | PIrrefutable SourceSpan Pattern
  | PNegLit SourceSpan Literal
  | PParen SourceSpan Pattern
  | PRecord SourceSpan Text [(Text, Pattern)] Bool -- Bool: wildcard present
  | PTypeSig SourceSpan Pattern Type
  | PSplice SourceSpan Expr
  -- \$pat or $(pat) (TH pattern splice)
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan Pattern where
  getSourceSpan pat =
    case pat of
      PAnn _ sub -> getSourceSpan sub
      PVar span' _ -> span'
      PWildcard span' -> span'
      PLit span' _ -> span'
      PQuasiQuote span' _ _ -> span'
      PTuple span' _ _ -> span'
      PUnboxedSum span' _ _ _ -> span'
      PList span' _ -> span'
      PCon span' _ _ -> span'
      PInfix span' _ _ _ -> span'
      PView span' _ _ -> span'
      PAs span' _ _ -> span'
      PStrict span' _ -> span'
      PIrrefutable span' _ -> span'
      PNegLit span' _ -> span'
      PParen span' _ -> span'
      PRecord span' _ _ _ -> span'
      PTypeSig span' _ _ -> span'
      PSplice span' _ -> span'

data Type
  = TAnn Annotation Type
  | TVar SourceSpan Text
  | TCon SourceSpan Text TypePromotion
  | TImplicitParam SourceSpan Text Type
  | TTypeLit SourceSpan TypeLiteral
  | TStar SourceSpan
  | TQuasiQuote SourceSpan Text Text
  | TForall SourceSpan [Text] Type
  | TApp SourceSpan Type Type
  | TFun SourceSpan Type Type
  | TTuple SourceSpan TupleFlavor TypePromotion [Type]
  | TUnboxedSum SourceSpan [Type]
  | TList SourceSpan TypePromotion [Type]
  | TParen SourceSpan Type
  | TKindSig SourceSpan Type Type
  | TContext SourceSpan [Type] Type
  | TSplice SourceSpan Expr
  | -- \$typ or $(typ) (TH type splice)
    -- \_ (wildcard type, used in type family instance patterns)
    TWildcard SourceSpan
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan Type where
  getSourceSpan ty =
    case ty of
      TAnn _ sub -> getSourceSpan sub
      TVar span' _ -> span'
      TCon span' _ _ -> span'
      TImplicitParam span' _ _ -> span'
      TTypeLit span' _ -> span'
      TStar span' -> span'
      TQuasiQuote span' _ _ -> span'
      TForall span' _ _ -> span'
      TApp span' _ _ -> span'
      TFun span' _ _ -> span'
      TTuple span' _ _ _ -> span'
      TUnboxedSum span' _ -> span'
      TList span' _ _ -> span'
      TParen span' _ -> span'
      TKindSig span' _ _ -> span'
      TContext span' _ _ -> span'
      TSplice span' _ -> span'
      TWildcard span' -> span'

data TypeLiteral
  = TypeLitInteger Integer Text
  | TypeLitSymbol Text Text
  | TypeLitChar Char Text
  deriving (Data, Eq, Show, Generic, NFData)

data TypePromotion
  = Unpromoted
  | Promoted
  deriving (Data, Eq, Show, Generic, NFData)

data TyVarBinder = TyVarBinder
  { tyVarBinderSpan :: SourceSpan,
    tyVarBinderName :: Text,
    tyVarBinderKind :: Maybe Type
  }
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan TyVarBinder where
  getSourceSpan = tyVarBinderSpan

data TypeHeadForm
  = TypeHeadPrefix
  | TypeHeadInfix
  deriving (Data, Eq, Show, Generic, NFData)

data Role
  = RoleNominal
  | RoleRepresentational
  | RolePhantom
  | RoleInfer
  deriving (Data, Eq, Show, Generic, NFData)

data RoleAnnotation = RoleAnnotation
  { roleAnnotationSpan :: SourceSpan,
    roleAnnotationName :: Text,
    roleAnnotationRoles :: [Role]
  }
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan RoleAnnotation where
  getSourceSpan = roleAnnotationSpan

data TypeSynDecl = TypeSynDecl
  { typeSynSpan :: SourceSpan,
    typeSynName :: Text,
    typeSynParams :: [TyVarBinder],
    typeSynBody :: Type
  }
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan TypeSynDecl where
  getSourceSpan = typeSynSpan

-- | Open or closed type synonym family declaration.
-- Used for top-level @type family F a@ and associated @type F a :: Kind@ in class bodies.
data TypeFamilyDecl = TypeFamilyDecl
  { typeFamilyDeclSpan :: SourceSpan,
    typeFamilyDeclHeadForm :: TypeHeadForm,
    -- | Family head type. For simple families like @type family F a@, this is @TCon "F"@.
    -- For infix families like @type family l `And` r@, this is the full infix type.
    typeFamilyDeclHead :: Type,
    typeFamilyDeclParams :: [TyVarBinder],
    -- | Optional result kind annotation (@:: Kind@)
    typeFamilyDeclKind :: Maybe Type,
    -- | @Nothing@ = open family; @Just eqs@ = closed family with equations
    typeFamilyDeclEquations :: Maybe [TypeFamilyEq]
  }
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan TypeFamilyDecl where
  getSourceSpan = typeFamilyDeclSpan

-- | One equation in a closed type family: @[forall binders.] LhsType = RhsType@
data TypeFamilyEq = TypeFamilyEq
  { typeFamilyEqSpan :: SourceSpan,
    typeFamilyEqForall :: [TyVarBinder],
    typeFamilyEqHeadForm :: TypeHeadForm,
    typeFamilyEqLhs :: Type,
    typeFamilyEqRhs :: Type
  }
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan TypeFamilyEq where
  getSourceSpan = typeFamilyEqSpan

-- | Data family declaration (standalone or associated in a class body).
data DataFamilyDecl = DataFamilyDecl
  { dataFamilyDeclSpan :: SourceSpan,
    dataFamilyDeclName :: Text,
    dataFamilyDeclParams :: [TyVarBinder],
    -- | Optional result kind annotation (@:: Kind@)
    dataFamilyDeclKind :: Maybe Type
  }
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan DataFamilyDecl where
  getSourceSpan = dataFamilyDeclSpan

-- | Type family instance: @type [instance] [forall binders.] LhsType = RhsType@
data TypeFamilyInst = TypeFamilyInst
  { typeFamilyInstSpan :: SourceSpan,
    typeFamilyInstForall :: [TyVarBinder],
    typeFamilyInstHeadForm :: TypeHeadForm,
    typeFamilyInstLhs :: Type,
    typeFamilyInstRhs :: Type
  }
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan TypeFamilyInst where
  getSourceSpan = typeFamilyInstSpan

-- | Data or newtype family instance (standalone or in an instance body).
data DataFamilyInst = DataFamilyInst
  { dataFamilyInstSpan :: SourceSpan,
    -- | @True@ when declared with @newtype instance@
    dataFamilyInstIsNewtype :: Bool,
    dataFamilyInstForall :: [TyVarBinder],
    -- | The LHS type-application pattern (e.g. @GMap (Either a b) v@)
    dataFamilyInstHead :: Type,
    dataFamilyInstConstructors :: [DataConDecl],
    dataFamilyInstDeriving :: [DerivingClause]
  }
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan DataFamilyInst where
  getSourceSpan = dataFamilyInstSpan

data DataDecl = DataDecl
  { dataDeclSpan :: SourceSpan,
    dataDeclContext :: [Type],
    dataDeclName :: Text,
    dataDeclParams :: [TyVarBinder],
    dataDeclConstructors :: [DataConDecl],
    dataDeclDeriving :: [DerivingClause]
  }
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan DataDecl where
  getSourceSpan = dataDeclSpan

data NewtypeDecl = NewtypeDecl
  { newtypeDeclSpan :: SourceSpan,
    newtypeDeclContext :: [Type],
    newtypeDeclName :: Text,
    newtypeDeclParams :: [TyVarBinder],
    newtypeDeclConstructor :: Maybe DataConDecl,
    newtypeDeclDeriving :: [DerivingClause]
  }
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan NewtypeDecl where
  getSourceSpan = newtypeDeclSpan

data DataConDecl
  = PrefixCon SourceSpan [Text] [Type] Text [BangType]
  | InfixCon SourceSpan [Text] [Type] BangType Text BangType
  | RecordCon SourceSpan [Text] [Type] Text [FieldDecl]
  | -- | GADT-style constructor: @Con :: forall a. Ctx => Type@
    -- The list of names supports multiple constructors: @T1, T2 :: Type@
    GadtCon SourceSpan [TyVarBinder] [Type] [Text] GadtBody
  deriving (Data, Eq, Show, Generic, NFData)

-- | Body of a GADT constructor after the @::@ and optional forall/context
data GadtBody
  = -- | Prefix body: @a -> b -> T a@
    GadtPrefixBody [BangType] Type
  | -- | Record body: @{ field :: Type } -> T a@
    GadtRecordBody [FieldDecl] Type
  deriving (Data, Eq, Show, Generic, NFData)

-- | Get the result type from a GADT body
gadtBodyResultType :: GadtBody -> Type
gadtBodyResultType body =
  case body of
    GadtPrefixBody _ ty -> ty
    GadtRecordBody _ ty -> ty

instance HasSourceSpan DataConDecl where
  getSourceSpan dataConDecl =
    case dataConDecl of
      PrefixCon span' _ _ _ _ -> span'
      InfixCon span' _ _ _ _ _ -> span'
      RecordCon span' _ _ _ _ -> span'
      GadtCon span' _ _ _ _ -> span'

data BangType = BangType
  { bangSpan :: SourceSpan,
    bangSourceUnpackedness :: SourceUnpackedness,
    bangStrict :: Bool,
    bangType :: Type
  }
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan BangType where
  getSourceSpan = bangSpan

data SourceUnpackedness
  = NoSourceUnpackedness
  | SourceUnpack
  | SourceNoUnpack
  deriving (Data, Eq, Show, Generic, NFData)

data FieldDecl = FieldDecl
  { fieldSpan :: SourceSpan,
    fieldNames :: [Text],
    fieldType :: BangType
  }
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan FieldDecl where
  getSourceSpan = fieldSpan

data DerivingClause = DerivingClause
  { derivingStrategy :: Maybe DerivingStrategy,
    derivingClasses :: [Type],
    derivingViaType :: Maybe Type
  }
  deriving (Data, Eq, Show, Generic, NFData)

data DerivingStrategy
  = DerivingStock
  | DerivingNewtype
  | DerivingAnyclass
  deriving (Data, Eq, Show, Generic, NFData)

data StandaloneDerivingDecl = StandaloneDerivingDecl
  { standaloneDerivingSpan :: SourceSpan,
    standaloneDerivingStrategy :: Maybe DerivingStrategy,
    standaloneDerivingViaType :: Maybe Type,
    standaloneDerivingOverlapPragma :: Maybe InstanceOverlapPragma,
    standaloneDerivingWarning :: Maybe WarningText,
    standaloneDerivingForall :: [TyVarBinder],
    standaloneDerivingContext :: [Type],
    standaloneDerivingParenthesizedHead :: Bool,
    standaloneDerivingClassName :: Text,
    standaloneDerivingTypes :: [Type]
  }
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan StandaloneDerivingDecl where
  getSourceSpan = standaloneDerivingSpan

data ClassDecl = ClassDecl
  { classDeclSpan :: SourceSpan,
    classDeclContext :: Maybe [Type],
    classDeclHeadForm :: TypeHeadForm,
    classDeclName :: Text,
    classDeclParams :: [TyVarBinder],
    classDeclFundeps :: [FunctionalDependency],
    classDeclItems :: [ClassDeclItem]
  }
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan ClassDecl where
  getSourceSpan = classDeclSpan

data FunctionalDependency = FunctionalDependency
  { functionalDependencySpan :: SourceSpan,
    functionalDependencyDeterminers :: [Text],
    functionalDependencyDetermined :: [Text]
  }
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan FunctionalDependency where
  getSourceSpan = functionalDependencySpan

data ClassDeclItem
  = ClassItemTypeSig SourceSpan [BinderName] Type
  | ClassItemDefaultSig SourceSpan BinderName Type
  | ClassItemFixity SourceSpan FixityAssoc (Maybe IEEntityNamespace) (Maybe Int) [OperatorName]
  | ClassItemDefault SourceSpan ValueDecl
  | ClassItemTypeFamilyDecl SourceSpan TypeFamilyDecl
  | ClassItemDataFamilyDecl SourceSpan DataFamilyDecl
  | ClassItemDefaultTypeInst SourceSpan TypeFamilyInst
  | -- pragma inside class body
    ClassItemPragma SourceSpan Pragma
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan ClassDeclItem where
  getSourceSpan classDeclItem =
    case classDeclItem of
      ClassItemTypeSig span' _ _ -> span'
      ClassItemDefaultSig span' _ _ -> span'
      ClassItemFixity span' _ _ _ _ -> span'
      ClassItemDefault span' _ -> span'
      ClassItemTypeFamilyDecl span' _ -> span'
      ClassItemDataFamilyDecl span' _ -> span'
      ClassItemDefaultTypeInst span' _ -> span'
      ClassItemPragma span' _ -> span'

data InstanceDecl = InstanceDecl
  { instanceDeclSpan :: SourceSpan,
    instanceDeclOverlapPragma :: Maybe InstanceOverlapPragma,
    instanceDeclWarning :: Maybe WarningText,
    instanceDeclForall :: [TyVarBinder],
    instanceDeclContext :: [Type],
    instanceDeclParenthesizedHead :: Bool,
    instanceDeclClassName :: Text,
    instanceDeclTypes :: [Type],
    instanceDeclItems :: [InstanceDeclItem]
  }
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan InstanceDecl where
  getSourceSpan = instanceDeclSpan

data InstanceOverlapPragma
  = Overlapping
  | Overlappable
  | Overlaps
  | Incoherent
  deriving (Data, Eq, Ord, Show, Read, Generic, NFData)

data InstanceDeclItem
  = InstanceItemBind SourceSpan ValueDecl
  | InstanceItemTypeSig SourceSpan [BinderName] Type
  | InstanceItemFixity SourceSpan FixityAssoc (Maybe IEEntityNamespace) (Maybe Int) [OperatorName]
  | InstanceItemTypeFamilyInst SourceSpan TypeFamilyInst
  | InstanceItemDataFamilyInst SourceSpan DataFamilyInst
  | -- pragma inside instance body (e.g. {-# SPECIALIZE instance ... #-})
    InstanceItemPragma SourceSpan Pragma
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan InstanceDeclItem where
  getSourceSpan instanceDeclItem =
    case instanceDeclItem of
      InstanceItemBind span' _ -> span'
      InstanceItemTypeSig span' _ _ -> span'
      InstanceItemFixity span' _ _ _ _ -> span'
      InstanceItemTypeFamilyInst span' _ -> span'
      InstanceItemDataFamilyInst span' _ -> span'
      InstanceItemPragma span' _ -> span'

data FixityAssoc
  = Infix
  | InfixL
  | InfixR
  deriving (Data, Eq, Show, Generic, NFData)

data ForeignDecl = ForeignDecl
  { foreignDeclSpan :: SourceSpan,
    foreignDirection :: ForeignDirection,
    foreignCallConv :: CallConv,
    foreignSafety :: Maybe ForeignSafety,
    foreignEntity :: ForeignEntitySpec,
    foreignName :: Text,
    foreignType :: Type
  }
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan ForeignDecl where
  getSourceSpan = foreignDeclSpan

data ForeignEntitySpec
  = ForeignEntityDynamic
  | ForeignEntityWrapper
  | ForeignEntityStatic (Maybe Text)
  | ForeignEntityAddress (Maybe Text)
  | ForeignEntityNamed Text
  | ForeignEntityOmitted
  deriving (Data, Eq, Show, Generic, NFData)

data ForeignDirection
  = ForeignImport
  | ForeignExport
  deriving (Data, Eq, Show, Generic, NFData)

data CallConv
  = CCall
  | StdCall
  | CApi
  deriving (Data, Eq, Show, Generic, NFData)

data ForeignSafety
  = Safe
  | Unsafe
  deriving (Data, Eq, Show, Generic, NFData)

newtype Annotation = Annotation Dynamic
  deriving (Show, Generic)

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

data Expr
  = EAnn Annotation Expr
  | EVar SourceSpan Text
  | EInt SourceSpan Integer Text
  | EIntHash SourceSpan Integer Text
  | EIntBase SourceSpan Integer Text
  | EIntBaseHash SourceSpan Integer Text
  | EFloat SourceSpan Double Text
  | EFloatHash SourceSpan Double Text
  | EChar SourceSpan Char Text
  | ECharHash SourceSpan Char Text
  | EString SourceSpan Text Text
  | EStringHash SourceSpan Text Text
  | EOverloadedLabel SourceSpan Text Text
  | EQuasiQuote SourceSpan Text Text
  | EIf SourceSpan Expr Expr Expr
  | EMultiWayIf SourceSpan [GuardedRhs]
  | ELambdaPats SourceSpan [Pattern] Expr
  | ELambdaCase SourceSpan [CaseAlt]
  | EInfix SourceSpan Expr Text Expr
  | ENegate SourceSpan Expr
  | ESectionL SourceSpan Expr Text
  | ESectionR SourceSpan Text Expr
  | ELetDecls SourceSpan [Decl] Expr
  | ECase SourceSpan Expr [CaseAlt]
  | EDo SourceSpan [DoStmt Expr] Bool -- Bool: True = mdo, False = do
  | EListComp SourceSpan Expr [CompStmt]
  | EListCompParallel SourceSpan Expr [[CompStmt]]
  | EArithSeq SourceSpan ArithSeq
  | ERecordCon SourceSpan Text [(Text, Expr)] Bool -- Bool: wildcard present
  | ERecordUpd SourceSpan Expr [(Text, Expr)]
  | ETypeSig SourceSpan Expr Type
  | EParen SourceSpan Expr
  | EWhereDecls SourceSpan Expr [Decl]
  | EList SourceSpan [Expr]
  | ETuple SourceSpan TupleFlavor [Maybe Expr]
  | EUnboxedSum SourceSpan Int Int Expr
  | ETypeApp SourceSpan Expr Type
  | EApp SourceSpan Expr Expr
  | -- Template Haskell quotes
    ETHExpQuote SourceSpan Expr -- [| expr |] or [e| expr |]
  | ETHTypedQuote SourceSpan Expr -- [|| expr ||] or [e|| expr ||]
  | ETHDeclQuote SourceSpan [Decl] -- [d| decls |]
  | ETHTypeQuote SourceSpan Type -- [t| type |]
  | ETHPatQuote SourceSpan Pattern -- [p| pat |]
  | ETHNameQuote SourceSpan Text -- 'name
  | ETHTypeNameQuote SourceSpan Text -- ''Name
  | -- Template Haskell splices
    ETHSplice SourceSpan Expr
  | -- \$expr or $(expr)
    ETHTypedSplice SourceSpan Expr -- \$$expr or $$(expr)
  | -- Arrow notation (Arrows extension)
    EProc SourceSpan Pattern Cmd -- proc pat -> cmd
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan Expr where
  getSourceSpan expr =
    case expr of
      EVar span' _ -> span'
      EInt span' _ _ -> span'
      EIntHash span' _ _ -> span'
      EIntBase span' _ _ -> span'
      EIntBaseHash span' _ _ -> span'
      EFloat span' _ _ -> span'
      EFloatHash span' _ _ -> span'
      EChar span' _ _ -> span'
      ECharHash span' _ _ -> span'
      EString span' _ _ -> span'
      EStringHash span' _ _ -> span'
      EOverloadedLabel span' _ _ -> span'
      EQuasiQuote span' _ _ -> span'
      EIf span' _ _ _ -> span'
      EMultiWayIf span' _ -> span'
      ELambdaPats span' _ _ -> span'
      ELambdaCase span' _ -> span'
      EInfix span' _ _ _ -> span'
      ENegate span' _ -> span'
      ESectionL span' _ _ -> span'
      ESectionR span' _ _ -> span'
      ELetDecls span' _ _ -> span'
      ECase span' _ _ -> span'
      EDo span' _ _ -> span'
      EListComp span' _ _ -> span'
      EListCompParallel span' _ _ -> span'
      EArithSeq span' _ -> span'
      ERecordCon span' _ _ _ -> span'
      ERecordUpd span' _ _ -> span'
      ETypeSig span' _ _ -> span'
      EParen span' _ -> span'
      EWhereDecls span' _ _ -> span'
      EList span' _ -> span'
      ETuple span' _ _ -> span'
      EUnboxedSum span' _ _ _ -> span'
      ETypeApp span' _ _ -> span'
      EApp span' _ _ -> span'
      ETHExpQuote span' _ -> span'
      ETHTypedQuote span' _ -> span'
      ETHDeclQuote span' _ -> span'
      ETHTypeQuote span' _ -> span'
      ETHPatQuote span' _ -> span'
      ETHNameQuote span' _ -> span'
      ETHTypeNameQuote span' _ -> span'
      ETHSplice span' _ -> span'
      ETHTypedSplice span' _ -> span'
      EProc span' _ _ -> span'
      EAnn _ sub -> getSourceSpan sub

data CaseAlt = CaseAlt
  { caseAltSpan :: SourceSpan,
    caseAltPattern :: Pattern,
    caseAltRhs :: Rhs
  }
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan CaseAlt where
  getSourceSpan = caseAltSpan

data DoStmt body
  = DoBind SourceSpan Pattern body
  | DoLet SourceSpan [(Text, Expr)]
  | DoLetDecls SourceSpan [Decl]
  | DoExpr SourceSpan body
  | DoRecStmt SourceSpan [DoStmt body] -- rec { stmts }
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan (DoStmt body) where
  getSourceSpan doStmt =
    case doStmt of
      DoBind span' _ _ -> span'
      DoLet span' _ -> span'
      DoLetDecls span' _ -> span'
      DoExpr span' _ -> span'
      DoRecStmt span' _ -> span'

-- | Arrow command type (used inside 'proc' expressions).
-- Commands mirror expressions but live in a separate namespace so the
-- pretty-printer knows not to parenthesise @do { … }@ as an infix LHS.
data Cmd
  = -- | @exp -\< exp@ or @exp -\<\< exp@
    CmdArrApp SourceSpan Expr ArrAppType Expr
  | -- | Command-level infix: @cmd1 op cmd2@
    CmdInfix SourceSpan Cmd Text Cmd
  | -- | @do { cstmts }@
    CmdDo SourceSpan [DoStmt Cmd]
  | -- | @if exp then cmd else cmd@
    CmdIf SourceSpan Expr Cmd Cmd
  | -- | @case exp of { calts }@
    CmdCase SourceSpan Expr [CmdCaseAlt]
  | -- | @let decls in cmd@
    CmdLet SourceSpan [Decl] Cmd
  | -- | @\\pats -> cmd@
    CmdLam SourceSpan [Pattern] Cmd
  | -- | Command application: @cmd exp@
    CmdApp SourceSpan Cmd Expr
  | -- | Parenthesised command: @(cmd)@
    CmdPar SourceSpan Cmd
  deriving (Data, Eq, Show, Generic, NFData)

-- | Arrow application type: first-order (@-\<@) or higher-order (@-\<\<@).
data ArrAppType = HsFirstOrderApp | HsHigherOrderApp
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan Cmd where
  getSourceSpan cmd =
    case cmd of
      CmdArrApp span' _ _ _ -> span'
      CmdInfix span' _ _ _ -> span'
      CmdDo span' _ -> span'
      CmdIf span' _ _ _ -> span'
      CmdCase span' _ _ -> span'
      CmdLet span' _ _ -> span'
      CmdLam span' _ _ -> span'
      CmdApp span' _ _ -> span'
      CmdPar span' _ -> span'

-- | Case alternative with a command body (used in arrow @case@ commands).
data CmdCaseAlt = CmdCaseAlt
  { cmdCaseAltSpan :: SourceSpan,
    cmdCaseAltPat :: Pattern,
    cmdCaseAltBody :: Cmd
  }
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan CmdCaseAlt where
  getSourceSpan = cmdCaseAltSpan

data CompStmt
  = CompGen SourceSpan Pattern Expr
  | CompGuard SourceSpan Expr
  | CompLet SourceSpan [(Text, Expr)]
  | CompLetDecls SourceSpan [Decl]
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan CompStmt where
  getSourceSpan compStmt =
    case compStmt of
      CompGen span' _ _ -> span'
      CompGuard span' _ -> span'
      CompLet span' _ -> span'
      CompLetDecls span' _ -> span'

data ArithSeq
  = ArithSeqFrom SourceSpan Expr
  | ArithSeqFromThen SourceSpan Expr Expr
  | ArithSeqFromTo SourceSpan Expr Expr
  | ArithSeqFromThenTo SourceSpan Expr Expr Expr
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan ArithSeq where
  getSourceSpan arithSeq =
    case arithSeq of
      ArithSeqFrom span' _ -> span'
      ArithSeqFromThen span' _ _ -> span'
      ArithSeqFromTo span' _ _ -> span'
      ArithSeqFromThenTo span' _ _ _ -> span'

valueDeclBinderName :: ValueDecl -> Maybe Text
valueDeclBinderName vdecl =
  case vdecl of
    FunctionBind _ name _ -> Just name
    PatternBind _ pat _ ->
      case pat of
        PVar _ name -> Just name
        _ -> Nothing

declValueBinderNames :: Decl -> [Text]
declValueBinderNames decl =
  case decl of
    DeclValue _ vdecl ->
      case valueDeclBinderName vdecl of
        Just name -> [name]
        Nothing -> []
    _ -> []
