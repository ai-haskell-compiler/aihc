{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

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
    Name (..),
    NameType (..),
    UnqualifiedName (..),
    WarningText (..),
    Annotation,
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
    SourceSpan (..),
    SourceUnpackedness (..),
    StandaloneDerivingDecl (..),
    Type (..),
    TupleFlavor (..),
    TypeSyntaxForm (..),
    TypeLiteral (..),
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
    effectiveExtensions,
    extensionName,
    extensionSettingName,
    gadtBodyResultType,
    languageEditionExtensions,
    editionFromExtensionSettings,
    noSourceSpan,
    mergeSourceSpans,
    mkName,
    mkQualifiedName,
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
    moduleWarningText,
    moduleExports,
    mkAnnotation,
    fromAnnotation,
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
    getArithSeqSourceSpan,
    getClassDeclItemSourceSpan,
    getCmdSourceSpan,
    getCompStmtSourceSpan,
    getDataConDeclSourceSpan,
    getDeclSourceSpan,
    getDoStmtSourceSpan,
    getExportSpecSourceSpan,
    getExprSourceSpan,
    getGuardQualifierSourceSpan,
    getImportItemSourceSpan,
    getInstanceDeclItemSourceSpan,
    getLiteralSourceSpan,
    getPatternSourceSpan,
    getTypeSourceSpan,
    getWarningTextSourceSpan,
  )
where

import Control.DeepSeq (NFData (..))
import Data.Char (GeneralCategory (..), generalCategory)
import Data.Data (Constr, Data (..), DataType, Fixity (Prefix), mkConstr, mkDataType)
import Data.Dynamic (Dynamic, Typeable, fromDynamic, toDyn)
import Data.List (sort)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

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
  Map.lookup trimmed extensionMap
  where
    extensionMap :: Map.Map Text Extension
    extensionMap = Map.fromList $ aliases ++ [(T.pack (show ext), ext) | ext <- [minBound .. maxBound]]
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
-- 'nameQualifier' is 'Nothing'.
data Name = Name
  { -- | Module qualifier (e.g., @\"Data.List\"@ for @Data.List.map@)
    nameQualifier :: Maybe Text,
    -- | Whether this is a variable, constructor, or operator
    nameType :: NameType,
    -- | The local name (e.g., @\"map\"@, @\".+.\"@)
    nameText :: Text
  }
  deriving (Eq, Show, Generic, NFData, Data)

data UnqualifiedName = UnqualifiedName
  { unqualifiedNameType :: NameType,
    unqualifiedNameText :: Text
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
mkName = Name

mkQualifiedName :: UnqualifiedName -> Maybe Text -> Name
mkQualifiedName name qualifier =
  Name qualifier (unqualifiedNameType name) (unqualifiedNameText name)

qualifyName :: Maybe Text -> UnqualifiedName -> Name
qualifyName qualifier name =
  Name qualifier (unqualifiedNameType name) (unqualifiedNameText name)

mkUnqualifiedName :: NameType -> Text -> UnqualifiedName
mkUnqualifiedName = UnqualifiedName

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
   in Name qualifier (inferNameType localName) localName
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
unqualifiedNameFromText txt = UnqualifiedName (inferNameType txt) txt

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
isIdentChar c = isIdentifierStartChar c || isIdentifierNumberChar c || c == '\''

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
    OtherPunctuation -> c > '\x7f'
    _ -> False

type BinderName = UnqualifiedName

type OperatorName = UnqualifiedName

data WarningText
  = DeprText Text
  | WarnText Text
  | WarningTextAnn Annotation WarningText
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

getWarningTextSourceSpan :: WarningText -> SourceSpan
getWarningTextSourceSpan warningText =
  case warningText of
    DeprText _ -> NoSourceSpan
    WarnText _ -> NoSourceSpan
    WarningTextAnn ann sub
      | Just srcSpan <- fromAnnotation ann -> srcSpan
      | otherwise -> getWarningTextSourceSpan sub

data Module = Module
  { moduleAnns :: [Annotation],
    moduleHead :: Maybe ModuleHead,
    moduleLanguagePragmas :: [ExtensionSetting],
    moduleImports :: [ImportDecl],
    moduleDecls :: [Decl]
  }
  deriving (Data, Eq, Show, Generic, NFData)

data ModuleHead = ModuleHead
  { moduleHeadAnns :: [Annotation],
    moduleHeadName :: Text,
    moduleHeadWarningText :: Maybe WarningText,
    moduleHeadExports :: Maybe [ExportSpec]
  }
  deriving (Data, Eq, Show, Generic, NFData)

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
    ieBundledMemberName :: Name
  }
  deriving (Data, Eq, Show, Generic, NFData)

data ExportSpec
  = ExportModule (Maybe WarningText) Text
  | ExportVar (Maybe WarningText) (Maybe IEEntityNamespace) Name
  | ExportAbs (Maybe WarningText) (Maybe IEEntityNamespace) Name
  | ExportAll (Maybe WarningText) (Maybe IEEntityNamespace) Name
  | ExportWith (Maybe WarningText) (Maybe IEEntityNamespace) Name [IEBundledMember]
  | ExportWithAll (Maybe WarningText) (Maybe IEEntityNamespace) Name [IEBundledMember]
  | ExportAnn Annotation ExportSpec
  deriving (Data, Eq, Show, Generic, NFData)

getExportSpecSourceSpan :: ExportSpec -> SourceSpan
getExportSpecSourceSpan spec =
  case spec of
    ExportAnn ann sub
      | Just srcSpan <- fromAnnotation ann -> srcSpan
      | otherwise -> getExportSpecSourceSpan sub
    _ -> NoSourceSpan

data ImportDecl = ImportDecl
  { importDeclAnns :: [Annotation],
    importDeclLevel :: Maybe ImportLevel,
    importDeclPackage :: Maybe Text,
    importDeclSource :: Bool,
    importDeclSafe :: Bool,
    importDeclQualified :: Bool,
    importDeclQualifiedPost :: Bool,
    importDeclModule :: Text,
    importDeclAs :: Maybe Text,
    importDeclSpec :: Maybe ImportSpec
  }
  deriving (Data, Eq, Show, Generic, NFData)

data ImportLevel
  = ImportLevelQuote
  | ImportLevelSplice
  deriving (Data, Eq, Show, Generic, NFData)

data ImportSpec = ImportSpec
  { importSpecAnns :: [Annotation],
    importSpecHiding :: Bool,
    importSpecItems :: [ImportItem]
  }
  deriving (Data, Eq, Show, Generic, NFData)

data ImportItem
  = ImportItemVar (Maybe IEEntityNamespace) UnqualifiedName
  | ImportItemAbs (Maybe IEEntityNamespace) UnqualifiedName
  | ImportItemAll (Maybe IEEntityNamespace) UnqualifiedName
  | ImportItemWith (Maybe IEEntityNamespace) UnqualifiedName [IEBundledMember]
  | ImportItemAllWith (Maybe IEEntityNamespace) UnqualifiedName [IEBundledMember]
  | ImportAnn Annotation ImportItem
  deriving (Data, Eq, Show, Generic, NFData)

getImportItemSourceSpan :: ImportItem -> SourceSpan
getImportItemSourceSpan item =
  case item of
    ImportAnn ann sub
      | Just srcSpan <- fromAnnotation ann -> srcSpan
      | otherwise -> getImportItemSourceSpan sub
    _ -> NoSourceSpan

data Decl
  = DeclAnn Annotation Decl
  | DeclValue ValueDecl
  | DeclTypeSig [BinderName] Type
  | DeclPatSyn PatSynDecl
  | DeclPatSynSig [BinderName] Type
  | DeclStandaloneKindSig BinderName Type
  | DeclFixity FixityAssoc (Maybe IEEntityNamespace) (Maybe Int) [OperatorName]
  | DeclRoleAnnotation RoleAnnotation
  | DeclTypeSyn TypeSynDecl
  | DeclTypeData DataDecl
  | DeclData DataDecl
  | DeclNewtype NewtypeDecl
  | DeclClass ClassDecl
  | DeclInstance InstanceDecl
  | DeclStandaloneDeriving StandaloneDerivingDecl
  | DeclDefault [Type]
  | -- \$decl or $(decl) (TH top-level splice)
    DeclSplice Expr
  | DeclForeign ForeignDecl
  | DeclTypeFamilyDecl TypeFamilyDecl
  | DeclDataFamilyDecl DataFamilyDecl
  | DeclTypeFamilyInst TypeFamilyInst
  | DeclDataFamilyInst DataFamilyInst
  | -- pragma declaration (e.g. {-# INLINE f #-}, {-# SPECIALIZE ... #-})
    DeclPragma Pragma
  deriving (Data, Eq, Show, Generic, NFData)

getDeclSourceSpan :: Decl -> SourceSpan
getDeclSourceSpan decl =
  case decl of
    DeclAnn ann sub
      | Just srcSpan <- fromAnnotation ann -> srcSpan
      | otherwise -> getDeclSourceSpan sub
    _ -> NoSourceSpan

-- | Peel nested 'DeclAnn' wrappers.
peelDeclAnn :: Decl -> Decl
peelDeclAnn (DeclAnn _ inner) = peelDeclAnn inner
peelDeclAnn d = d

data ValueDecl
  = FunctionBind BinderName [Match]
  | PatternBind Pattern Rhs
  deriving (Data, Eq, Show, Generic, NFData)

data Match = Match
  { matchAnns :: [Annotation],
    matchHeadForm :: MatchHeadForm,
    matchPats :: [Pattern],
    matchRhs :: Rhs
  }
  deriving (Data, Eq, Show, Generic, NFData)

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
  { patSynDeclName :: UnqualifiedName,
    patSynDeclArgs :: PatSynArgs,
    patSynDeclPat :: Pattern,
    patSynDeclDir :: PatSynDir
  }
  deriving (Data, Eq, Show, Generic, NFData)

data Rhs
  = UnguardedRhs [Annotation] Expr (Maybe [Decl])
  | GuardedRhss [Annotation] [GuardedRhs] (Maybe [Decl])
  deriving (Data, Eq, Show, Generic, NFData)

data GuardedRhs = GuardedRhs
  { guardedRhsAnns :: [Annotation],
    guardedRhsGuards :: [GuardQualifier],
    guardedRhsBody :: Expr
  }
  deriving (Data, Eq, Show, Generic, NFData)

data GuardQualifier
  = -- | Metadata for the whole guard qualifier (typically a 'SourceSpan' via 'mkAnnotation').
    GuardAnn Annotation GuardQualifier
  | GuardExpr Expr
  | GuardPat Pattern Expr
  | GuardLet [Decl]
  deriving (Data, Eq, Show, Generic, NFData)

peelGuardQualifierAnn :: GuardQualifier -> GuardQualifier
peelGuardQualifierAnn (GuardAnn _ inner) = peelGuardQualifierAnn inner
peelGuardQualifierAnn q = q

getGuardQualifierSourceSpan :: GuardQualifier -> SourceSpan
getGuardQualifierSourceSpan qualifier =
  case qualifier of
    GuardAnn ann sub
      | Just srcSpan <- fromAnnotation @SourceSpan ann -> srcSpan
      | otherwise -> getGuardQualifierSourceSpan sub
    _ -> NoSourceSpan

data Literal
  = LitAnn Annotation Literal
  | LitInt Integer Text
  | LitIntHash Integer Text
  | LitIntBase Integer Text
  | LitIntBaseHash Integer Text
  | LitFloat Double Text
  | LitFloatHash Double Text
  | LitChar Char Text
  | LitCharHash Char Text
  | LitString Text Text
  | LitStringHash Text Text
  deriving (Data, Eq, Show, Generic, NFData)

getLiteralSourceSpan :: Literal -> SourceSpan
getLiteralSourceSpan literal =
  case literal of
    LitAnn ann sub
      | Just srcSpan <- fromAnnotation @SourceSpan ann -> srcSpan
      | otherwise -> getLiteralSourceSpan sub
    _ -> NoSourceSpan

literalAnnSpan :: SourceSpan -> Literal -> Literal
literalAnnSpan sp = LitAnn (mkAnnotation sp)

peelLiteralAnn :: Literal -> Literal
peelLiteralAnn (LitAnn _ inner) = peelLiteralAnn inner
peelLiteralAnn lit = lit

data TupleFlavor
  = Boxed
  | Unboxed
  deriving (Data, Eq, Show, Generic, NFData)

data Pattern
  = PAnn Annotation Pattern
  | PVar UnqualifiedName
  | PTypeBinder TyVarBinder
  | PTypeSyntax TypeSyntaxForm Type
  | PWildcard
  | PLit Literal
  | PQuasiQuote Text Text
  | PTuple TupleFlavor [Pattern]
  | PUnboxedSum Int Int Pattern
  | PList [Pattern]
  | PCon Name [Type] [Pattern]
  | PInfix Pattern Name Pattern
  | PView Expr Pattern
  | PAs Text Pattern
  | PStrict Pattern
  | PIrrefutable Pattern
  | PNegLit Literal
  | PParen Pattern
  | PRecord Name [(Name, Pattern)] Bool -- Bool: wildcard present
  | PTypeSig Pattern Type
  | PSplice Expr
  -- \$pat or $(pat) (TH pattern splice)
  deriving (Data, Eq, Show, Generic, NFData)

getPatternSourceSpan :: Pattern -> SourceSpan
getPatternSourceSpan pat =
  case pat of
    PAnn ann sub
      | Just srcSpan <- fromAnnotation @SourceSpan ann -> srcSpan
      | otherwise -> getPatternSourceSpan sub
    _ -> NoSourceSpan

-- | Peel nested 'PAnn' wrappers.
peelPatternAnn :: Pattern -> Pattern
peelPatternAnn (PAnn _ inner) = peelPatternAnn inner
peelPatternAnn p = p

data TypeSyntaxForm
  = TypeSyntaxExplicitNamespace
  | TypeSyntaxInTerm
  deriving (Data, Eq, Show, Generic, NFData)

data ForallVis
  = ForallInvisible
  | ForallVisible
  deriving (Data, Eq, Show, Generic, NFData)

data ForallTelescope = ForallTelescope
  { forallTelescopeVisibility :: ForallVis,
    forallTelescopeBinders :: [TyVarBinder]
  }
  deriving (Data, Eq, Show, Generic, NFData)

data Type
  = TAnn Annotation Type
  | TVar UnqualifiedName
  | TCon Name TypePromotion
  | TImplicitParam Text Type
  | TTypeLit TypeLiteral
  | TStar
  | TQuasiQuote Text Text
  | TForall ForallTelescope Type
  | TApp Type Type
  | TFun Type Type
  | TTuple TupleFlavor TypePromotion [Type]
  | TUnboxedSum [Type]
  | TList TypePromotion [Type]
  | TParen Type
  | TKindSig Type Type
  | TContext [Type] Type
  | TSplice Expr
  | -- \$typ or $(typ) (TH type splice)
    -- \_ (wildcard type, used in type family instance patterns)
    TWildcard
  deriving (Data, Eq, Show, Generic, NFData)

getTypeSourceSpan :: Type -> SourceSpan
getTypeSourceSpan ty =
  case ty of
    TAnn ann sub
      | Just srcSpan <- fromAnnotation @SourceSpan ann -> srcSpan
      | otherwise -> getTypeSourceSpan sub
    _ -> NoSourceSpan

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

data TypeLiteral
  = TypeLitInteger Integer Text
  | TypeLitSymbol Text Text
  | TypeLitChar Char Text
  deriving (Data, Eq, Show, Generic, NFData)

data TypePromotion
  = Unpromoted
  | Promoted
  deriving (Data, Eq, Show, Generic, NFData)

data TyVarBSpecificity
  = TyVarBInferred
  | TyVarBSpecified
  deriving (Data, Eq, Show, Generic, NFData)

data TyVarBVisibility
  = TyVarBVisible
  | TyVarBInvisible
  deriving (Data, Eq, Show, Generic, NFData)

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
  { roleAnnotationName :: Text,
    roleAnnotationRoles :: [Role]
  }
  deriving (Data, Eq, Show, Generic, NFData)

data TypeSynDecl = TypeSynDecl
  { typeSynHeadForm :: TypeHeadForm,
    typeSynName :: Text,
    typeSynParams :: [TyVarBinder],
    typeSynBody :: Type
  }
  deriving (Data, Eq, Show, Generic, NFData)

-- | Open or closed type synonym family declaration.
-- Used for top-level @type family F a@ and associated @type F a :: Kind@ in class bodies.
data TypeFamilyDecl = TypeFamilyDecl
  { typeFamilyDeclHeadForm :: TypeHeadForm,
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

-- | One equation in a closed type family: @[forall binders.] LhsType = RhsType@
data TypeFamilyEq = TypeFamilyEq
  { typeFamilyEqAnns :: [Annotation],
    typeFamilyEqForall :: [TyVarBinder],
    typeFamilyEqHeadForm :: TypeHeadForm,
    typeFamilyEqLhs :: Type,
    typeFamilyEqRhs :: Type
  }
  deriving (Data, Eq, Show, Generic, NFData)

-- | Data family declaration (standalone or associated in a class body).
data DataFamilyDecl = DataFamilyDecl
  { dataFamilyDeclName :: UnqualifiedName,
    dataFamilyDeclParams :: [TyVarBinder],
    -- | Optional result kind annotation (@:: Kind@)
    dataFamilyDeclKind :: Maybe Type
  }
  deriving (Data, Eq, Show, Generic, NFData)

-- | Type family instance: @type [instance] [forall binders.] LhsType = RhsType@
data TypeFamilyInst = TypeFamilyInst
  { typeFamilyInstForall :: [TyVarBinder],
    typeFamilyInstHeadForm :: TypeHeadForm,
    typeFamilyInstLhs :: Type,
    typeFamilyInstRhs :: Type
  }
  deriving (Data, Eq, Show, Generic, NFData)

-- | Data or newtype family instance (standalone or in an instance body).
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

data DataDecl = DataDecl
  { dataDeclHeadForm :: TypeHeadForm,
    dataDeclContext :: [Type],
    dataDeclName :: UnqualifiedName,
    dataDeclParams :: [TyVarBinder],
    -- | Optional inline kind annotation (@:: Kind@) before @=@ or @where@
    dataDeclKind :: Maybe Type,
    dataDeclConstructors :: [DataConDecl],
    dataDeclDeriving :: [DerivingClause]
  }
  deriving (Data, Eq, Show, Generic, NFData)

data NewtypeDecl = NewtypeDecl
  { newtypeDeclHeadForm :: TypeHeadForm,
    newtypeDeclContext :: [Type],
    newtypeDeclName :: UnqualifiedName,
    newtypeDeclParams :: [TyVarBinder],
    -- | Optional inline kind annotation (@:: Kind@) before @=@ or @where@
    newtypeDeclKind :: Maybe Type,
    newtypeDeclConstructor :: Maybe DataConDecl,
    newtypeDeclDeriving :: [DerivingClause]
  }
  deriving (Data, Eq, Show, Generic, NFData)

data DataConDecl
  = -- | Metadata for the whole constructor declaration (typically a 'SourceSpan' via 'mkAnnotation').
    DataConAnn Annotation DataConDecl
  | PrefixCon [Text] [Type] UnqualifiedName [BangType]
  | InfixCon [Text] [Type] BangType UnqualifiedName BangType
  | RecordCon [Text] [Type] UnqualifiedName [FieldDecl]
  | -- | GADT-style constructor: @Con :: forall a. Ctx => Type@
    -- The list of names supports multiple constructors: @T1, T2 :: Type@
    GadtCon [ForallTelescope] [Type] [UnqualifiedName] GadtBody
  deriving (Data, Eq, Show, Generic, NFData)

-- | Strip nested 'DataConAnn' wrappers.
peelDataConAnn :: DataConDecl -> DataConDecl
peelDataConAnn (DataConAnn _ inner) = peelDataConAnn inner
peelDataConAnn d = d

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

getDataConDeclSourceSpan :: DataConDecl -> SourceSpan
getDataConDeclSourceSpan dataConDecl =
  case dataConDecl of
    DataConAnn ann sub
      | Just srcSpan <- fromAnnotation @SourceSpan ann -> srcSpan
      | otherwise -> getDataConDeclSourceSpan sub
    _ -> NoSourceSpan

data BangType = BangType
  { bangAnns :: [Annotation],
    bangSourceUnpackedness :: SourceUnpackedness,
    bangStrict :: Bool,
    bangLazy :: Bool,
    bangType :: Type
  }
  deriving (Data, Eq, Show, Generic, NFData)

data SourceUnpackedness
  = NoSourceUnpackedness
  | SourceUnpack
  | SourceNoUnpack
  deriving (Data, Eq, Show, Generic, NFData)

data FieldDecl = FieldDecl
  { fieldAnns :: [Annotation],
    fieldNames :: [UnqualifiedName],
    fieldType :: BangType
  }
  deriving (Data, Eq, Show, Generic, NFData)

data DerivingClause = DerivingClause
  { derivingStrategy :: Maybe DerivingStrategy,
    derivingClasses :: [Type],
    derivingViaType :: Maybe Type,
    derivingParenthesized :: Bool
  }
  deriving (Data, Eq, Show, Generic, NFData)

data DerivingStrategy
  = DerivingStock
  | DerivingNewtype
  | DerivingAnyclass
  deriving (Data, Eq, Show, Generic, NFData)

data StandaloneDerivingDecl = StandaloneDerivingDecl
  { standaloneDerivingStrategy :: Maybe DerivingStrategy,
    standaloneDerivingViaType :: Maybe Type,
    standaloneDerivingOverlapPragma :: Maybe InstanceOverlapPragma,
    standaloneDerivingWarning :: Maybe WarningText,
    standaloneDerivingForall :: [TyVarBinder],
    standaloneDerivingContext :: [Type],
    standaloneDerivingParenthesizedHead :: Bool,
    standaloneDerivingHeadForm :: TypeHeadForm,
    standaloneDerivingClassName :: UnqualifiedName,
    standaloneDerivingTypes :: [Type]
  }
  deriving (Data, Eq, Show, Generic, NFData)

data ClassDecl = ClassDecl
  { classDeclContext :: Maybe [Type],
    classDeclHeadForm :: TypeHeadForm,
    classDeclName :: Text,
    classDeclParams :: [TyVarBinder],
    classDeclFundeps :: [FunctionalDependency],
    classDeclItems :: [ClassDeclItem]
  }
  deriving (Data, Eq, Show, Generic, NFData)

data FunctionalDependency = FunctionalDependency
  { functionalDependencyAnns :: [Annotation],
    functionalDependencyDeterminers :: [Text],
    functionalDependencyDetermined :: [Text]
  }
  deriving (Data, Eq, Show, Generic, NFData)

data ClassDeclItem
  = ClassItemAnn Annotation ClassDeclItem
  | ClassItemTypeSig [BinderName] Type
  | ClassItemDefaultSig BinderName Type
  | ClassItemFixity FixityAssoc (Maybe IEEntityNamespace) (Maybe Int) [OperatorName]
  | ClassItemDefault ValueDecl
  | ClassItemTypeFamilyDecl TypeFamilyDecl
  | ClassItemDataFamilyDecl DataFamilyDecl
  | ClassItemDefaultTypeInst TypeFamilyInst
  | -- pragma inside class body
    ClassItemPragma Pragma
  deriving (Data, Eq, Show, Generic, NFData)

getClassDeclItemSourceSpan :: ClassDeclItem -> SourceSpan
getClassDeclItemSourceSpan classDeclItem =
  case classDeclItem of
    ClassItemAnn ann sub
      | Just srcSpan <- fromAnnotation @SourceSpan ann -> srcSpan
      | otherwise -> getClassDeclItemSourceSpan sub
    _ -> NoSourceSpan

peelClassDeclItemAnn :: ClassDeclItem -> ClassDeclItem
peelClassDeclItemAnn (ClassItemAnn _ inner) = peelClassDeclItemAnn inner
peelClassDeclItemAnn item = item

data InstanceDecl = InstanceDecl
  { instanceDeclOverlapPragma :: Maybe InstanceOverlapPragma,
    instanceDeclWarning :: Maybe WarningText,
    instanceDeclForall :: [TyVarBinder],
    instanceDeclContext :: [Type],
    instanceDeclParenthesizedHead :: Bool,
    instanceDeclHeadForm :: TypeHeadForm,
    instanceDeclClassName :: Text,
    instanceDeclTypes :: [Type],
    instanceDeclItems :: [InstanceDeclItem]
  }
  deriving (Data, Eq, Show, Generic, NFData)

data InstanceOverlapPragma
  = Overlapping
  | Overlappable
  | Overlaps
  | Incoherent
  deriving (Data, Eq, Ord, Show, Read, Generic, NFData)

data InstanceDeclItem
  = -- | Metadata for the whole instance item (typically a 'SourceSpan' via 'mkAnnotation').
    InstanceItemAnn Annotation InstanceDeclItem
  | InstanceItemBind ValueDecl
  | InstanceItemTypeSig [BinderName] Type
  | InstanceItemFixity FixityAssoc (Maybe IEEntityNamespace) (Maybe Int) [OperatorName]
  | InstanceItemTypeFamilyInst TypeFamilyInst
  | InstanceItemDataFamilyInst DataFamilyInst
  | -- pragma inside instance body (e.g. {-# SPECIALIZE instance ... #-})
    InstanceItemPragma Pragma
  deriving (Data, Eq, Show, Generic, NFData)

peelInstanceDeclItemAnn :: InstanceDeclItem -> InstanceDeclItem
peelInstanceDeclItemAnn (InstanceItemAnn _ inner) = peelInstanceDeclItemAnn inner
peelInstanceDeclItemAnn item = item

getInstanceDeclItemSourceSpan :: InstanceDeclItem -> SourceSpan
getInstanceDeclItemSourceSpan instanceDeclItem =
  case instanceDeclItem of
    InstanceItemAnn ann sub
      | Just srcSpan <- fromAnnotation @SourceSpan ann -> srcSpan
      | otherwise -> getInstanceDeclItemSourceSpan sub
    _ -> NoSourceSpan

data FixityAssoc
  = Infix
  | InfixL
  | InfixR
  deriving (Data, Eq, Show, Generic, NFData)

data ForeignDecl = ForeignDecl
  { foreignDirection :: ForeignDirection,
    foreignCallConv :: CallConv,
    foreignSafety :: Maybe ForeignSafety,
    foreignEntity :: ForeignEntitySpec,
    foreignName :: Text,
    foreignType :: Type
  }
  deriving (Data, Eq, Show, Generic, NFData)

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
  | Interruptible
  deriving (Data, Eq, Show, Generic, NFData)

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

data Expr
  = EAnn Annotation Expr
  | EVar Name
  | ETypeSyntax TypeSyntaxForm Type
  | EInt Integer Text
  | EIntHash Integer Text
  | EIntBase Integer Text
  | EIntBaseHash Integer Text
  | EFloat Double Text
  | EFloatHash Double Text
  | EChar Char Text
  | ECharHash Char Text
  | EString Text Text
  | EStringHash Text Text
  | EOverloadedLabel Text Text
  | EQuasiQuote Text Text
  | EIf Expr Expr Expr
  | EMultiWayIf [GuardedRhs]
  | ELambdaPats [Pattern] Expr
  | ELambdaCase [CaseAlt]
  | EInfix Expr Name Expr
  | ENegate Expr
  | ESectionL Expr Name
  | ESectionR Name Expr
  | ELetDecls [Decl] Expr
  | ECase Expr [CaseAlt]
  | EDo [DoStmt Expr] Bool -- Bool: True = mdo, False = do
  | EListComp Expr [CompStmt]
  | EListCompParallel Expr [[CompStmt]]
  | EArithSeq ArithSeq
  | ERecordCon Text [(Text, Expr)] Bool -- Bool: wildcard present
  | ERecordUpd Expr [(Text, Expr)]
  | ETypeSig Expr Type
  | EParen Expr
  | EList [Expr]
  | ETuple TupleFlavor [Maybe Expr]
  | EUnboxedSum Int Int Expr
  | ETypeApp Expr Type
  | EApp Expr Expr
  | -- Template Haskell quotes
    ETHExpQuote Expr -- [| expr |] or [e| expr |]
  | ETHTypedQuote Expr -- [|| expr ||] or [e|| expr ||]
  | ETHDeclQuote [Decl] -- [d| decls |]
  | ETHTypeQuote Type -- [t| type |]
  | ETHPatQuote Pattern -- [p| pat |]
  | ETHNameQuote Text -- 'name
  | ETHTypeNameQuote Name -- ''Name
  | -- Template Haskell splices
    ETHSplice Expr
  | -- \$expr or $(expr)
    ETHTypedSplice Expr -- \$$expr or $$(expr)
  | -- Arrow notation (Arrows extension)
    EProc Pattern Cmd -- proc pat -> cmd
  deriving (Data, Eq, Show, Generic, NFData)

getExprSourceSpan :: Expr -> SourceSpan
getExprSourceSpan expr =
  case expr of
    EAnn ann sub
      | Just srcSpan <- fromAnnotation @SourceSpan ann -> srcSpan
      | otherwise -> getExprSourceSpan sub
    _ -> NoSourceSpan

-- | Peel nested 'EAnn' layers (e.g. span-only dynamic annotations).
peelExprAnn :: Expr -> Expr
peelExprAnn (EAnn _ x) = peelExprAnn x
peelExprAnn x = x

data CaseAlt = CaseAlt
  { caseAltAnns :: [Annotation],
    caseAltPattern :: Pattern,
    caseAltRhs :: Rhs
  }
  deriving (Data, Eq, Show, Generic, NFData)

data DoStmt body
  = -- | Metadata for the whole do-statement (typically a 'SourceSpan' via 'mkAnnotation').
    DoAnn Annotation (DoStmt body)
  | DoBind Pattern body
  | DoLetDecls [Decl]
  | DoExpr body
  | DoRecStmt [DoStmt body] -- rec { stmts }
  deriving (Data, Eq, Show, Generic, NFData)

peelDoStmtAnn :: DoStmt body -> DoStmt body
peelDoStmtAnn (DoAnn _ inner) = peelDoStmtAnn inner
peelDoStmtAnn s = s

getDoStmtSourceSpan :: DoStmt body -> SourceSpan
getDoStmtSourceSpan doStmt =
  case doStmt of
    DoAnn ann sub
      | Just srcSpan <- fromAnnotation @SourceSpan ann -> srcSpan
      | otherwise -> getDoStmtSourceSpan sub
    _ -> NoSourceSpan

-- | Arrow command type (used inside 'proc' expressions).
-- Commands mirror expressions but live in a separate namespace so the
-- pretty-printer knows not to parenthesise @do { … }@ as an infix LHS.
data Cmd
  = -- | Metadata for the whole command (typically a 'SourceSpan' via 'mkAnnotation').
    CmdAnn Annotation Cmd
  | -- | @exp -\< exp@ or @exp -\<\< exp@
    CmdArrApp Expr ArrAppType Expr
  | -- | Command-level infix: @cmd1 op cmd2@
    CmdInfix Cmd Text Cmd
  | -- | @do { cstmts }@
    CmdDo [DoStmt Cmd]
  | -- | @if exp then cmd else cmd@
    CmdIf Expr Cmd Cmd
  | -- | @case exp of { calts }@
    CmdCase Expr [CmdCaseAlt]
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
data ArrAppType = HsFirstOrderApp | HsHigherOrderApp
  deriving (Data, Eq, Show, Generic, NFData)

getCmdSourceSpan :: Cmd -> SourceSpan
getCmdSourceSpan cmd =
  case cmd of
    CmdAnn ann sub
      | Just srcSpan <- fromAnnotation @SourceSpan ann -> srcSpan
      | otherwise -> getCmdSourceSpan sub
    _ -> NoSourceSpan

-- | Case alternative with a command body (used in arrow @case@ commands).
data CmdCaseAlt = CmdCaseAlt
  { cmdCaseAltAnns :: [Annotation],
    cmdCaseAltPat :: Pattern,
    cmdCaseAltBody :: Cmd
  }
  deriving (Data, Eq, Show, Generic, NFData)

data CompStmt
  = -- | Metadata for the whole comprehension statement (typically a 'SourceSpan' via 'mkAnnotation').
    CompAnn Annotation CompStmt
  | CompGen Pattern Expr
  | CompGuard Expr
  | CompLetDecls [Decl]
  deriving (Data, Eq, Show, Generic, NFData)

peelCompStmtAnn :: CompStmt -> CompStmt
peelCompStmtAnn (CompAnn _ inner) = peelCompStmtAnn inner
peelCompStmtAnn s = s

getCompStmtSourceSpan :: CompStmt -> SourceSpan
getCompStmtSourceSpan compStmt =
  case compStmt of
    CompAnn ann sub
      | Just srcSpan <- fromAnnotation @SourceSpan ann -> srcSpan
      | otherwise -> getCompStmtSourceSpan sub
    _ -> NoSourceSpan

data ArithSeq
  = -- | Metadata for the whole arithmetic sequence (typically a 'SourceSpan' via 'mkAnnotation').
    ArithSeqAnn Annotation ArithSeq
  | ArithSeqFrom Expr
  | ArithSeqFromThen Expr Expr
  | ArithSeqFromTo Expr Expr
  | ArithSeqFromThenTo Expr Expr Expr
  deriving (Data, Eq, Show, Generic, NFData)

peelArithSeqAnn :: ArithSeq -> ArithSeq
peelArithSeqAnn (ArithSeqAnn _ inner) = peelArithSeqAnn inner
peelArithSeqAnn s = s

getArithSeqSourceSpan :: ArithSeq -> SourceSpan
getArithSeqSourceSpan arithSeq =
  case arithSeq of
    ArithSeqAnn ann sub
      | Just srcSpan <- fromAnnotation @SourceSpan ann -> srcSpan
      | otherwise -> getArithSeqSourceSpan sub
    _ -> NoSourceSpan
