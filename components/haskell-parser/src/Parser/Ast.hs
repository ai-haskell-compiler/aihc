{-# LANGUAGE DeriveDataTypeable #-}

module Parser.Ast
  ( ArithSeq (..),
    BangType (..),
    BinderName,
    CallConv (..),
    CaseAlt (..),
    ClassDecl (..),
    ClassDeclItem (..),
    CompStmt (..),
    Constraint (..),
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
    FieldDecl (..),
    FixityAssoc (..),
    ForeignDecl (..),
    ForeignDirection (..),
    ForeignEntitySpec (..),
    ForeignSafety (..),
    GuardQualifier (..),
    GuardedRhs (..),
    ImportDecl (..),
    ImportLevel (..),
    ImportItem (..),
    ImportSpec (..),
    InstanceDecl (..),
    InstanceDeclItem (..),
    Literal (..),
    Match (..),
    Module (..),
    WarningText (..),
    NewtypeDecl (..),
    OperatorName,
    Pattern (..),
    Rhs (..),
    SourceSpan (..),
    Type (..),
    TyVarBinder (..),
    TypeSynDecl (..),
    ValueDecl (..),
    declValueBinderNames,
    allKnownExtensions,
    extensionName,
    extensionSettingName,
    noSourceSpan,
    parseExtensionName,
    parseExtensionSettingName,
    valueDeclBinderName,
  )
where

import Control.Applicative ((<|>))
import Data.Data (Data)
import Data.Text (Text)
import qualified Data.Text as T
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
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data ExtensionSetting
  = EnableExtension Extension
  | DisableExtension Extension
  deriving (Eq, Ord, Show, Read)

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

data SourceSpan
  = NoSourceSpan
  | SourceSpan
      { sourceSpanStartLine :: !Int,
        sourceSpanStartCol :: !Int,
        sourceSpanEndLine :: !Int,
        sourceSpanEndCol :: !Int
      }
  deriving (Data, Eq, Ord, Show)

noSourceSpan :: SourceSpan
noSourceSpan = NoSourceSpan

type BinderName = Text

type OperatorName = Text

data WarningText
  = DeprText SourceSpan Text
  | WarnText SourceSpan Text
  deriving (Eq, Show)

data Module = Module
  { moduleSpan :: SourceSpan,
    moduleName :: Maybe Text,
    moduleLanguagePragmas :: [ExtensionSetting],
    moduleWarningText :: Maybe WarningText,
    moduleExports :: Maybe [ExportSpec],
    moduleImports :: [ImportDecl],
    moduleDecls :: [Decl]
  }
  deriving (Eq, Show)

data ExportSpec
  = ExportModule SourceSpan Text
  | ExportVar SourceSpan (Maybe Text) Text
  | ExportAbs SourceSpan (Maybe Text) Text
  | ExportAll SourceSpan (Maybe Text) Text
  | ExportWith SourceSpan (Maybe Text) Text [Text]
  deriving (Eq, Show)

data ImportDecl = ImportDecl
  { importDeclSpan :: SourceSpan,
    importDeclLevel :: Maybe ImportLevel,
    importDeclPackage :: Maybe Text,
    importDeclQualified :: Bool,
    importDeclQualifiedPost :: Bool,
    importDeclModule :: Text,
    importDeclAs :: Maybe Text,
    importDeclSpec :: Maybe ImportSpec
  }
  deriving (Eq, Show)

data ImportLevel
  = ImportLevelQuote
  | ImportLevelSplice
  deriving (Eq, Show)

data ImportSpec = ImportSpec
  { importSpecSpan :: SourceSpan,
    importSpecHiding :: Bool,
    importSpecItems :: [ImportItem]
  }
  deriving (Eq, Show)

data ImportItem
  = ImportItemVar SourceSpan (Maybe Text) Text
  | ImportItemAbs SourceSpan (Maybe Text) Text
  | ImportItemAll SourceSpan (Maybe Text) Text
  | ImportItemWith SourceSpan (Maybe Text) Text [Text]
  deriving (Eq, Show)

data Decl
  = DeclValue SourceSpan ValueDecl
  | DeclTypeSig SourceSpan [BinderName] Type
  | DeclFixity SourceSpan FixityAssoc (Maybe Int) [OperatorName]
  | DeclTypeSyn SourceSpan TypeSynDecl
  | DeclData SourceSpan DataDecl
  | DeclNewtype SourceSpan NewtypeDecl
  | DeclClass SourceSpan ClassDecl
  | DeclInstance SourceSpan InstanceDecl
  | DeclDefault SourceSpan [Type]
  | DeclForeign SourceSpan ForeignDecl
  deriving (Data, Eq, Show)

data ValueDecl
  = FunctionBind SourceSpan BinderName [Match]
  | PatternBind SourceSpan Pattern Rhs
  deriving (Data, Eq, Show)

data Match = Match
  { matchSpan :: SourceSpan,
    matchPats :: [Pattern],
    matchRhs :: Rhs
  }
  deriving (Data, Eq, Show)

data Rhs
  = UnguardedRhs SourceSpan Expr
  | GuardedRhss SourceSpan [GuardedRhs]
  deriving (Data, Eq, Show)

data GuardedRhs = GuardedRhs
  { guardedRhsSpan :: SourceSpan,
    guardedRhsGuards :: [GuardQualifier],
    guardedRhsBody :: Expr
  }
  deriving (Data, Eq, Show)

data GuardQualifier
  = GuardExpr SourceSpan Expr
  | GuardPat SourceSpan Pattern Expr
  | GuardLet SourceSpan [Decl]
  deriving (Data, Eq, Show)

data Literal
  = LitInt SourceSpan Integer Text
  | LitIntBase SourceSpan Integer Text
  | LitFloat SourceSpan Double Text
  | LitChar SourceSpan Char Text
  | LitString SourceSpan Text Text
  deriving (Data, Eq, Show)

data Pattern
  = PVar SourceSpan Text
  | PWildcard SourceSpan
  | PLit SourceSpan Literal
  | PQuasiQuote SourceSpan Text Text
  | PTuple SourceSpan [Pattern]
  | PList SourceSpan [Pattern]
  | PCon SourceSpan Text [Pattern]
  | PInfix SourceSpan Pattern Text Pattern
  | PView SourceSpan Expr Pattern
  | PAs SourceSpan Text Pattern
  | PStrict SourceSpan Pattern
  | PIrrefutable SourceSpan Pattern
  | PNegLit SourceSpan Literal
  | PParen SourceSpan Pattern
  | PRecord SourceSpan Text [(Text, Pattern)]
  deriving (Data, Eq, Show)

data Type
  = TVar SourceSpan Text
  | TCon SourceSpan Text
  | TStar SourceSpan
  | TQuasiQuote SourceSpan Text Text
  | TForall SourceSpan [Text] Type
  | TApp SourceSpan Type Type
  | TFun SourceSpan Type Type
  | TTuple SourceSpan [Type]
  | TList SourceSpan Type
  | TParen SourceSpan Type
  | TContext SourceSpan [Constraint] Type
  deriving (Data, Eq, Show)

data Constraint = Constraint
  { constraintSpan :: SourceSpan,
    constraintClass :: Text,
    constraintArgs :: [Type],
    constraintParen :: Bool
  }
  deriving (Data, Eq, Show)

data TyVarBinder = TyVarBinder
  { tyVarBinderSpan :: SourceSpan,
    tyVarBinderName :: Text,
    tyVarBinderKind :: Maybe Type
  }
  deriving (Data, Eq, Show)

data TypeSynDecl = TypeSynDecl
  { typeSynSpan :: SourceSpan,
    typeSynName :: Text,
    typeSynParams :: [TyVarBinder],
    typeSynBody :: Type
  }
  deriving (Data, Eq, Show)

data DataDecl = DataDecl
  { dataDeclSpan :: SourceSpan,
    dataDeclContext :: [Constraint],
    dataDeclName :: Text,
    dataDeclParams :: [TyVarBinder],
    dataDeclConstructors :: [DataConDecl],
    dataDeclDeriving :: [DerivingClause]
  }
  deriving (Data, Eq, Show)

data NewtypeDecl = NewtypeDecl
  { newtypeDeclSpan :: SourceSpan,
    newtypeDeclContext :: [Constraint],
    newtypeDeclName :: Text,
    newtypeDeclParams :: [TyVarBinder],
    newtypeDeclConstructor :: Maybe DataConDecl,
    newtypeDeclDeriving :: [DerivingClause]
  }
  deriving (Data, Eq, Show)

data DataConDecl
  = PrefixCon SourceSpan [Text] [Constraint] Text [BangType]
  | InfixCon SourceSpan [Text] [Constraint] BangType Text BangType
  | RecordCon SourceSpan [Text] [Constraint] Text [FieldDecl]
  deriving (Data, Eq, Show)

data BangType = BangType
  { bangSpan :: SourceSpan,
    bangStrict :: Bool,
    bangType :: Type
  }
  deriving (Data, Eq, Show)

data FieldDecl = FieldDecl
  { fieldSpan :: SourceSpan,
    fieldNames :: [Text],
    fieldType :: BangType
  }
  deriving (Data, Eq, Show)

data DerivingClause = DerivingClause
  { derivingStrategy :: Maybe DerivingStrategy,
    derivingClasses :: [Text]
  }
  deriving (Data, Eq, Show)

data DerivingStrategy
  = DerivingStock
  | DerivingNewtype
  | DerivingAnyclass
  deriving (Data, Eq, Show)

data ClassDecl = ClassDecl
  { classDeclSpan :: SourceSpan,
    classDeclContext :: [Constraint],
    classDeclName :: Text,
    classDeclParams :: [TyVarBinder],
    classDeclItems :: [ClassDeclItem]
  }
  deriving (Data, Eq, Show)

data ClassDeclItem
  = ClassItemTypeSig SourceSpan [BinderName] Type
  | ClassItemFixity SourceSpan FixityAssoc (Maybe Int) [OperatorName]
  | ClassItemDefault SourceSpan ValueDecl
  deriving (Data, Eq, Show)

data InstanceDecl = InstanceDecl
  { instanceDeclSpan :: SourceSpan,
    instanceDeclContext :: [Constraint],
    instanceDeclClassName :: Text,
    instanceDeclTypes :: [Type],
    instanceDeclItems :: [InstanceDeclItem]
  }
  deriving (Data, Eq, Show)

data InstanceDeclItem
  = InstanceItemBind SourceSpan ValueDecl
  | InstanceItemTypeSig SourceSpan [BinderName] Type
  | InstanceItemFixity SourceSpan FixityAssoc (Maybe Int) [OperatorName]
  deriving (Data, Eq, Show)

data FixityAssoc
  = Infix
  | InfixL
  | InfixR
  deriving (Data, Eq, Show)

data ForeignDecl = ForeignDecl
  { foreignDeclSpan :: SourceSpan,
    foreignDirection :: ForeignDirection,
    foreignCallConv :: CallConv,
    foreignSafety :: Maybe ForeignSafety,
    foreignEntity :: ForeignEntitySpec,
    foreignName :: Text,
    foreignType :: Type
  }
  deriving (Data, Eq, Show)

data ForeignEntitySpec
  = ForeignEntityDynamic
  | ForeignEntityWrapper
  | ForeignEntityStatic (Maybe Text)
  | ForeignEntityAddress (Maybe Text)
  | ForeignEntityNamed Text
  | ForeignEntityOmitted
  deriving (Data, Eq, Show)

data ForeignDirection
  = ForeignImport
  | ForeignExport
  deriving (Data, Eq, Show)

data CallConv
  = CCall
  | StdCall
  deriving (Data, Eq, Show)

data ForeignSafety
  = Safe
  | Unsafe
  deriving (Data, Eq, Show)

data Expr
  = EVar SourceSpan Text
  | EInt SourceSpan Integer Text
  | EIntBase SourceSpan Integer Text
  | EFloat SourceSpan Double Text
  | EChar SourceSpan Char Text
  | EString SourceSpan Text Text
  | EQuasiQuote SourceSpan Text Text
  | EIf SourceSpan Expr Expr Expr
  | ELambdaPats SourceSpan [Pattern] Expr
  | ELambdaCase SourceSpan [CaseAlt]
  | EInfix SourceSpan Expr Text Expr
  | ENegate SourceSpan Expr
  | ESectionL SourceSpan Expr Text
  | ESectionR SourceSpan Text Expr
  | ELetDecls SourceSpan [Decl] Expr
  | ECase SourceSpan Expr [CaseAlt]
  | EDo SourceSpan [DoStmt]
  | EListComp SourceSpan Expr [CompStmt]
  | EListCompParallel SourceSpan Expr [[CompStmt]]
  | EArithSeq SourceSpan ArithSeq
  | ERecordCon SourceSpan Text [(Text, Expr)]
  | ERecordUpd SourceSpan Expr [(Text, Expr)]
  | ETypeSig SourceSpan Expr Type
  | EParen SourceSpan Expr
  | EWhereDecls SourceSpan Expr [Decl]
  | EList SourceSpan [Expr]
  | ETuple SourceSpan [Expr]
  | ETupleSection SourceSpan [Maybe Expr]
  | ETupleCon SourceSpan Int
  | ETypeApp SourceSpan Expr Type
  | EApp SourceSpan Expr Expr
  deriving (Data, Eq, Show)

data CaseAlt = CaseAlt
  { caseAltSpan :: SourceSpan,
    caseAltPattern :: Pattern,
    caseAltRhs :: Rhs
  }
  deriving (Data, Eq, Show)

data DoStmt
  = DoBind SourceSpan Pattern Expr
  | DoLet SourceSpan [(Text, Expr)]
  | DoLetDecls SourceSpan [Decl]
  | DoExpr SourceSpan Expr
  deriving (Data, Eq, Show)

data CompStmt
  = CompGen SourceSpan Pattern Expr
  | CompGuard SourceSpan Expr
  | CompLet SourceSpan [(Text, Expr)]
  | CompLetDecls SourceSpan [Decl]
  deriving (Data, Eq, Show)

data ArithSeq
  = ArithSeqFrom SourceSpan Expr
  | ArithSeqFromThen SourceSpan Expr Expr
  | ArithSeqFromTo SourceSpan Expr Expr
  | ArithSeqFromThenTo SourceSpan Expr Expr Expr
  deriving (Data, Eq, Show)

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
