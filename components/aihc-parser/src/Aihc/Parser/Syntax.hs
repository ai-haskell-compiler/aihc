{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
--
-- Module      : Aihc.Parser.Syntax
-- Description : Abstract Syntax Tree
-- License     : Unlicense
--
-- Abstract Syntax Tree (AST) covering Haskell2010 plus all language extensions.
module Aihc.Parser.Syntax
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
    GadtBody (..),
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
    ModuleHead (..),
    WarningText (..),
    NewtypeDecl (..),
    OperatorName,
    Pattern (..),
    Rhs (..),
    HasSourceSpan (..),
    SourceSpan (..),
    StandaloneDerivingDecl (..),
    Type (..),
    TypeLiteral (..),
    TypePromotion (..),
    TyVarBinder (..),
    TypeSynDecl (..),
    ValueDecl (..),
    declValueBinderNames,
    allKnownExtensions,
    extensionName,
    extensionSettingName,
    gadtBodyResultType,
    mergeSourceSpans,
    noSourceSpan,
    parseExtensionName,
    parseExtensionSettingName,
    sourceSpanEnd,
    valueDeclBinderName,
    moduleName,
    moduleWarningText,
    moduleExports,
  )
where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Data.Data (Data)
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
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic, NFData)

data ExtensionSetting
  = EnableExtension Extension
  | DisableExtension Extension
  deriving (Eq, Ord, Show, Read, Generic, NFData)

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
  deriving (Data, Eq, Ord, Show, Generic, NFData)

noSourceSpan :: SourceSpan
noSourceSpan = NoSourceSpan

class HasSourceSpan a where
  getSourceSpan :: a -> SourceSpan

mergeSourceSpans :: SourceSpan -> SourceSpan -> SourceSpan
mergeSourceSpans left right =
  case (left, right) of
    (SourceSpan l1 c1 _ _, SourceSpan _ _ l2 c2) -> SourceSpan l1 c1 l2 c2
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
  deriving (Eq, Show, Generic, NFData)

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

data ExportSpec
  = ExportModule SourceSpan Text
  | ExportVar SourceSpan (Maybe Text) Text
  | ExportAbs SourceSpan (Maybe Text) Text
  | ExportAll SourceSpan (Maybe Text) Text
  | ExportWith SourceSpan (Maybe Text) Text [Text]
  deriving (Eq, Show, Generic, NFData)

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
  = ImportItemVar SourceSpan (Maybe Text) Text
  | ImportItemAbs SourceSpan (Maybe Text) Text
  | ImportItemAll SourceSpan (Maybe Text) Text
  | ImportItemWith SourceSpan (Maybe Text) Text [Text]
  deriving (Eq, Show, Generic, NFData)

data Decl
  = DeclValue SourceSpan ValueDecl
  | DeclTypeSig SourceSpan [BinderName] Type
  | DeclStandaloneKindSig SourceSpan BinderName Type
  | DeclFixity SourceSpan FixityAssoc (Maybe Int) [OperatorName]
  | DeclTypeSyn SourceSpan TypeSynDecl
  | DeclData SourceSpan DataDecl
  | DeclNewtype SourceSpan NewtypeDecl
  | DeclClass SourceSpan ClassDecl
  | DeclInstance SourceSpan InstanceDecl
  | DeclStandaloneDeriving SourceSpan StandaloneDerivingDecl
  | DeclDefault SourceSpan [Type]
  | DeclForeign SourceSpan ForeignDecl
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan Decl where
  getSourceSpan decl =
    case decl of
      DeclValue span' _ -> span'
      DeclTypeSig span' _ _ -> span'
      DeclStandaloneKindSig span' _ _ -> span'
      DeclFixity span' _ _ _ -> span'
      DeclTypeSyn span' _ -> span'
      DeclData span' _ -> span'
      DeclNewtype span' _ -> span'
      DeclClass span' _ -> span'
      DeclInstance span' _ -> span'
      DeclStandaloneDeriving span' _ -> span'
      DeclDefault span' _ -> span'
      DeclForeign span' _ -> span'

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
    matchPats :: [Pattern],
    matchRhs :: Rhs
  }
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan Match where
  getSourceSpan = matchSpan

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
  | LitIntBase SourceSpan Integer Text
  | LitFloat SourceSpan Double Text
  | LitChar SourceSpan Char Text
  | LitString SourceSpan Text Text
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan Literal where
  getSourceSpan literal =
    case literal of
      LitInt span' _ _ -> span'
      LitIntBase span' _ _ -> span'
      LitFloat span' _ _ -> span'
      LitChar span' _ _ -> span'
      LitString span' _ _ -> span'

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
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan Pattern where
  getSourceSpan pat =
    case pat of
      PVar span' _ -> span'
      PWildcard span' -> span'
      PLit span' _ -> span'
      PQuasiQuote span' _ _ -> span'
      PTuple span' _ -> span'
      PList span' _ -> span'
      PCon span' _ _ -> span'
      PInfix span' _ _ _ -> span'
      PView span' _ _ -> span'
      PAs span' _ _ -> span'
      PStrict span' _ -> span'
      PIrrefutable span' _ -> span'
      PNegLit span' _ -> span'
      PParen span' _ -> span'
      PRecord span' _ _ -> span'

data Type
  = TVar SourceSpan Text
  | TCon SourceSpan Text TypePromotion
  | TTypeLit SourceSpan TypeLiteral
  | TStar SourceSpan
  | TQuasiQuote SourceSpan Text Text
  | TForall SourceSpan [Text] Type
  | TApp SourceSpan Type Type
  | TFun SourceSpan Type Type
  | TTuple SourceSpan TypePromotion [Type]
  | TList SourceSpan TypePromotion Type
  | TParen SourceSpan Type
  | TContext SourceSpan [Constraint] Type
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan Type where
  getSourceSpan ty =
    case ty of
      TVar span' _ -> span'
      TCon span' _ _ -> span'
      TTypeLit span' _ -> span'
      TStar span' -> span'
      TQuasiQuote span' _ _ -> span'
      TForall span' _ _ -> span'
      TApp span' _ _ -> span'
      TFun span' _ _ -> span'
      TTuple span' _ _ -> span'
      TList span' _ _ -> span'
      TParen span' _ -> span'
      TContext span' _ _ -> span'

data TypeLiteral
  = TypeLitInteger Integer Text
  | TypeLitSymbol Text Text
  | TypeLitChar Char Text
  deriving (Data, Eq, Show, Generic, NFData)

data TypePromotion
  = Unpromoted
  | Promoted
  deriving (Data, Eq, Show, Generic, NFData)

data Constraint = Constraint
  { constraintSpan :: SourceSpan,
    constraintClass :: Text,
    constraintArgs :: [Type],
    constraintParen :: Bool
  }
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan Constraint where
  getSourceSpan = constraintSpan

data TyVarBinder = TyVarBinder
  { tyVarBinderSpan :: SourceSpan,
    tyVarBinderName :: Text,
    tyVarBinderKind :: Maybe Type
  }
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan TyVarBinder where
  getSourceSpan = tyVarBinderSpan

data TypeSynDecl = TypeSynDecl
  { typeSynSpan :: SourceSpan,
    typeSynName :: Text,
    typeSynParams :: [TyVarBinder],
    typeSynBody :: Type
  }
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan TypeSynDecl where
  getSourceSpan = typeSynSpan

data DataDecl = DataDecl
  { dataDeclSpan :: SourceSpan,
    dataDeclContext :: [Constraint],
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
    newtypeDeclContext :: [Constraint],
    newtypeDeclName :: Text,
    newtypeDeclParams :: [TyVarBinder],
    newtypeDeclConstructor :: Maybe DataConDecl,
    newtypeDeclDeriving :: [DerivingClause]
  }
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan NewtypeDecl where
  getSourceSpan = newtypeDeclSpan

data DataConDecl
  = PrefixCon SourceSpan [Text] [Constraint] Text [BangType]
  | InfixCon SourceSpan [Text] [Constraint] BangType Text BangType
  | RecordCon SourceSpan [Text] [Constraint] Text [FieldDecl]
  | -- | GADT-style constructor: @Con :: forall a. Ctx => Type@
    -- The list of names supports multiple constructors: @T1, T2 :: Type@
    GadtCon SourceSpan [TyVarBinder] [Constraint] [Text] GadtBody
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
    bangStrict :: Bool,
    bangType :: Type
  }
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan BangType where
  getSourceSpan = bangSpan

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
    derivingClasses :: [Text]
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
    standaloneDerivingContext :: [Constraint],
    standaloneDerivingClassName :: Text,
    standaloneDerivingTypes :: [Type]
  }
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan StandaloneDerivingDecl where
  getSourceSpan = standaloneDerivingSpan

data ClassDecl = ClassDecl
  { classDeclSpan :: SourceSpan,
    classDeclContext :: Maybe [Constraint],
    classDeclName :: Text,
    classDeclParams :: [TyVarBinder],
    classDeclItems :: [ClassDeclItem]
  }
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan ClassDecl where
  getSourceSpan = classDeclSpan

data ClassDeclItem
  = ClassItemTypeSig SourceSpan [BinderName] Type
  | ClassItemFixity SourceSpan FixityAssoc (Maybe Int) [OperatorName]
  | ClassItemDefault SourceSpan ValueDecl
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan ClassDeclItem where
  getSourceSpan classDeclItem =
    case classDeclItem of
      ClassItemTypeSig span' _ _ -> span'
      ClassItemFixity span' _ _ _ -> span'
      ClassItemDefault span' _ -> span'

data InstanceDecl = InstanceDecl
  { instanceDeclSpan :: SourceSpan,
    instanceDeclContext :: [Constraint],
    instanceDeclClassName :: Text,
    instanceDeclTypes :: [Type],
    instanceDeclItems :: [InstanceDeclItem]
  }
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan InstanceDecl where
  getSourceSpan = instanceDeclSpan

data InstanceDeclItem
  = InstanceItemBind SourceSpan ValueDecl
  | InstanceItemTypeSig SourceSpan [BinderName] Type
  | InstanceItemFixity SourceSpan FixityAssoc (Maybe Int) [OperatorName]
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan InstanceDeclItem where
  getSourceSpan instanceDeclItem =
    case instanceDeclItem of
      InstanceItemBind span' _ -> span'
      InstanceItemTypeSig span' _ _ -> span'
      InstanceItemFixity span' _ _ _ -> span'

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
  deriving (Data, Eq, Show, Generic, NFData)

data ForeignSafety
  = Safe
  | Unsafe
  deriving (Data, Eq, Show, Generic, NFData)

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
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan Expr where
  getSourceSpan expr =
    case expr of
      EVar span' _ -> span'
      EInt span' _ _ -> span'
      EIntBase span' _ _ -> span'
      EFloat span' _ _ -> span'
      EChar span' _ _ -> span'
      EString span' _ _ -> span'
      EQuasiQuote span' _ _ -> span'
      EIf span' _ _ _ -> span'
      ELambdaPats span' _ _ -> span'
      ELambdaCase span' _ -> span'
      EInfix span' _ _ _ -> span'
      ENegate span' _ -> span'
      ESectionL span' _ _ -> span'
      ESectionR span' _ _ -> span'
      ELetDecls span' _ _ -> span'
      ECase span' _ _ -> span'
      EDo span' _ -> span'
      EListComp span' _ _ -> span'
      EListCompParallel span' _ _ -> span'
      EArithSeq span' _ -> span'
      ERecordCon span' _ _ -> span'
      ERecordUpd span' _ _ -> span'
      ETypeSig span' _ _ -> span'
      EParen span' _ -> span'
      EWhereDecls span' _ _ -> span'
      EList span' _ -> span'
      ETuple span' _ -> span'
      ETupleSection span' _ -> span'
      ETupleCon span' _ -> span'
      ETypeApp span' _ _ -> span'
      EApp span' _ _ -> span'

data CaseAlt = CaseAlt
  { caseAltSpan :: SourceSpan,
    caseAltPattern :: Pattern,
    caseAltRhs :: Rhs
  }
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan CaseAlt where
  getSourceSpan = caseAltSpan

data DoStmt
  = DoBind SourceSpan Pattern Expr
  | DoLet SourceSpan [(Text, Expr)]
  | DoLetDecls SourceSpan [Decl]
  | DoExpr SourceSpan Expr
  deriving (Data, Eq, Show, Generic, NFData)

instance HasSourceSpan DoStmt where
  getSourceSpan doStmt =
    case doStmt of
      DoBind span' _ _ -> span'
      DoLet span' _ -> span'
      DoLetDecls span' _ -> span'
      DoExpr span' _ -> span'

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
