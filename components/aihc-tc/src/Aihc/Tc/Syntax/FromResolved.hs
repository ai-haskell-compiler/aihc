{-# LANGUAGE TypeFamilies #-}

-- | Copy a resolved syntax tree into the tree that the type checker works
-- on. The copy has no type checker information.
--
-- Each instance has one case for each constructor of the data declaration.
-- When a constructor changes in one tree, change its case here too.
module Aihc.Tc.Syntax.FromResolved
  ( FromResolved (..),
  )
where

import Aihc.Parser.Syntax qualified as P
import Aihc.Resolve.Syntax qualified as R
import Aihc.Tc.Syntax.Pending
import Data.Text (Text)

class FromResolved a where
  type ToPending a
  fromResolved :: a -> ToPending a

instance (FromResolved a) => FromResolved [a] where
  type ToPending [a] = [ToPending a]
  fromResolved = fmap fromResolved

instance (FromResolved a) => FromResolved (Maybe a) where
  type ToPending (Maybe a) = Maybe (ToPending a)
  fromResolved = fmap fromResolved

instance (FromResolved a, FromResolved b) => FromResolved (a, b) where
  type ToPending (a, b) = (ToPending a, ToPending b)
  fromResolved (left, right) = (fromResolved left, fromResolved right)

instance (FromResolved a, FromResolved b) => FromResolved (Either a b) where
  type ToPending (Either a b) = Either (ToPending a) (ToPending b)
  fromResolved = either (Left . fromResolved) (Right . fromResolved)

instance FromResolved Annotation where
  type ToPending Annotation = Annotation
  fromResolved = id

instance FromResolved ArrAppType where
  type ToPending ArrAppType = ArrAppType
  fromResolved = id

instance FromResolved Bool where
  type ToPending Bool = Bool
  fromResolved = id

instance FromResolved BuiltinCon where
  type ToPending BuiltinCon = BuiltinCon
  fromResolved = id

instance FromResolved CallConv where
  type ToPending CallConv = CallConv
  fromResolved = id

instance FromResolved Char where
  type ToPending Char = Char
  fromResolved = id

instance FromResolved DoFlavor where
  type ToPending DoFlavor = DoFlavor
  fromResolved = id

instance FromResolved ExtensionSetting where
  type ToPending ExtensionSetting = ExtensionSetting
  fromResolved = id

instance FromResolved FixityAssoc where
  type ToPending FixityAssoc = FixityAssoc
  fromResolved = id

instance FromResolved FloatType where
  type ToPending FloatType = FloatType
  fromResolved = id

instance FromResolved ForallVis where
  type ToPending ForallVis = ForallVis
  fromResolved = id

instance FromResolved ForeignDirection where
  type ToPending ForeignDirection = ForeignDirection
  fromResolved = id

instance FromResolved ForeignEntitySpec where
  type ToPending ForeignEntitySpec = ForeignEntitySpec
  fromResolved = id

instance FromResolved ForeignSafety where
  type ToPending ForeignSafety = ForeignSafety
  fromResolved = id

instance FromResolved FunctionalDependency where
  type ToPending FunctionalDependency = FunctionalDependency
  fromResolved = id

instance FromResolved IEBundledNamespace where
  type ToPending IEBundledNamespace = IEBundledNamespace
  fromResolved = id

instance FromResolved IEEntityNamespace where
  type ToPending IEEntityNamespace = IEEntityNamespace
  fromResolved = id

instance FromResolved ImportLevel where
  type ToPending ImportLevel = ImportLevel
  fromResolved = id

instance FromResolved Int where
  type ToPending Int = Int
  fromResolved = id

instance FromResolved Integer where
  type ToPending Integer = Integer
  fromResolved = id

instance FromResolved MatchHeadForm where
  type ToPending MatchHeadForm = MatchHeadForm
  fromResolved = id

instance FromResolved NameType where
  type ToPending NameType = NameType
  fromResolved = id

instance FromResolved NumericType where
  type ToPending NumericType = NumericType
  fromResolved = id

instance FromResolved PatSynArgs where
  type ToPending PatSynArgs = PatSynArgs
  fromResolved = id

instance FromResolved Pragma where
  type ToPending Pragma = Pragma
  fromResolved = id

instance FromResolved Rational where
  type ToPending Rational = Rational
  fromResolved = id

instance FromResolved RecKnotVariables where
  type ToPending RecKnotVariables = RecKnotVariables
  fromResolved = id

instance FromResolved ResolutionResult where
  type ToPending ResolutionResult = ResolutionResult
  fromResolved = id

instance FromResolved ResolveError where
  type ToPending ResolveError = ResolveError
  fromResolved = id

instance FromResolved Role where
  type ToPending Role = Role
  fromResolved = id

instance FromResolved RuleActivation where
  type ToPending RuleActivation = RuleActivation
  fromResolved = id

instance FromResolved Text where
  type ToPending Text = Text
  fromResolved = id

instance FromResolved TupleFlavor where
  type ToPending TupleFlavor = TupleFlavor
  fromResolved = id

instance FromResolved TyVarBSpecificity where
  type ToPending TyVarBSpecificity = TyVarBSpecificity
  fromResolved = id

instance FromResolved TyVarBVisibility where
  type ToPending TyVarBVisibility = TyVarBVisibility
  fromResolved = id

instance FromResolved TypeFamilyInjectivity where
  type ToPending TypeFamilyInjectivity = TypeFamilyInjectivity
  fromResolved = id

instance FromResolved TypeHeadForm where
  type ToPending TypeHeadForm = TypeHeadForm
  fromResolved = id

instance FromResolved TypeLiteral where
  type ToPending TypeLiteral = TypeLiteral
  fromResolved = id

instance FromResolved TypePromotion where
  type ToPending TypePromotion = TypePromotion
  fromResolved = id

instance FromResolved TypeSyntaxForm where
  type ToPending TypeSyntaxForm = TypeSyntaxForm
  fromResolved = id

instance FromResolved R.Name where
  type ToPending R.Name = Name
  fromResolved node =
    case node of
      R.Name x1 x2 x3 x4 x5 -> Name (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4) (fromResolved x5) Nothing Nothing

instance FromResolved R.UnqualifiedName where
  type ToPending R.UnqualifiedName = UnqualifiedName
  fromResolved node =
    case node of
      R.UnqualifiedName x1 x2 x3 x4 -> UnqualifiedName (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4) Nothing

instance FromResolved R.Module where
  type ToPending R.Module = Module
  fromResolved node =
    case node of
      R.Module x1 x2 x3 x4 x5 -> Module (fromResolved x1) Nothing (fromResolved x2) (fromResolved x3) (fromResolved x4) (fromResolved x5)

instance FromResolved R.ModuleHead where
  type ToPending R.ModuleHead = ModuleHead
  fromResolved node =
    case node of
      R.ModuleHead x1 x2 x3 x4 -> ModuleHead (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4)

instance FromResolved R.IEBundledMember where
  type ToPending R.IEBundledMember = IEBundledMember
  fromResolved node =
    case node of
      R.IEBundledMember x1 x2 -> IEBundledMember (fromResolved x1) (fromResolved x2)

instance FromResolved R.ExportSpec where
  type ToPending R.ExportSpec = ExportSpec
  fromResolved node =
    case node of
      R.ExportModule x1 x2 -> ExportModule (fromResolved x1) (fromResolved x2)
      R.ExportVar x1 x2 x3 -> ExportVar (fromResolved x1) (fromResolved x2) (fromResolved x3)
      R.ExportAbs x1 x2 x3 -> ExportAbs (fromResolved x1) (fromResolved x2) (fromResolved x3)
      R.ExportAll x1 x2 x3 -> ExportAll (fromResolved x1) (fromResolved x2) (fromResolved x3)
      R.ExportWith x1 x2 x3 x4 -> ExportWith (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4)
      R.ExportWithAll x1 x2 x3 x4 x5 -> ExportWithAll (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4) (fromResolved x5)
      R.ExportAnn x1 x2 -> ExportAnn (fromResolved x1) (fromResolved x2)

instance FromResolved R.ImportDecl where
  type ToPending R.ImportDecl = ImportDecl
  fromResolved node =
    case node of
      R.ImportDecl x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 -> ImportDecl (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4) (fromResolved x5) (fromResolved x6) (fromResolved x7) (fromResolved x8) (fromResolved x9) (fromResolved x10) (fromResolved x11)

instance FromResolved R.ImportSpec where
  type ToPending R.ImportSpec = ImportSpec
  fromResolved node =
    case node of
      R.ImportSpec x1 x2 x3 -> ImportSpec (fromResolved x1) (fromResolved x2) (fromResolved x3)

instance FromResolved R.ImportItem where
  type ToPending R.ImportItem = ImportItem
  fromResolved node =
    case node of
      R.ImportItemVar x1 x2 -> ImportItemVar (fromResolved x1) (fromResolved x2)
      R.ImportItemAbs x1 x2 -> ImportItemAbs (fromResolved x1) (fromResolved x2)
      R.ImportItemAll x1 x2 -> ImportItemAll (fromResolved x1) (fromResolved x2)
      R.ImportItemWith x1 x2 x3 -> ImportItemWith (fromResolved x1) (fromResolved x2) (fromResolved x3)
      R.ImportItemAllWith x1 x2 x3 x4 -> ImportItemAllWith (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4)
      R.ImportAnn x1 x2 -> ImportAnn (fromResolved x1) (fromResolved x2)
      R.ImportItemError x1 x2 -> ImportItemError (fromResolved x1) (fromResolved x2)

instance FromResolved R.Decl where
  type ToPending R.Decl = Decl
  fromResolved node =
    case node of
      R.DeclAnn x1 x2 -> DeclAnn (fromResolved x1) (fromResolved x2)
      R.DeclError x1 x2 -> DeclError (fromResolved x1) (fromResolved x2)
      R.DeclValue x1 -> DeclValue (fromResolved x1)
      R.DeclImplicitParam x1 x2 x3 -> DeclImplicitParam (fromResolved x1) (fromResolved x2) (fromResolved x3)
      R.DeclTypeSig x1 x2 -> DeclTypeSig (fromResolved x1) (fromResolved x2)
      R.DeclPatSyn x1 -> DeclPatSyn (fromResolved x1)
      R.DeclPatSynSig x1 x2 -> DeclPatSynSig (fromResolved x1) (fromResolved x2)
      R.DeclStandaloneKindSig x1 x2 -> DeclStandaloneKindSig (fromResolved x1) (fromResolved x2)
      R.DeclFixity x1 x2 x3 x4 -> DeclFixity (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4)
      R.DeclRoleAnnotation x1 -> DeclRoleAnnotation (fromResolved x1)
      R.DeclTypeSyn x1 -> DeclTypeSyn (fromResolved x1)
      R.DeclTypeData x1 -> DeclTypeData (fromResolved x1)
      R.DeclData x1 -> DeclData (fromResolved x1)
      R.DeclNewtype x1 -> DeclNewtype (fromResolved x1)
      R.DeclClass x1 -> DeclClass (fromResolved x1)
      R.DeclInstance x1 -> DeclInstance (fromResolved x1)
      R.DeclStandaloneDeriving x1 -> DeclStandaloneDeriving (fromResolved x1)
      R.DeclDefault x1 -> DeclDefault (fromResolved x1)
      R.DeclSplice x1 -> DeclSplice (fromResolved x1)
      R.DeclForeign x1 -> DeclForeign (fromResolved x1)
      R.DeclTypeFamilyDecl x1 -> DeclTypeFamilyDecl (fromResolved x1)
      R.DeclDataFamilyDecl x1 -> DeclDataFamilyDecl (fromResolved x1)
      R.DeclTypeFamilyInst x1 -> DeclTypeFamilyInst (fromResolved x1)
      R.DeclDataFamilyInst x1 -> DeclDataFamilyInst (fromResolved x1)
      R.DeclPragma x1 -> DeclPragma (fromResolved x1)
      R.DeclRules x1 -> DeclRules (fromResolved x1)

instance FromResolved R.RuleDecl where
  type ToPending R.RuleDecl = RuleDecl
  fromResolved node =
    case node of
      R.RuleDecl x1 x2 x3 x4 x5 x6 x7 x8 -> RuleDecl (fromResolved x1) Nothing (fromResolved x2) (fromResolved x3) (fromResolved x4) (fromResolved x5) (fromResolved x6) (fromResolved x7) (fromResolved x8)

instance FromResolved R.RuleBinder where
  type ToPending R.RuleBinder = RuleBinder
  fromResolved node =
    case node of
      R.RuleBinder x1 x2 x3 -> RuleBinder (fromResolved x1) Nothing (fromResolved x2) (fromResolved x3)

instance FromResolved R.MultiplicityTag where
  type ToPending R.MultiplicityTag = MultiplicityTag
  fromResolved node =
    case node of
      R.NoMultiplicityTag -> NoMultiplicityTag
      R.LinearMultiplicityTag -> LinearMultiplicityTag
      R.ExplicitMultiplicityTag x1 -> ExplicitMultiplicityTag (fromResolved x1)

instance FromResolved R.ValueDecl where
  type ToPending R.ValueDecl = ValueDecl
  fromResolved node =
    case node of
      R.FunctionBind x1 x2 -> FunctionBind (fromResolved x1) (fromResolved x2)
      R.PatternBind x1 x2 x3 -> PatternBind (fromResolved x1) (fromResolved x2) (fromResolved x3)

instance FromResolved R.Match where
  type ToPending R.Match = Match
  fromResolved node =
    case node of
      R.Match x1 x2 x3 x4 -> Match (fromResolved x1) Nothing (fromResolved x2) (fromResolved x3) (fromResolved x4)

instance FromResolved R.PatSynDir where
  type ToPending R.PatSynDir = PatSynDir
  fromResolved node =
    case node of
      R.PatSynUnidirectional -> PatSynUnidirectional
      R.PatSynBidirectional -> PatSynBidirectional
      R.PatSynExplicitBidirectional x1 -> PatSynExplicitBidirectional (fromResolved x1)

instance FromResolved R.PatSynDecl where
  type ToPending R.PatSynDecl = PatSynDecl
  fromResolved node =
    case node of
      R.PatSynDecl x1 x2 x3 x4 -> PatSynDecl (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4) Nothing

instance (FromResolved body) => FromResolved (R.Rhs body) where
  type ToPending (R.Rhs body) = Rhs (ToPending body)
  fromResolved node =
    case node of
      R.UnguardedRhs x1 x2 x3 -> UnguardedRhs (fromResolved x1) [] (fromResolved x2) (fromResolved x3)
      R.GuardedRhss x1 x2 x3 -> GuardedRhss (fromResolved x1) [] (fromResolved x2) (fromResolved x3)

instance (FromResolved body) => FromResolved (R.GuardedRhs body) where
  type ToPending (R.GuardedRhs body) = GuardedRhs (ToPending body)
  fromResolved node =
    case node of
      R.GuardedRhs x1 x2 x3 -> GuardedRhs (fromResolved x1) (fromResolved x2) (fromResolved x3)

instance FromResolved R.GuardQualifier where
  type ToPending R.GuardQualifier = GuardQualifier
  fromResolved node =
    case node of
      R.GuardAnn x1 x2 -> GuardAnn (fromResolved x1) (fromResolved x2)
      R.GuardExpr x1 -> GuardExpr (fromResolved x1)
      R.GuardPat x1 x2 -> GuardPat (fromResolved x1) (fromResolved x2)
      R.GuardLet x1 -> GuardLet (fromResolved x1)

instance (FromResolved a) => FromResolved (R.RecordField a) where
  type ToPending (R.RecordField a) = RecordField (ToPending a)
  fromResolved node =
    case node of
      R.RecordField x1 x2 x3 -> RecordField (fromResolved x1) (fromResolved x2) (fromResolved x3)

instance FromResolved R.LiteralMatch where
  type ToPending R.LiteralMatch = LiteralMatch
  fromResolved node =
    case node of
      R.LiteralMatchFixed -> LiteralMatchFixed
      R.LiteralMatchPrimitive x1 -> LiteralMatchPrimitive (fromResolved x1)
      R.LiteralMatchOverloaded x1 -> LiteralMatchOverloaded (fromResolved x1)

instance FromResolved R.OverloadedLiteralMatch where
  type ToPending R.OverloadedLiteralMatch = OverloadedLiteralMatch
  fromResolved (R.OverloadedLiteralMatch x1 x2 x3 x4) =
    OverloadedLiteralMatch (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4)

instance FromResolved R.NPlusKMatch where
  type ToPending R.NPlusKMatch = NPlusKMatch
  fromResolved (R.NPlusKMatch x1 x2 x3 x4) =
    NPlusKMatch (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4)

instance FromResolved R.LiteralSyntax where
  type ToPending R.LiteralSyntax = LiteralSyntax
  fromResolved syntax =
    case syntax of
      R.LiteralSyntaxPrimitive x1 -> LiteralSyntaxPrimitive (fromResolved x1)
      R.LiteralSyntaxOverloaded x1 x2 -> LiteralSyntaxOverloaded (fromResolved x1) (fromResolved x2)

instance FromResolved R.DoBindMethods where
  type ToPending R.DoBindMethods = DoBindMethods
  fromResolved (R.DoBindMethods x1 x2) = DoBindMethods (fromResolved x1) (fromResolved x2)

instance FromResolved R.DoRecMethods where
  type ToPending R.DoRecMethods = DoRecMethods
  fromResolved (R.DoRecMethods x1 x2 x3) = DoRecMethods (fromResolved x1) (fromResolved x2) (fromResolved x3)

instance FromResolved R.ListMatch where
  type ToPending R.ListMatch = ListMatch
  fromResolved (R.ListMatch x1 x2) = ListMatch (fromResolved x1) (fromResolved x2)

instance FromResolved R.Pattern where
  type ToPending R.Pattern = Pattern
  fromResolved node =
    case node of
      R.PAnn x1 x2 -> PAnn (fromResolved x1) (fromResolved x2)
      R.PVar x1 -> PVar (fromResolved x1)
      R.PTypeBinder x1 -> PTypeBinder (fromResolved x1)
      R.PTypeSyntax x1 x2 -> PTypeSyntax (fromResolved x1) (fromResolved x2)
      R.PWildcard -> PWildcard
      R.PLit x1 x2 -> PLit (fromResolved x1) (fromResolved x2)
      R.PQuasiQuote x1 x2 -> PQuasiQuote (fromResolved x1) (fromResolved x2)
      R.PTuple x1 x2 -> PTuple (fromResolved x1) (fromResolved x2)
      R.PUnboxedSum x1 x2 x3 -> PUnboxedSum (fromResolved x1) (fromResolved x2) (fromResolved x3)
      R.PList x1 x2 -> PList (fromResolved x1) (fromResolved x2)
      R.PCon x1 x2 x3 -> PCon (fromResolved x1) (fromResolved x2) (fromResolved x3)
      R.PBuiltinCon x1 x2 x3 -> PBuiltinCon (fromResolved x1) (fromResolved x2) (fromResolved x3)
      R.PInfix x1 x2 x3 -> PInfix (fromResolved x1) (fromResolved x2) (fromResolved x3)
      R.PView x1 x2 -> PView (fromResolved x1) (fromResolved x2)
      R.PAs x1 x2 -> PAs (fromResolved x1) (fromResolved x2)
      R.PStrict x1 -> PStrict (fromResolved x1)
      R.PIrrefutable x1 -> PIrrefutable (fromResolved x1)
      R.PNegLit x1 x2 -> PNegLit (fromResolved x1) (fromResolved x2)
      R.PNPlusK x1 x2 x3 -> PNPlusK (fromResolved x1) (fromResolved x2) (fromResolved x3)
      R.PParen x1 -> PParen (fromResolved x1)
      R.PRecord x1 x2 x3 -> PRecord (fromResolved x1) (fromResolved x2) (fromResolved x3)
      R.PTypeSig x1 x2 -> PTypeSig (fromResolved x1) (fromResolved x2)
      R.PSplice x1 -> PSplice (fromResolved x1)

instance FromResolved R.ForallTelescope where
  type ToPending R.ForallTelescope = ForallTelescope
  fromResolved node =
    case node of
      R.ForallTelescope x1 x2 -> ForallTelescope (fromResolved x1) (fromResolved x2)

instance FromResolved R.ArrowKind where
  type ToPending R.ArrowKind = ArrowKind
  fromResolved node =
    case node of
      R.ArrowUnrestricted -> ArrowUnrestricted
      R.ArrowLinear -> ArrowLinear
      R.ArrowExplicit x1 -> ArrowExplicit (fromResolved x1)

instance FromResolved R.Type where
  type ToPending R.Type = Type
  fromResolved node =
    case node of
      R.TAnn x1 x2 -> TAnn (fromResolved x1) (fromResolved x2)
      R.TVar x1 -> TVar (fromResolved x1)
      R.TCon x1 x2 -> TCon (fromResolved x1) (fromResolved x2)
      R.TBuiltinCon x1 x2 -> TBuiltinCon (fromResolved x1) (fromResolved x2)
      R.TImplicitParam x1 x2 -> TImplicitParam (fromResolved x1) (fromResolved x2)
      R.TTypeLit x1 -> TTypeLit (fromResolved x1)
      R.TStar x1 -> TStar (fromResolved x1)
      R.TQuasiQuote x1 x2 -> TQuasiQuote (fromResolved x1) (fromResolved x2)
      R.TForall x1 x2 -> TForall (fromResolved x1) (fromResolved x2)
      R.TApp x1 x2 -> TApp (fromResolved x1) (fromResolved x2)
      R.TTypeApp x1 x2 -> TTypeApp (fromResolved x1) (fromResolved x2)
      R.TInfix x1 x2 x3 x4 -> TInfix (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4)
      R.TFun x1 x2 x3 -> TFun (fromResolved x1) (fromResolved x2) (fromResolved x3)
      R.TTuple x1 x2 x3 x4 -> TTuple (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4)
      R.TUnboxedSum x1 -> TUnboxedSum (fromResolved x1)
      R.TList x1 x2 x3 -> TList (fromResolved x1) (fromResolved x2) (fromResolved x3)
      R.TParen x1 -> TParen (fromResolved x1)
      R.TKindSig x1 x2 -> TKindSig (fromResolved x1) (fromResolved x2)
      R.TContext x1 x2 -> TContext (fromResolved x1) (fromResolved x2)
      R.TSplice x1 -> TSplice (fromResolved x1)
      R.TWildcard -> TWildcard

instance FromResolved R.TyVarBinder where
  type ToPending R.TyVarBinder = TyVarBinder
  fromResolved node =
    case node of
      R.TyVarBinder x1 x2 x3 x4 x5 -> TyVarBinder (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4) (fromResolved x5)

instance (FromResolved name) => FromResolved (R.BinderHead name) where
  type ToPending (R.BinderHead name) = BinderHead (ToPending name)
  fromResolved node =
    case node of
      R.PrefixBinderHead x1 x2 -> PrefixBinderHead (fromResolved x1) (fromResolved x2)
      R.InfixBinderHead x1 x2 x3 x4 -> InfixBinderHead (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4)

instance FromResolved R.RoleAnnotation where
  type ToPending R.RoleAnnotation = RoleAnnotation
  fromResolved node =
    case node of
      R.RoleAnnotation x1 x2 -> RoleAnnotation (fromResolved x1) (fromResolved x2)

instance FromResolved R.TypeSynDecl where
  type ToPending R.TypeSynDecl = TypeSynDecl
  fromResolved node =
    case node of
      R.TypeSynDecl x1 x2 -> TypeSynDecl (fromResolved x1) (fromResolved x2)

instance FromResolved R.TypeFamilyDecl where
  type ToPending R.TypeFamilyDecl = TypeFamilyDecl
  fromResolved node =
    case node of
      R.TypeFamilyDecl x1 x2 x3 x4 x5 x6 -> TypeFamilyDecl (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4) (fromResolved x5) (fromResolved x6)

instance FromResolved R.TypeFamilyResultSig where
  type ToPending R.TypeFamilyResultSig = TypeFamilyResultSig
  fromResolved node =
    case node of
      R.TypeFamilyKindSig x1 -> TypeFamilyKindSig (fromResolved x1)
      R.TypeFamilyTyVarSig x1 -> TypeFamilyTyVarSig (fromResolved x1)
      R.TypeFamilyInjectiveSig x1 x2 -> TypeFamilyInjectiveSig (fromResolved x1) (fromResolved x2)

instance FromResolved R.TypeFamilyEq where
  type ToPending R.TypeFamilyEq = TypeFamilyEq
  fromResolved node =
    case node of
      R.TypeFamilyEq x1 x2 x3 x4 x5 -> TypeFamilyEq (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4) (fromResolved x5)

instance FromResolved R.DataFamilyDecl where
  type ToPending R.DataFamilyDecl = DataFamilyDecl
  fromResolved node =
    case node of
      R.DataFamilyDecl x1 x2 -> DataFamilyDecl (fromResolved x1) (fromResolved x2)

instance FromResolved R.TypeFamilyInst where
  type ToPending R.TypeFamilyInst = TypeFamilyInst
  fromResolved node =
    case node of
      R.TypeFamilyInst x1 x2 x3 x4 -> TypeFamilyInst (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4) Nothing

instance FromResolved R.DataFamilyInst where
  type ToPending R.DataFamilyInst = DataFamilyInst
  fromResolved node =
    case node of
      R.DataFamilyInst x1 x2 x3 x4 x5 x6 -> DataFamilyInst (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4) (fromResolved x5) (fromResolved x6) Nothing

instance FromResolved R.DataDecl where
  type ToPending R.DataDecl = DataDecl
  fromResolved node =
    case node of
      R.DataDecl x1 x2 x3 x4 x5 x6 -> DataDecl (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4) (fromResolved x5) (fromResolved x6)

instance FromResolved R.NewtypeDecl where
  type ToPending R.NewtypeDecl = NewtypeDecl
  fromResolved node =
    case node of
      R.NewtypeDecl x1 x2 x3 x4 x5 x6 -> NewtypeDecl (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4) (fromResolved x5) (fromResolved x6)

instance FromResolved R.DataConDecl where
  type ToPending R.DataConDecl = DataConDecl
  fromResolved node =
    case node of
      R.DataConAnn x1 x2 -> DataConAnn (fromResolved x1) (fromResolved x2)
      R.PrefixCon x1 x2 x3 x4 -> PrefixCon Nothing (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4)
      R.InfixCon x1 x2 x3 x4 x5 -> InfixCon Nothing (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4) (fromResolved x5)
      R.RecordCon x1 x2 x3 x4 -> RecordCon Nothing (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4)
      R.GadtCon x1 x2 x3 x4 -> GadtCon Nothing (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4)
      R.TupleCon x1 x2 x3 x4 -> TupleCon Nothing (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4)
      R.UnboxedSumCon x1 x2 x3 x4 x5 -> UnboxedSumCon Nothing (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4) (fromResolved x5)
      R.ListCon x1 x2 x3 -> ListCon Nothing (fromResolved x1) (fromResolved x2) (fromResolved x3)

instance FromResolved R.GadtBody where
  type ToPending R.GadtBody = GadtBody
  fromResolved node =
    case node of
      R.GadtPrefixBody x1 x2 -> GadtPrefixBody (fromResolved x1) (fromResolved x2)
      R.GadtRecordBody x1 x2 -> GadtRecordBody (fromResolved x1) (fromResolved x2)

instance FromResolved R.BangType where
  type ToPending R.BangType = BangType
  fromResolved node =
    case node of
      R.BangType x1 x2 x3 x4 x5 -> BangType (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4) (fromResolved x5)

instance FromResolved R.FieldDecl where
  type ToPending R.FieldDecl = FieldDecl
  fromResolved node =
    case node of
      R.FieldDecl x1 x2 x3 x4 -> FieldDecl (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4) []

instance FromResolved R.DerivingClause where
  type ToPending R.DerivingClause = DerivingClause
  fromResolved node =
    case node of
      R.DerivingClause x1 x2 -> DerivingClause (fromResolved x1) (fromResolved x2) []

instance FromResolved R.DerivingStrategy where
  type ToPending R.DerivingStrategy = DerivingStrategy
  fromResolved node =
    case node of
      R.DerivingStock -> DerivingStock
      R.DerivingNewtype -> DerivingNewtype
      R.DerivingAnyclass -> DerivingAnyclass
      R.DerivingVia x1 -> DerivingVia (fromResolved x1)

instance FromResolved R.StandaloneDerivingDecl where
  type ToPending R.StandaloneDerivingDecl = StandaloneDerivingDecl
  fromResolved node =
    case node of
      R.StandaloneDerivingDecl x1 x2 x3 x4 x5 x6 -> StandaloneDerivingDecl (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4) (fromResolved x5) (fromResolved x6) Nothing

instance FromResolved R.ClassDecl where
  type ToPending R.ClassDecl = ClassDecl
  fromResolved node =
    case node of
      R.ClassDecl x1 x2 x3 x4 -> ClassDecl (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4) Nothing

instance FromResolved R.ClassDeclItem where
  type ToPending R.ClassDeclItem = ClassDeclItem
  fromResolved node =
    case node of
      R.ClassItemAnn x1 x2 -> ClassItemAnn (fromResolved x1) (fromResolved x2)
      R.ClassItemError x1 x2 -> ClassItemError (fromResolved x1) (fromResolved x2)
      R.ClassItemTypeSig x1 x2 -> ClassItemTypeSig (fromResolved x1) (fromResolved x2)
      R.ClassItemDefaultSig x1 x2 -> ClassItemDefaultSig (fromResolved x1) (fromResolved x2)
      R.ClassItemFixity x1 x2 x3 x4 -> ClassItemFixity (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4)
      R.ClassItemDefault x1 -> ClassItemDefault Nothing (fromResolved x1)
      R.ClassItemTypeFamilyDecl x1 -> ClassItemTypeFamilyDecl (fromResolved x1)
      R.ClassItemDataFamilyDecl x1 -> ClassItemDataFamilyDecl (fromResolved x1)
      R.ClassItemDefaultTypeInst x1 -> ClassItemDefaultTypeInst (fromResolved x1)
      R.ClassItemPragma x1 -> ClassItemPragma (fromResolved x1)

instance FromResolved R.InstanceDecl where
  type ToPending R.InstanceDecl = InstanceDecl
  fromResolved node =
    case node of
      R.InstanceDecl x1 x2 x3 x4 x5 x6 -> InstanceDecl (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4) (fromResolved x5) (fromResolved x6) Nothing Nothing

instance FromResolved R.InstanceDeclItem where
  type ToPending R.InstanceDeclItem = InstanceDeclItem
  fromResolved node =
    case node of
      R.InstanceItemAnn x1 x2 -> InstanceItemAnn (fromResolved x1) (fromResolved x2)
      R.InstanceItemError x1 x2 -> InstanceItemError (fromResolved x1) (fromResolved x2)
      R.InstanceItemBind x1 -> InstanceItemBind Nothing (fromResolved x1)
      R.InstanceItemTypeSig x1 x2 -> InstanceItemTypeSig (fromResolved x1) (fromResolved x2)
      R.InstanceItemFixity x1 x2 x3 x4 -> InstanceItemFixity (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4)
      R.InstanceItemTypeFamilyInst x1 -> InstanceItemTypeFamilyInst (fromResolved x1)
      R.InstanceItemDataFamilyInst x1 -> InstanceItemDataFamilyInst (fromResolved x1)
      R.InstanceItemPragma x1 -> InstanceItemPragma (fromResolved x1)

instance FromResolved R.ForeignDecl where
  type ToPending R.ForeignDecl = ForeignDecl
  fromResolved node =
    case node of
      R.ForeignDecl x1 x2 x3 x4 x5 x6 -> ForeignDecl (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4) (fromResolved x5) (fromResolved x6) Nothing

instance FromResolved R.Expr where
  type ToPending R.Expr = Expr
  fromResolved node =
    case node of
      R.EAnn x1 x2 -> EAnn (fromResolved x1) (fromResolved x2)
      R.EVar x1 -> EVar (fromResolved x1)
      R.EImplicitParam x1 -> EImplicitParam (fromResolved x1)
      R.ETypeSyntax x1 x2 -> ETypeSyntax (fromResolved x1) (fromResolved x2)
      R.EInt x1 x2 x3 x4 -> EInt (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4)
      R.EFloat x1 x2 x3 x4 -> EFloat (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4)
      R.EChar x1 x2 -> EChar (fromResolved x1) (fromResolved x2)
      R.ECharHash x1 x2 x3 -> ECharHash (fromResolved x1) (fromResolved x2) (fromResolved x3)
      R.EString x1 x2 x3 -> EString (fromResolved x1) (fromResolved x2) (fromResolved x3)
      R.EStringHash x1 x2 x3 -> EStringHash (fromResolved x1) (fromResolved x2) (fromResolved x3)
      R.EOverloadedLabel x1 x2 -> EOverloadedLabel (fromResolved x1) (fromResolved x2)
      R.EQuasiQuote x1 x2 -> EQuasiQuote (fromResolved x1) (fromResolved x2)
      R.EIf x1 x2 x3 x4 -> EIf (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4)
      R.EMultiWayIf x1 -> EMultiWayIf (fromResolved x1)
      R.ELambdaPats x1 x2 -> ELambdaPats (fromResolved x1) (fromResolved x2)
      R.ELambdaCase x1 -> ELambdaCase (fromResolved x1)
      R.ELambdaCases x1 -> ELambdaCases (fromResolved x1)
      R.EInfix x1 x2 x3 -> EInfix (fromResolved x1) (fromResolved x2) (fromResolved x3)
      R.EViewPat x1 x2 -> EViewPat (fromResolved x1) (fromResolved x2)
      R.ENegate x1 x2 -> ENegate (fromResolved x1) (fromResolved x2)
      R.ESectionL x1 x2 -> ESectionL (fromResolved x1) (fromResolved x2)
      R.ESectionR x1 x2 -> ESectionR (fromResolved x1) (fromResolved x2)
      R.ELetDecls x1 x2 -> ELetDecls (fromResolved x1) (fromResolved x2)
      R.ECase x1 x2 -> ECase (fromResolved x1) (fromResolved x2)
      R.EDo x1 x2 -> EDo (fromResolved x1) (fromResolved x2)
      R.EListComp x1 x2 -> EListComp (fromResolved x1) (fromResolved x2)
      R.EListCompParallel x1 x2 -> EListCompParallel (fromResolved x1) (fromResolved x2)
      R.EArithSeq x1 x2 -> EArithSeq (fromResolved x1) (fromResolved x2)
      R.ERecordCon x1 x2 x3 -> ERecordCon (fromResolved x1) (fromResolved x2) (fromResolved x3)
      R.ERecordUpd x1 x2 -> ERecordUpd (fromResolved x1) (fromResolved x2)
      R.EGetField x1 x2 x3 -> EGetField (fromResolved x1) (fromResolved x2) (fromResolved x3)
      R.EGetFieldProjection x1 x2 -> EGetFieldProjection (fromResolved x1) (fromResolved x2)
      R.ETypeSig x1 x2 -> ETypeSig (fromResolved x1) (fromResolved x2)
      R.EParen x1 -> EParen (fromResolved x1)
      R.EList x1 x2 x3 -> EList (fromResolved x1) (fromResolved x2) (fromResolved x3)
      R.ETuple x1 x2 x3 -> ETuple (fromResolved x1) (fromResolved x2) (fromResolved x3)
      R.EUnboxedSum x1 x2 x3 -> EUnboxedSum (fromResolved x1) (fromResolved x2) (fromResolved x3)
      R.ETypeApp x1 x2 -> ETypeApp (fromResolved x1) (fromResolved x2)
      R.EApp x1 x2 -> EApp (fromResolved x1) (fromResolved x2)
      R.ETHExpQuote x1 -> ETHExpQuote (fromResolved x1)
      R.ETHTypedQuote x1 -> ETHTypedQuote (fromResolved x1)
      R.ETHDeclQuote x1 -> ETHDeclQuote (fromResolved x1)
      R.ETHTypeQuote x1 -> ETHTypeQuote (fromResolved x1)
      R.ETHPatQuote x1 -> ETHPatQuote (fromResolved x1)
      R.ETHNameQuote x1 -> ETHNameQuote (fromResolved x1)
      R.ETHTypeNameQuote x1 -> ETHTypeNameQuote (fromResolved x1)
      R.ETHSplice x1 -> ETHSplice (fromResolved x1)
      R.ETHTypedSplice x1 -> ETHTypedSplice (fromResolved x1)
      R.EProc x1 x2 -> EProc (fromResolved x1) (fromResolved x2)
      R.EPragma x1 x2 -> EPragma (fromResolved x1) (fromResolved x2)

instance (FromResolved body) => FromResolved (R.CaseAlt body) where
  type ToPending (R.CaseAlt body) = CaseAlt (ToPending body)
  fromResolved node =
    case node of
      R.CaseAlt x1 x2 x3 -> CaseAlt (fromResolved x1) (fromResolved x2) (fromResolved x3)

instance FromResolved R.LambdaCaseAlt where
  type ToPending R.LambdaCaseAlt = LambdaCaseAlt
  fromResolved node =
    case node of
      R.LambdaCaseAlt x1 x2 x3 -> LambdaCaseAlt (fromResolved x1) (fromResolved x2) (fromResolved x3)

instance (FromResolved body) => FromResolved (R.DoStmt body) where
  type ToPending (R.DoStmt body) = DoStmt (ToPending body)
  fromResolved node =
    case node of
      R.DoAnn x1 x2 -> DoAnn (fromResolved x1) (fromResolved x2)
      R.DoRecKnot x1 x2 -> DoRecKnot (fromResolved x1) (fromResolved x2)
      R.DoBind x1 x2 x3 -> DoBind (fromResolved x1) (fromResolved x2) (fromResolved x3)
      R.DoLetDecls x1 -> DoLetDecls (fromResolved x1)
      R.DoExpr x1 x2 -> DoExpr (fromResolved x1) (fromResolved x2)
      R.DoRecStmt x1 x2 -> DoRecStmt Nothing (fromResolved x1) (fromResolved x2)

instance FromResolved R.Cmd where
  type ToPending R.Cmd = Cmd
  fromResolved node =
    case node of
      R.CmdAnn x1 x2 -> CmdAnn (fromResolved x1) (fromResolved x2)
      R.CmdArrApp x1 x2 x3 -> CmdArrApp (fromResolved x1) (fromResolved x2) (fromResolved x3)
      R.CmdInfix x1 x2 x3 -> CmdInfix (fromResolved x1) (fromResolved x2) (fromResolved x3)
      R.CmdDo x1 -> CmdDo (fromResolved x1)
      R.CmdIf x1 x2 x3 -> CmdIf (fromResolved x1) (fromResolved x2) (fromResolved x3)
      R.CmdCase x1 x2 -> CmdCase (fromResolved x1) (fromResolved x2)
      R.CmdLet x1 x2 -> CmdLet (fromResolved x1) (fromResolved x2)
      R.CmdLam x1 x2 -> CmdLam (fromResolved x1) (fromResolved x2)
      R.CmdApp x1 x2 -> CmdApp (fromResolved x1) (fromResolved x2)
      R.CmdPar x1 -> CmdPar (fromResolved x1)

instance FromResolved R.CompStmt where
  type ToPending R.CompStmt = CompStmt
  fromResolved node =
    case node of
      R.CompAnn x1 x2 -> CompAnn (fromResolved x1) (fromResolved x2)
      R.CompGen x1 x2 -> CompGen Nothing (fromResolved x1) (fromResolved x2)
      R.CompGuard x1 -> CompGuard Nothing (fromResolved x1)
      R.CompLetDecls x1 -> CompLetDecls Nothing (fromResolved x1)
      R.CompThen x1 -> CompThen Nothing (fromResolved x1)
      R.CompThenBy x1 x2 -> CompThenBy Nothing (fromResolved x1) (fromResolved x2)
      R.CompGroupUsing x1 -> CompGroupUsing Nothing (fromResolved x1)
      R.CompGroupByUsing x1 x2 -> CompGroupByUsing Nothing (fromResolved x1) (fromResolved x2)

instance FromResolved R.ArithSeq where
  type ToPending R.ArithSeq = ArithSeq
  fromResolved node =
    case node of
      R.ArithSeqAnn x1 x2 -> ArithSeqAnn (fromResolved x1) (fromResolved x2)
      R.ArithSeqFrom x1 x2 -> ArithSeqFrom (fromResolved x1) (fromResolved x2)
      R.ArithSeqFromThen x1 x2 x3 -> ArithSeqFromThen (fromResolved x1) (fromResolved x2) (fromResolved x3)
      R.ArithSeqFromTo x1 x2 x3 -> ArithSeqFromTo (fromResolved x1) (fromResolved x2) (fromResolved x3)
      R.ArithSeqFromThenTo x1 x2 x3 x4 -> ArithSeqFromThenTo (fromResolved x1) (fromResolved x2) (fromResolved x3) (fromResolved x4)

instance FromResolved P.Literal where
  type ToPending P.Literal = Literal
  fromResolved node =
    case node of
      P.LitAnn x1 x2 -> LitAnn (fromResolved x1) (fromResolved x2)
      P.LitInt x1 x2 x3 -> LitInt (fromResolved x1) (fromResolved x2) (fromResolved x3)
      P.LitFloat x1 x2 x3 -> LitFloat (fromResolved x1) (fromResolved x2) (fromResolved x3)
      P.LitChar x1 x2 -> LitChar (fromResolved x1) (fromResolved x2)
      P.LitCharHash x1 x2 -> LitCharHash (fromResolved x1) (fromResolved x2)
      P.LitString x1 x2 -> LitString (fromResolved x1) (fromResolved x2)
      P.LitStringHash x1 x2 -> LitStringHash (fromResolved x1) (fromResolved x2)
