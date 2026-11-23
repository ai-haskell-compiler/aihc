{-# LANGUAGE TypeFamilies #-}

-- | Change the tree that the type checker gives into a resolved syntax
-- tree with 'Annotation' values. The phases after the type checker read
-- the parser tree. "Aihc.Resolve.Syntax.ToParser" changes the result of
-- this module into that tree.
--
-- Each item of type checker information becomes an annotation, in the
-- position where the type checker put it before the explicit tree existed:
--
-- * A wrapper constructor, such as 'ETc', becomes the annotation wrapper of
--   its syntax, such as 'R.EAnn'.
-- * The items of a name or a binder come after its annotations.
-- * The items of a module, a rule, a rule binder, an equation, or a
--   right-hand side come before its annotations.
--
-- This change is temporary. When the phases after the type checker read
-- the tree of "Aihc.Tc.Syntax", this module goes away.
--
-- Each instance has one case for each constructor of the data declaration.
-- When a constructor changes in one tree, change its case here too.
module Aihc.Tc.Syntax.ToResolved
  ( ToResolved (..),
    infoAnnotation,
  )
where

import Aihc.Parser.Syntax qualified as P
import Aihc.Resolve.Syntax qualified as R
import Aihc.Resolve.Syntax.ToParser (toParserMatch)
import Aihc.Tc.Annotations
  ( TcCoercedDeriving (..),
    TcDerivedInstance (..),
    TcDerivingClauseInstance (..),
    TcPatSynAnnotation (..),
    TcPatternInstantiation (..),
    TcRecKnotOccurrence (..),
    TcSelectorType (..),
  )
import Aihc.Tc.Syntax
import Data.Text (Text)

-- | The annotation that carries one item of type checker information.
infoAnnotation :: TcInfo -> Annotation
infoAnnotation item =
  case item of
    InfoType annotation -> P.mkAnnotation annotation
    InfoCast annotation -> P.mkAnnotation annotation
    InfoMatchArityCast annotation -> P.mkAnnotation annotation
    InfoPatternInstantiation -> P.mkAnnotation TcPatternInstantiation
    InfoInstanceMethod annotation -> P.mkAnnotation annotation
    InfoInstance annotation -> P.mkAnnotation annotation
    InfoClass annotation -> P.mkAnnotation annotation
    InfoDeriving annotation -> P.mkAnnotation annotation
    InfoDerivedInstance -> P.mkAnnotation TcDerivedInstance
    InfoDerivingClauseInstance -> P.mkAnnotation TcDerivingClauseInstance
    InfoCoercedDeriving plan -> P.mkAnnotation (TcCoercedDeriving plan)
    InfoSelectorType ty -> P.mkAnnotation (TcSelectorType ty)
    InfoRecKnotOccurrence entity -> P.mkAnnotation (TcRecKnotOccurrence entity)
    InfoModuleIdentity identity -> P.mkAnnotation identity
    InfoForeignImport annotation -> P.mkAnnotation annotation
    InfoTypeFamilyInstance info -> P.mkAnnotation info
    InfoDataFamilyInstance info -> P.mkAnnotation info
    InfoPatSyn (PatSynEquations matcher builder selectors) ->
      P.mkAnnotation
        ( TcPatSynAnnotation
            (parserMatch matcher)
            (map parserMatch <$> builder)
            [(label, parserMatch selector) | (label, selector) <- selectors]
        )
  where
    parserMatch = toParserMatch . toResolved

class ToResolved a where
  type ResolvedTree a
  toResolved :: a -> ResolvedTree a

instance (ToResolved a) => ToResolved [a] where
  type ResolvedTree [a] = [ResolvedTree a]
  toResolved = fmap toResolved

instance (ToResolved a) => ToResolved (Maybe a) where
  type ResolvedTree (Maybe a) = Maybe (ResolvedTree a)
  toResolved = fmap toResolved

instance (ToResolved a, ToResolved b) => ToResolved (a, b) where
  type ResolvedTree (a, b) = (ResolvedTree a, ResolvedTree b)
  toResolved (left, right) = (toResolved left, toResolved right)

instance (ToResolved a, ToResolved b) => ToResolved (Either a b) where
  type ResolvedTree (Either a b) = Either (ResolvedTree a) (ResolvedTree b)
  toResolved = either (Left . toResolved) (Right . toResolved)

instance ToResolved Annotation where
  type ResolvedTree Annotation = Annotation
  toResolved = id

instance ToResolved ArrAppType where
  type ResolvedTree ArrAppType = ArrAppType
  toResolved = id

instance ToResolved Bool where
  type ResolvedTree Bool = Bool
  toResolved = id

instance ToResolved BuiltinCon where
  type ResolvedTree BuiltinCon = BuiltinCon
  toResolved = id

instance ToResolved CallConv where
  type ResolvedTree CallConv = CallConv
  toResolved = id

instance ToResolved Char where
  type ResolvedTree Char = Char
  toResolved = id

instance ToResolved DoFlavor where
  type ResolvedTree DoFlavor = DoFlavor
  toResolved = id

instance ToResolved ExtensionSetting where
  type ResolvedTree ExtensionSetting = ExtensionSetting
  toResolved = id

instance ToResolved FixityAssoc where
  type ResolvedTree FixityAssoc = FixityAssoc
  toResolved = id

instance ToResolved FloatType where
  type ResolvedTree FloatType = FloatType
  toResolved = id

instance ToResolved ForallVis where
  type ResolvedTree ForallVis = ForallVis
  toResolved = id

instance ToResolved ForeignDirection where
  type ResolvedTree ForeignDirection = ForeignDirection
  toResolved = id

instance ToResolved ForeignEntitySpec where
  type ResolvedTree ForeignEntitySpec = ForeignEntitySpec
  toResolved = id

instance ToResolved ForeignSafety where
  type ResolvedTree ForeignSafety = ForeignSafety
  toResolved = id

instance ToResolved FunctionalDependency where
  type ResolvedTree FunctionalDependency = FunctionalDependency
  toResolved = id

instance ToResolved IEBundledNamespace where
  type ResolvedTree IEBundledNamespace = IEBundledNamespace
  toResolved = id

instance ToResolved IEEntityNamespace where
  type ResolvedTree IEEntityNamespace = IEEntityNamespace
  toResolved = id

instance ToResolved ImportLevel where
  type ResolvedTree ImportLevel = ImportLevel
  toResolved = id

instance ToResolved Int where
  type ResolvedTree Int = Int
  toResolved = id

instance ToResolved Integer where
  type ResolvedTree Integer = Integer
  toResolved = id

instance ToResolved MatchHeadForm where
  type ResolvedTree MatchHeadForm = MatchHeadForm
  toResolved = id

instance ToResolved NameType where
  type ResolvedTree NameType = NameType
  toResolved = id

instance ToResolved NumericType where
  type ResolvedTree NumericType = NumericType
  toResolved = id

instance ToResolved PatSynArgs where
  type ResolvedTree PatSynArgs = PatSynArgs
  toResolved = id

instance ToResolved Pragma where
  type ResolvedTree Pragma = Pragma
  toResolved = id

instance ToResolved Rational where
  type ResolvedTree Rational = Rational
  toResolved = id

instance ToResolved RecKnotVariables where
  type ResolvedTree RecKnotVariables = RecKnotVariables
  toResolved = id

instance ToResolved ResolutionResult where
  type ResolvedTree ResolutionResult = ResolutionResult
  toResolved = id

instance ToResolved ResolveError where
  type ResolvedTree ResolveError = ResolveError
  toResolved = id

instance ToResolved Role where
  type ResolvedTree Role = Role
  toResolved = id

instance ToResolved RuleActivation where
  type ResolvedTree RuleActivation = RuleActivation
  toResolved = id

instance ToResolved Text where
  type ResolvedTree Text = Text
  toResolved = id

instance ToResolved TupleFlavor where
  type ResolvedTree TupleFlavor = TupleFlavor
  toResolved = id

instance ToResolved TyVarBSpecificity where
  type ResolvedTree TyVarBSpecificity = TyVarBSpecificity
  toResolved = id

instance ToResolved TyVarBVisibility where
  type ResolvedTree TyVarBVisibility = TyVarBVisibility
  toResolved = id

instance ToResolved TypeFamilyInjectivity where
  type ResolvedTree TypeFamilyInjectivity = TypeFamilyInjectivity
  toResolved = id

instance ToResolved TypeHeadForm where
  type ResolvedTree TypeHeadForm = TypeHeadForm
  toResolved = id

instance ToResolved TypeLiteral where
  type ResolvedTree TypeLiteral = TypeLiteral
  toResolved = id

instance ToResolved TypePromotion where
  type ResolvedTree TypePromotion = TypePromotion
  toResolved = id

instance ToResolved TypeSyntaxForm where
  type ResolvedTree TypeSyntaxForm = TypeSyntaxForm
  toResolved = id

instance ToResolved Literal where
  type ResolvedTree Literal = P.Literal
  toResolved node =
    case node of
      LitAnn x1 x2 -> P.LitAnn (toResolved x1) (toResolved x2)
      LitTc x1 x2 -> P.LitAnn (infoAnnotation x1) (toResolved x2)
      LitInt x1 x2 x3 -> P.LitInt (toResolved x1) (toResolved x2) (toResolved x3)
      LitFloat x1 x2 x3 -> P.LitFloat (toResolved x1) (toResolved x2) (toResolved x3)
      LitChar x1 x2 -> P.LitChar (toResolved x1) (toResolved x2)
      LitCharHash x1 x2 -> P.LitCharHash (toResolved x1) (toResolved x2)
      LitString x1 x2 -> P.LitString (toResolved x1) (toResolved x2)
      LitStringHash x1 x2 -> P.LitStringHash (toResolved x1) (toResolved x2)

instance ToResolved Name where
  type ResolvedTree Name = R.Name
  toResolved node =
    case node of
      Name x1 x2 x3 x4 x5 x6 -> R.Name (toResolved x1) (toResolved x2) (toResolved x3) (x4 <> map infoAnnotation x6) (toResolved x5)

instance ToResolved UnqualifiedName where
  type ResolvedTree UnqualifiedName = R.UnqualifiedName
  toResolved node =
    case node of
      UnqualifiedName x1 x2 x3 x4 x5 -> R.UnqualifiedName (toResolved x1) (toResolved x2) (x3 <> map infoAnnotation x5) (toResolved x4)

instance ToResolved Module where
  type ResolvedTree Module = R.Module
  toResolved node =
    case node of
      Module x1 x2 x3 x4 x5 x6 _ -> R.Module (map infoAnnotation x2 <> x1) (toResolved x3) (toResolved x4) (toResolved x5) (toResolved x6)

instance ToResolved ModuleHead where
  type ResolvedTree ModuleHead = R.ModuleHead
  toResolved node =
    case node of
      ModuleHead x1 x2 x3 x4 -> R.ModuleHead (toResolved x1) (toResolved x2) (toResolved x3) (toResolved x4)

instance ToResolved IEBundledMember where
  type ResolvedTree IEBundledMember = R.IEBundledMember
  toResolved node =
    case node of
      IEBundledMember x1 x2 -> R.IEBundledMember (toResolved x1) (toResolved x2)

instance ToResolved ExportSpec where
  type ResolvedTree ExportSpec = R.ExportSpec
  toResolved node =
    case node of
      ExportModule x1 x2 -> R.ExportModule (toResolved x1) (toResolved x2)
      ExportVar x1 x2 x3 -> R.ExportVar (toResolved x1) (toResolved x2) (toResolved x3)
      ExportAbs x1 x2 x3 -> R.ExportAbs (toResolved x1) (toResolved x2) (toResolved x3)
      ExportAll x1 x2 x3 -> R.ExportAll (toResolved x1) (toResolved x2) (toResolved x3)
      ExportWith x1 x2 x3 x4 -> R.ExportWith (toResolved x1) (toResolved x2) (toResolved x3) (toResolved x4)
      ExportWithAll x1 x2 x3 x4 x5 -> R.ExportWithAll (toResolved x1) (toResolved x2) (toResolved x3) (toResolved x4) (toResolved x5)
      ExportAnn x1 x2 -> R.ExportAnn (toResolved x1) (toResolved x2)

instance ToResolved ImportDecl where
  type ResolvedTree ImportDecl = R.ImportDecl
  toResolved node =
    case node of
      ImportDecl x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 -> R.ImportDecl (toResolved x1) (toResolved x2) (toResolved x3) (toResolved x4) (toResolved x5) (toResolved x6) (toResolved x7) (toResolved x8) (toResolved x9) (toResolved x10) (toResolved x11)

instance ToResolved ImportSpec where
  type ResolvedTree ImportSpec = R.ImportSpec
  toResolved node =
    case node of
      ImportSpec x1 x2 x3 -> R.ImportSpec (toResolved x1) (toResolved x2) (toResolved x3)

instance ToResolved ImportItem where
  type ResolvedTree ImportItem = R.ImportItem
  toResolved node =
    case node of
      ImportItemVar x1 x2 -> R.ImportItemVar (toResolved x1) (toResolved x2)
      ImportItemAbs x1 x2 -> R.ImportItemAbs (toResolved x1) (toResolved x2)
      ImportItemAll x1 x2 -> R.ImportItemAll (toResolved x1) (toResolved x2)
      ImportItemWith x1 x2 x3 -> R.ImportItemWith (toResolved x1) (toResolved x2) (toResolved x3)
      ImportItemAllWith x1 x2 x3 x4 -> R.ImportItemAllWith (toResolved x1) (toResolved x2) (toResolved x3) (toResolved x4)
      ImportAnn x1 x2 -> R.ImportAnn (toResolved x1) (toResolved x2)
      ImportItemError x1 x2 -> R.ImportItemError (toResolved x1) (toResolved x2)

instance ToResolved Decl where
  type ResolvedTree Decl = R.Decl
  toResolved node =
    case node of
      DeclAnn x1 x2 -> R.DeclAnn (toResolved x1) (toResolved x2)
      DeclTc x1 x2 -> R.DeclAnn (infoAnnotation x1) (toResolved x2)
      DeclError x1 x2 -> R.DeclError (toResolved x1) (toResolved x2)
      DeclValue x1 -> R.DeclValue (toResolved x1)
      DeclImplicitParam x1 x2 x3 -> R.DeclImplicitParam (toResolved x1) (toResolved x2) (toResolved x3)
      DeclTypeSig x1 x2 -> R.DeclTypeSig (toResolved x1) (toResolved x2)
      DeclPatSyn x1 -> R.DeclPatSyn (toResolved x1)
      DeclPatSynSig x1 x2 -> R.DeclPatSynSig (toResolved x1) (toResolved x2)
      DeclStandaloneKindSig x1 x2 -> R.DeclStandaloneKindSig (toResolved x1) (toResolved x2)
      DeclFixity x1 x2 x3 x4 -> R.DeclFixity (toResolved x1) (toResolved x2) (toResolved x3) (toResolved x4)
      DeclRoleAnnotation x1 -> R.DeclRoleAnnotation (toResolved x1)
      DeclTypeSyn x1 -> R.DeclTypeSyn (toResolved x1)
      DeclTypeData x1 -> R.DeclTypeData (toResolved x1)
      DeclData x1 -> R.DeclData (toResolved x1)
      DeclNewtype x1 -> R.DeclNewtype (toResolved x1)
      DeclClass x1 -> R.DeclClass (toResolved x1)
      DeclInstance x1 -> R.DeclInstance (toResolved x1)
      DeclStandaloneDeriving x1 -> R.DeclStandaloneDeriving (toResolved x1)
      DeclDefault x1 -> R.DeclDefault (toResolved x1)
      DeclSplice x1 -> R.DeclSplice (toResolved x1)
      DeclForeign x1 -> R.DeclForeign (toResolved x1)
      DeclTypeFamilyDecl x1 -> R.DeclTypeFamilyDecl (toResolved x1)
      DeclDataFamilyDecl x1 -> R.DeclDataFamilyDecl (toResolved x1)
      DeclTypeFamilyInst x1 -> R.DeclTypeFamilyInst (toResolved x1)
      DeclDataFamilyInst x1 -> R.DeclDataFamilyInst (toResolved x1)
      DeclPragma x1 -> R.DeclPragma (toResolved x1)
      DeclRules x1 -> R.DeclRules (toResolved x1)

instance ToResolved RuleDecl where
  type ResolvedTree RuleDecl = R.RuleDecl
  toResolved node =
    case node of
      RuleDecl x1 x2 x3 x4 x5 x6 x7 x8 x9 -> R.RuleDecl (map infoAnnotation x2 <> x1) (toResolved x3) (toResolved x4) (toResolved x5) (toResolved x6) (toResolved x7) (toResolved x8) (toResolved x9)

instance ToResolved RuleBinder where
  type ResolvedTree RuleBinder = R.RuleBinder
  toResolved node =
    case node of
      RuleBinder x1 x2 x3 x4 -> R.RuleBinder (map infoAnnotation x2 <> x1) (toResolved x3) (toResolved x4)

instance ToResolved MultiplicityTag where
  type ResolvedTree MultiplicityTag = R.MultiplicityTag
  toResolved node =
    case node of
      NoMultiplicityTag -> R.NoMultiplicityTag
      LinearMultiplicityTag -> R.LinearMultiplicityTag
      ExplicitMultiplicityTag x1 -> R.ExplicitMultiplicityTag (toResolved x1)

instance ToResolved ValueDecl where
  type ResolvedTree ValueDecl = R.ValueDecl
  toResolved node =
    case node of
      FunctionBind x1 x2 -> R.FunctionBind (toResolved x1) (toResolved x2)
      PatternBind x1 x2 x3 -> R.PatternBind (toResolved x1) (toResolved x2) (toResolved x3)

instance ToResolved Match where
  type ResolvedTree Match = R.Match
  toResolved node =
    case node of
      Match x1 x2 x3 x4 x5 -> R.Match (map infoAnnotation x2 <> x1) (toResolved x3) (toResolved x4) (toResolved x5)

instance ToResolved PatSynDir where
  type ResolvedTree PatSynDir = R.PatSynDir
  toResolved node =
    case node of
      PatSynUnidirectional -> R.PatSynUnidirectional
      PatSynBidirectional -> R.PatSynBidirectional
      PatSynExplicitBidirectional x1 -> R.PatSynExplicitBidirectional (toResolved x1)

instance ToResolved PatSynDecl where
  type ResolvedTree PatSynDecl = R.PatSynDecl
  toResolved node =
    case node of
      PatSynDecl x1 x2 x3 x4 -> R.PatSynDecl (toResolved x1) (toResolved x2) (toResolved x3) (toResolved x4)

instance (ToResolved body) => ToResolved (Rhs body) where
  type ResolvedTree (Rhs body) = R.Rhs (ResolvedTree body)
  toResolved node =
    case node of
      UnguardedRhs x1 x2 x3 x4 -> R.UnguardedRhs (map infoAnnotation x2 <> x1) (toResolved x3) (toResolved x4)
      GuardedRhss x1 x2 x3 x4 -> R.GuardedRhss (map infoAnnotation x2 <> x1) (toResolved x3) (toResolved x4)

instance (ToResolved body) => ToResolved (GuardedRhs body) where
  type ResolvedTree (GuardedRhs body) = R.GuardedRhs (ResolvedTree body)
  toResolved node =
    case node of
      GuardedRhs x1 x2 x3 -> R.GuardedRhs (toResolved x1) (toResolved x2) (toResolved x3)

instance ToResolved GuardQualifier where
  type ResolvedTree GuardQualifier = R.GuardQualifier
  toResolved node =
    case node of
      GuardAnn x1 x2 -> R.GuardAnn (toResolved x1) (toResolved x2)
      GuardExpr x1 -> R.GuardExpr (toResolved x1)
      GuardPat x1 x2 -> R.GuardPat (toResolved x1) (toResolved x2)
      GuardLet x1 -> R.GuardLet (toResolved x1)

instance (ToResolved a) => ToResolved (RecordField a) where
  type ResolvedTree (RecordField a) = R.RecordField (ResolvedTree a)
  toResolved node =
    case node of
      RecordField x1 x2 x3 -> R.RecordField (toResolved x1) (toResolved x2) (toResolved x3)

instance ToResolved Pattern where
  type ResolvedTree Pattern = R.Pattern
  toResolved node =
    case node of
      PAnn x1 x2 -> R.PAnn (toResolved x1) (toResolved x2)
      PTc x1 x2 -> R.PAnn (infoAnnotation x1) (toResolved x2)
      PResolved x1 x2 -> R.PResolved (toResolved x1) (toResolved x2)
      PVar x1 -> R.PVar (toResolved x1)
      PTypeBinder x1 -> R.PTypeBinder (toResolved x1)
      PTypeSyntax x1 x2 -> R.PTypeSyntax (toResolved x1) (toResolved x2)
      PWildcard -> R.PWildcard
      PLit x1 -> R.PLit (toResolved x1)
      PQuasiQuote x1 x2 -> R.PQuasiQuote (toResolved x1) (toResolved x2)
      PTuple x1 x2 -> R.PTuple (toResolved x1) (toResolved x2)
      PUnboxedSum x1 x2 x3 -> R.PUnboxedSum (toResolved x1) (toResolved x2) (toResolved x3)
      PList x1 -> R.PList (toResolved x1)
      PCon x1 x2 x3 -> R.PCon (toResolved x1) (toResolved x2) (toResolved x3)
      PBuiltinCon x1 x2 x3 -> R.PBuiltinCon (toResolved x1) (toResolved x2) (toResolved x3)
      PInfix x1 x2 x3 -> R.PInfix (toResolved x1) (toResolved x2) (toResolved x3)
      PView x1 x2 -> R.PView (toResolved x1) (toResolved x2)
      PAs x1 x2 -> R.PAs (toResolved x1) (toResolved x2)
      PStrict x1 -> R.PStrict (toResolved x1)
      PIrrefutable x1 -> R.PIrrefutable (toResolved x1)
      PNegLit x1 -> R.PNegLit (toResolved x1)
      PNPlusK x1 x2 -> R.PNPlusK (toResolved x1) (toResolved x2)
      PParen x1 -> R.PParen (toResolved x1)
      PRecord x1 x2 x3 -> R.PRecord (toResolved x1) (toResolved x2) (toResolved x3)
      PTypeSig x1 x2 -> R.PTypeSig (toResolved x1) (toResolved x2)
      PSplice x1 -> R.PSplice (toResolved x1)

instance ToResolved ForallTelescope where
  type ResolvedTree ForallTelescope = R.ForallTelescope
  toResolved node =
    case node of
      ForallTelescope x1 x2 -> R.ForallTelescope (toResolved x1) (toResolved x2)

instance ToResolved ArrowKind where
  type ResolvedTree ArrowKind = R.ArrowKind
  toResolved node =
    case node of
      ArrowUnrestricted -> R.ArrowUnrestricted
      ArrowLinear -> R.ArrowLinear
      ArrowExplicit x1 -> R.ArrowExplicit (toResolved x1)

instance ToResolved Type where
  type ResolvedTree Type = R.Type
  toResolved node =
    case node of
      TAnn x1 x2 -> R.TAnn (toResolved x1) (toResolved x2)
      TResolved x1 x2 -> R.TResolved (toResolved x1) (toResolved x2)
      TVar x1 -> R.TVar (toResolved x1)
      TCon x1 x2 -> R.TCon (toResolved x1) (toResolved x2)
      TBuiltinCon x1 x2 -> R.TBuiltinCon (toResolved x1) (toResolved x2)
      TImplicitParam x1 x2 -> R.TImplicitParam (toResolved x1) (toResolved x2)
      TTypeLit x1 -> R.TTypeLit (toResolved x1)
      TStar x1 -> R.TStar (toResolved x1)
      TQuasiQuote x1 x2 -> R.TQuasiQuote (toResolved x1) (toResolved x2)
      TForall x1 x2 -> R.TForall (toResolved x1) (toResolved x2)
      TApp x1 x2 -> R.TApp (toResolved x1) (toResolved x2)
      TTypeApp x1 x2 -> R.TTypeApp (toResolved x1) (toResolved x2)
      TInfix x1 x2 x3 x4 -> R.TInfix (toResolved x1) (toResolved x2) (toResolved x3) (toResolved x4)
      TFun x1 x2 x3 -> R.TFun (toResolved x1) (toResolved x2) (toResolved x3)
      TTuple x1 x2 x3 -> R.TTuple (toResolved x1) (toResolved x2) (toResolved x3)
      TUnboxedSum x1 -> R.TUnboxedSum (toResolved x1)
      TList x1 x2 -> R.TList (toResolved x1) (toResolved x2)
      TParen x1 -> R.TParen (toResolved x1)
      TKindSig x1 x2 -> R.TKindSig (toResolved x1) (toResolved x2)
      TContext x1 x2 -> R.TContext (toResolved x1) (toResolved x2)
      TSplice x1 -> R.TSplice (toResolved x1)
      TWildcard -> R.TWildcard

instance ToResolved TyVarBinder where
  type ResolvedTree TyVarBinder = R.TyVarBinder
  toResolved node =
    case node of
      TyVarBinder x1 x2 x3 x4 x5 -> R.TyVarBinder (toResolved x1) (toResolved x2) (toResolved x3) (toResolved x4) (toResolved x5)

instance (ToResolved name) => ToResolved (BinderHead name) where
  type ResolvedTree (BinderHead name) = R.BinderHead (ResolvedTree name)
  toResolved node =
    case node of
      PrefixBinderHead x1 x2 -> R.PrefixBinderHead (toResolved x1) (toResolved x2)
      InfixBinderHead x1 x2 x3 x4 -> R.InfixBinderHead (toResolved x1) (toResolved x2) (toResolved x3) (toResolved x4)

instance ToResolved RoleAnnotation where
  type ResolvedTree RoleAnnotation = R.RoleAnnotation
  toResolved node =
    case node of
      RoleAnnotation x1 x2 -> R.RoleAnnotation (toResolved x1) (toResolved x2)

instance ToResolved TypeSynDecl where
  type ResolvedTree TypeSynDecl = R.TypeSynDecl
  toResolved node =
    case node of
      TypeSynDecl x1 x2 -> R.TypeSynDecl (toResolved x1) (toResolved x2)

instance ToResolved TypeFamilyDecl where
  type ResolvedTree TypeFamilyDecl = R.TypeFamilyDecl
  toResolved node =
    case node of
      TypeFamilyDecl x1 x2 x3 x4 x5 x6 -> R.TypeFamilyDecl (toResolved x1) (toResolved x2) (toResolved x3) (toResolved x4) (toResolved x5) (toResolved x6)

instance ToResolved TypeFamilyResultSig where
  type ResolvedTree TypeFamilyResultSig = R.TypeFamilyResultSig
  toResolved node =
    case node of
      TypeFamilyKindSig x1 -> R.TypeFamilyKindSig (toResolved x1)
      TypeFamilyTyVarSig x1 -> R.TypeFamilyTyVarSig (toResolved x1)
      TypeFamilyInjectiveSig x1 x2 -> R.TypeFamilyInjectiveSig (toResolved x1) (toResolved x2)

instance ToResolved TypeFamilyEq where
  type ResolvedTree TypeFamilyEq = R.TypeFamilyEq
  toResolved node =
    case node of
      TypeFamilyEq x1 x2 x3 x4 x5 -> R.TypeFamilyEq (toResolved x1) (toResolved x2) (toResolved x3) (toResolved x4) (toResolved x5)

instance ToResolved DataFamilyDecl where
  type ResolvedTree DataFamilyDecl = R.DataFamilyDecl
  toResolved node =
    case node of
      DataFamilyDecl x1 x2 -> R.DataFamilyDecl (toResolved x1) (toResolved x2)

instance ToResolved TypeFamilyInst where
  type ResolvedTree TypeFamilyInst = R.TypeFamilyInst
  toResolved node =
    case node of
      TypeFamilyInst x1 x2 x3 x4 -> R.TypeFamilyInst (toResolved x1) (toResolved x2) (toResolved x3) (toResolved x4)

instance ToResolved DataFamilyInst where
  type ResolvedTree DataFamilyInst = R.DataFamilyInst
  toResolved node =
    case node of
      DataFamilyInst x1 x2 x3 x4 x5 x6 -> R.DataFamilyInst (toResolved x1) (toResolved x2) (toResolved x3) (toResolved x4) (toResolved x5) (toResolved x6)

instance ToResolved DataDecl where
  type ResolvedTree DataDecl = R.DataDecl
  toResolved node =
    case node of
      DataDecl x1 x2 x3 x4 x5 x6 -> R.DataDecl (toResolved x1) (toResolved x2) (toResolved x3) (toResolved x4) (toResolved x5) (toResolved x6)

instance ToResolved NewtypeDecl where
  type ResolvedTree NewtypeDecl = R.NewtypeDecl
  toResolved node =
    case node of
      NewtypeDecl x1 x2 x3 x4 x5 x6 -> R.NewtypeDecl (toResolved x1) (toResolved x2) (toResolved x3) (toResolved x4) (toResolved x5) (toResolved x6)

instance ToResolved DataConDecl where
  type ResolvedTree DataConDecl = R.DataConDecl
  toResolved node =
    case node of
      DataConAnn x1 x2 -> R.DataConAnn (toResolved x1) (toResolved x2)
      DataConTc x1 x2 -> R.DataConAnn (infoAnnotation x1) (toResolved x2)
      DataConResolved x1 x2 -> R.DataConResolved (toResolved x1) (toResolved x2)
      PrefixCon x1 x2 x3 x4 -> R.PrefixCon (toResolved x1) (toResolved x2) (toResolved x3) (toResolved x4)
      InfixCon x1 x2 x3 x4 x5 -> R.InfixCon (toResolved x1) (toResolved x2) (toResolved x3) (toResolved x4) (toResolved x5)
      RecordCon x1 x2 x3 x4 -> R.RecordCon (toResolved x1) (toResolved x2) (toResolved x3) (toResolved x4)
      GadtCon x1 x2 x3 x4 -> R.GadtCon (toResolved x1) (toResolved x2) (toResolved x3) (toResolved x4)
      TupleCon x1 x2 x3 x4 -> R.TupleCon (toResolved x1) (toResolved x2) (toResolved x3) (toResolved x4)
      UnboxedSumCon x1 x2 x3 x4 x5 -> R.UnboxedSumCon (toResolved x1) (toResolved x2) (toResolved x3) (toResolved x4) (toResolved x5)
      ListCon x1 x2 -> R.ListCon (toResolved x1) (toResolved x2)

instance ToResolved GadtBody where
  type ResolvedTree GadtBody = R.GadtBody
  toResolved node =
    case node of
      GadtPrefixBody x1 x2 -> R.GadtPrefixBody (toResolved x1) (toResolved x2)
      GadtRecordBody x1 x2 -> R.GadtRecordBody (toResolved x1) (toResolved x2)

instance ToResolved BangType where
  type ResolvedTree BangType = R.BangType
  toResolved node =
    case node of
      BangType x1 x2 x3 x4 x5 -> R.BangType (toResolved x1) (toResolved x2) (toResolved x3) (toResolved x4) (toResolved x5)

instance ToResolved FieldDecl where
  type ResolvedTree FieldDecl = R.FieldDecl
  toResolved node =
    case node of
      FieldDecl x1 x2 x3 x4 -> R.FieldDecl (toResolved x1) (toResolved x2) (toResolved x3) (toResolved x4)

instance ToResolved DerivingClause where
  type ResolvedTree DerivingClause = R.DerivingClause
  toResolved node =
    case node of
      DerivingClause x1 x2 -> R.DerivingClause (toResolved x1) (toResolved x2)

instance ToResolved DerivingStrategy where
  type ResolvedTree DerivingStrategy = R.DerivingStrategy
  toResolved node =
    case node of
      DerivingStock -> R.DerivingStock
      DerivingNewtype -> R.DerivingNewtype
      DerivingAnyclass -> R.DerivingAnyclass
      DerivingVia x1 -> R.DerivingVia (toResolved x1)

instance ToResolved StandaloneDerivingDecl where
  type ResolvedTree StandaloneDerivingDecl = R.StandaloneDerivingDecl
  toResolved node =
    case node of
      StandaloneDerivingDecl x1 x2 x3 x4 x5 x6 -> R.StandaloneDerivingDecl (toResolved x1) (toResolved x2) (toResolved x3) (toResolved x4) (toResolved x5) (toResolved x6)

instance ToResolved ClassDecl where
  type ResolvedTree ClassDecl = R.ClassDecl
  toResolved node =
    case node of
      ClassDecl x1 x2 x3 x4 -> R.ClassDecl (toResolved x1) (toResolved x2) (toResolved x3) (toResolved x4)

instance ToResolved ClassDeclItem where
  type ResolvedTree ClassDeclItem = R.ClassDeclItem
  toResolved node =
    case node of
      ClassItemAnn x1 x2 -> R.ClassItemAnn (toResolved x1) (toResolved x2)
      ClassItemTc x1 x2 -> R.ClassItemAnn (infoAnnotation x1) (toResolved x2)
      ClassItemError x1 x2 -> R.ClassItemError (toResolved x1) (toResolved x2)
      ClassItemTypeSig x1 x2 -> R.ClassItemTypeSig (toResolved x1) (toResolved x2)
      ClassItemDefaultSig x1 x2 -> R.ClassItemDefaultSig (toResolved x1) (toResolved x2)
      ClassItemFixity x1 x2 x3 x4 -> R.ClassItemFixity (toResolved x1) (toResolved x2) (toResolved x3) (toResolved x4)
      ClassItemDefault x1 -> R.ClassItemDefault (toResolved x1)
      ClassItemTypeFamilyDecl x1 -> R.ClassItemTypeFamilyDecl (toResolved x1)
      ClassItemDataFamilyDecl x1 -> R.ClassItemDataFamilyDecl (toResolved x1)
      ClassItemDefaultTypeInst x1 -> R.ClassItemDefaultTypeInst (toResolved x1)
      ClassItemPragma x1 -> R.ClassItemPragma (toResolved x1)

instance ToResolved InstanceDecl where
  type ResolvedTree InstanceDecl = R.InstanceDecl
  toResolved node =
    case node of
      InstanceDecl x1 x2 x3 x4 x5 x6 -> R.InstanceDecl (toResolved x1) (toResolved x2) (toResolved x3) (toResolved x4) (toResolved x5) (toResolved x6)

instance ToResolved InstanceDeclItem where
  type ResolvedTree InstanceDeclItem = R.InstanceDeclItem
  toResolved node =
    case node of
      InstanceItemAnn x1 x2 -> R.InstanceItemAnn (toResolved x1) (toResolved x2)
      InstanceItemTc x1 x2 -> R.InstanceItemAnn (infoAnnotation x1) (toResolved x2)
      InstanceItemError x1 x2 -> R.InstanceItemError (toResolved x1) (toResolved x2)
      InstanceItemBind x1 -> R.InstanceItemBind (toResolved x1)
      InstanceItemTypeSig x1 x2 -> R.InstanceItemTypeSig (toResolved x1) (toResolved x2)
      InstanceItemFixity x1 x2 x3 x4 -> R.InstanceItemFixity (toResolved x1) (toResolved x2) (toResolved x3) (toResolved x4)
      InstanceItemTypeFamilyInst x1 -> R.InstanceItemTypeFamilyInst (toResolved x1)
      InstanceItemDataFamilyInst x1 -> R.InstanceItemDataFamilyInst (toResolved x1)
      InstanceItemPragma x1 -> R.InstanceItemPragma (toResolved x1)

instance ToResolved ForeignDecl where
  type ResolvedTree ForeignDecl = R.ForeignDecl
  toResolved node =
    case node of
      ForeignDecl x1 x2 x3 x4 x5 x6 -> R.ForeignDecl (toResolved x1) (toResolved x2) (toResolved x3) (toResolved x4) (toResolved x5) (toResolved x6)

instance ToResolved Expr where
  type ResolvedTree Expr = R.Expr
  toResolved node =
    case node of
      EAnn x1 x2 -> R.EAnn (toResolved x1) (toResolved x2)
      ETc x1 x2 -> R.EAnn (infoAnnotation x1) (toResolved x2)
      EResolved x1 x2 -> R.EResolved (toResolved x1) (toResolved x2)
      EVar x1 -> R.EVar (toResolved x1)
      EImplicitParam x1 -> R.EImplicitParam (toResolved x1)
      ETypeSyntax x1 x2 -> R.ETypeSyntax (toResolved x1) (toResolved x2)
      EInt x1 x2 x3 -> R.EInt (toResolved x1) (toResolved x2) (toResolved x3)
      EFloat x1 x2 x3 -> R.EFloat (toResolved x1) (toResolved x2) (toResolved x3)
      EChar x1 x2 -> R.EChar (toResolved x1) (toResolved x2)
      ECharHash x1 x2 -> R.ECharHash (toResolved x1) (toResolved x2)
      EString x1 x2 -> R.EString (toResolved x1) (toResolved x2)
      EStringHash x1 x2 -> R.EStringHash (toResolved x1) (toResolved x2)
      EOverloadedLabel x1 x2 -> R.EOverloadedLabel (toResolved x1) (toResolved x2)
      EQuasiQuote x1 x2 -> R.EQuasiQuote (toResolved x1) (toResolved x2)
      EIf x1 x2 x3 -> R.EIf (toResolved x1) (toResolved x2) (toResolved x3)
      EMultiWayIf x1 -> R.EMultiWayIf (toResolved x1)
      ELambdaPats x1 x2 -> R.ELambdaPats (toResolved x1) (toResolved x2)
      ELambdaCase x1 -> R.ELambdaCase (toResolved x1)
      ELambdaCases x1 -> R.ELambdaCases (toResolved x1)
      EInfix x1 x2 x3 -> R.EInfix (toResolved x1) (toResolved x2) (toResolved x3)
      EViewPat x1 x2 -> R.EViewPat (toResolved x1) (toResolved x2)
      ENegate x1 -> R.ENegate (toResolved x1)
      ESectionL x1 x2 -> R.ESectionL (toResolved x1) (toResolved x2)
      ESectionR x1 x2 -> R.ESectionR (toResolved x1) (toResolved x2)
      ELetDecls x1 x2 -> R.ELetDecls (toResolved x1) (toResolved x2)
      ECase x1 x2 -> R.ECase (toResolved x1) (toResolved x2)
      EDo x1 x2 -> R.EDo (toResolved x1) (toResolved x2)
      EListComp x1 x2 -> R.EListComp (toResolved x1) (toResolved x2)
      EListCompParallel x1 x2 -> R.EListCompParallel (toResolved x1) (toResolved x2)
      EArithSeq x1 -> R.EArithSeq (toResolved x1)
      ERecordCon x1 x2 x3 -> R.ERecordCon (toResolved x1) (toResolved x2) (toResolved x3)
      ERecordUpd x1 x2 -> R.ERecordUpd (toResolved x1) (toResolved x2)
      EGetField x1 x2 -> R.EGetField (toResolved x1) (toResolved x2)
      EGetFieldProjection x1 -> R.EGetFieldProjection (toResolved x1)
      ETypeSig x1 x2 -> R.ETypeSig (toResolved x1) (toResolved x2)
      EParen x1 -> R.EParen (toResolved x1)
      EList x1 -> R.EList (toResolved x1)
      ETuple x1 x2 -> R.ETuple (toResolved x1) (toResolved x2)
      EUnboxedSum x1 x2 x3 -> R.EUnboxedSum (toResolved x1) (toResolved x2) (toResolved x3)
      ETypeApp x1 x2 -> R.ETypeApp (toResolved x1) (toResolved x2)
      EApp x1 x2 -> R.EApp (toResolved x1) (toResolved x2)
      ETHExpQuote x1 -> R.ETHExpQuote (toResolved x1)
      ETHTypedQuote x1 -> R.ETHTypedQuote (toResolved x1)
      ETHDeclQuote x1 -> R.ETHDeclQuote (toResolved x1)
      ETHTypeQuote x1 -> R.ETHTypeQuote (toResolved x1)
      ETHPatQuote x1 -> R.ETHPatQuote (toResolved x1)
      ETHNameQuote x1 -> R.ETHNameQuote (toResolved x1)
      ETHTypeNameQuote x1 -> R.ETHTypeNameQuote (toResolved x1)
      ETHSplice x1 -> R.ETHSplice (toResolved x1)
      ETHTypedSplice x1 -> R.ETHTypedSplice (toResolved x1)
      EProc x1 x2 -> R.EProc (toResolved x1) (toResolved x2)
      EPragma x1 x2 -> R.EPragma (toResolved x1) (toResolved x2)

instance (ToResolved body) => ToResolved (CaseAlt body) where
  type ResolvedTree (CaseAlt body) = R.CaseAlt (ResolvedTree body)
  toResolved node =
    case node of
      CaseAlt x1 x2 x3 -> R.CaseAlt (toResolved x1) (toResolved x2) (toResolved x3)

instance ToResolved LambdaCaseAlt where
  type ResolvedTree LambdaCaseAlt = R.LambdaCaseAlt
  toResolved node =
    case node of
      LambdaCaseAlt x1 x2 x3 -> R.LambdaCaseAlt (toResolved x1) (toResolved x2) (toResolved x3)

instance (ToResolved body) => ToResolved (DoStmt body) where
  type ResolvedTree (DoStmt body) = R.DoStmt (ResolvedTree body)
  toResolved node =
    case node of
      DoAnn x1 x2 -> R.DoAnn (toResolved x1) (toResolved x2)
      DoTc x1 x2 -> R.DoAnn (infoAnnotation x1) (toResolved x2)
      DoResolved x1 x2 -> R.DoResolved (toResolved x1) (toResolved x2)
      DoRecKnot x1 x2 -> R.DoRecKnot (toResolved x1) (toResolved x2)
      DoBind x1 x2 -> R.DoBind (toResolved x1) (toResolved x2)
      DoLetDecls x1 -> R.DoLetDecls (toResolved x1)
      DoExpr x1 -> R.DoExpr (toResolved x1)
      DoRecStmt x1 -> R.DoRecStmt (toResolved x1)

instance ToResolved Cmd where
  type ResolvedTree Cmd = R.Cmd
  toResolved node =
    case node of
      CmdAnn x1 x2 -> R.CmdAnn (toResolved x1) (toResolved x2)
      CmdArrApp x1 x2 x3 -> R.CmdArrApp (toResolved x1) (toResolved x2) (toResolved x3)
      CmdInfix x1 x2 x3 -> R.CmdInfix (toResolved x1) (toResolved x2) (toResolved x3)
      CmdDo x1 -> R.CmdDo (toResolved x1)
      CmdIf x1 x2 x3 -> R.CmdIf (toResolved x1) (toResolved x2) (toResolved x3)
      CmdCase x1 x2 -> R.CmdCase (toResolved x1) (toResolved x2)
      CmdLet x1 x2 -> R.CmdLet (toResolved x1) (toResolved x2)
      CmdLam x1 x2 -> R.CmdLam (toResolved x1) (toResolved x2)
      CmdApp x1 x2 -> R.CmdApp (toResolved x1) (toResolved x2)
      CmdPar x1 -> R.CmdPar (toResolved x1)

instance ToResolved CompStmt where
  type ResolvedTree CompStmt = R.CompStmt
  toResolved node =
    case node of
      CompAnn x1 x2 -> R.CompAnn (toResolved x1) (toResolved x2)
      CompTc x1 x2 -> R.CompAnn (infoAnnotation x1) (toResolved x2)
      CompGen x1 x2 -> R.CompGen (toResolved x1) (toResolved x2)
      CompGuard x1 -> R.CompGuard (toResolved x1)
      CompLetDecls x1 -> R.CompLetDecls (toResolved x1)
      CompThen x1 -> R.CompThen (toResolved x1)
      CompThenBy x1 x2 -> R.CompThenBy (toResolved x1) (toResolved x2)
      CompGroupUsing x1 -> R.CompGroupUsing (toResolved x1)
      CompGroupByUsing x1 x2 -> R.CompGroupByUsing (toResolved x1) (toResolved x2)

instance ToResolved ArithSeq where
  type ResolvedTree ArithSeq = R.ArithSeq
  toResolved node =
    case node of
      ArithSeqAnn x1 x2 -> R.ArithSeqAnn (toResolved x1) (toResolved x2)
      ArithSeqTc x1 x2 -> R.ArithSeqAnn (infoAnnotation x1) (toResolved x2)
      ArithSeqResolved x1 x2 -> R.ArithSeqResolved (toResolved x1) (toResolved x2)
      ArithSeqFrom x1 -> R.ArithSeqFrom (toResolved x1)
      ArithSeqFromThen x1 x2 -> R.ArithSeqFromThen (toResolved x1) (toResolved x2)
      ArithSeqFromTo x1 x2 -> R.ArithSeqFromTo (toResolved x1) (toResolved x2)
      ArithSeqFromThenTo x1 x2 x3 -> R.ArithSeqFromThenTo (toResolved x1) (toResolved x2) (toResolved x3)
