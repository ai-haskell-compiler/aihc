{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- | Change the tree that the type checker works on into the tree that it
-- gives. A function changes each item of pending type checker information
-- into final information, or removes the item.
--
-- The change visits the nodes in source order: the fields of a
-- constructor from left to right, and the information of a wrapper before
-- the node in the wrapper.
--
-- Each instance has one case for each constructor of the data declaration.
-- When a constructor changes in one tree, change its case here too.
module Aihc.Tc.Syntax.FromPending
  ( FromPending (..),
    Finalizers (..),
    plainFinalizers,
  )
where

import Aihc.Tc.Annotations (PendingTcAnnotation, PendingTcCastAnnotation, PendingTcMatchArityCast, TcAnnotation, TcCastAnnotation, TcClassAnnotation, TcDerivingPlan, TcInstanceAnnotation, TcInstanceMethodAnnotation, TcMatchArityCast, TcRecStmt (..), WorkingTcAnnotation (..))
import Aihc.Tc.Types (TcType)
import Aihc.Tc.Env (DataFamilyInstanceInfo, TypeFamilyInstanceInfo)
import Aihc.Tc.Syntax
import Aihc.Tc.Syntax.Pending qualified as Pending
import Control.Monad (join)
import Data.Maybe (catMaybes)
import Data.Text (Text)

-- | Wrap a node with its information, if the information stays.
wrapInfo :: (item -> node -> node) -> Maybe item -> node -> node
wrapInfo wrap item node = maybe node (`wrap` node) item

-- | The functions that change pending information into final information.
-- Each kind of information has one function.
data Finalizers m = Finalizers
  { finalizeTypeFamilyInstance :: TypeFamilyInstanceInfo -> m TypeFamilyInstanceInfo,
    finalizeDataFamilyInstance :: DataFamilyInstanceInfo -> m DataFamilyInstanceInfo,
    -- | Change an annotation, or remove it.
    finalizeAnnotation :: PendingTcAnnotation -> m (Maybe TcAnnotation),
    -- | Change an annotation that the checker writes as final.
    finalizeCheckedAnnotation :: TcAnnotation -> m TcAnnotation,
    -- | Change a cast, or remove it.
    finalizeCast :: PendingTcCastAnnotation -> m (Maybe TcCastAnnotation),
    -- | Change a type that can have meta variables.
    finalizePendingType :: TcType -> m TcType,
    -- | Change a type that the checker writes as final.
    finalizeCheckedType :: TcType -> m TcType,
    -- | Change the arity cast of an equation, or remove it.
    finalizeMatchArityCast :: PendingTcMatchArityCast -> m (Maybe TcMatchArityCast),
    finalizeClass :: TcClassAnnotation -> m TcClassAnnotation,
    finalizeDerivingPlan :: TcDerivingPlan -> m TcDerivingPlan,
    finalizeInstance :: TcInstanceAnnotation -> m TcInstanceAnnotation,
    finalizeInstanceMethod :: TcInstanceMethodAnnotation -> m TcInstanceMethodAnnotation
  }

-- | The functions that remove each annotation and each cast, and keep all
-- other information as it is.
plainFinalizers :: (Applicative m) => Finalizers m
plainFinalizers =
  Finalizers
    { finalizeTypeFamilyInstance = pure,
      finalizeDataFamilyInstance = pure,
      finalizeAnnotation = const (pure Nothing),
      finalizeCheckedAnnotation = pure,
      finalizeCast = const (pure Nothing),
      finalizePendingType = pure,
      finalizeCheckedType = pure,
      finalizeMatchArityCast = const (pure Nothing),
      finalizeClass = pure,
      finalizeDerivingPlan = pure,
      finalizeInstance = pure,
      finalizeInstanceMethod = pure
    }

-- | Change a list of casts, and remove the casts that the function removes.
casts :: (Monad m) => Finalizers m -> [PendingTcCastAnnotation] -> m [TcCastAnnotation]
casts info items = catMaybes <$> traverse (finalizeCast info) items

-- | Change the annotation of a node, if it has one and the function keeps
-- it.
annotation :: (Monad m) => Finalizers m -> Maybe PendingTcAnnotation -> m (Maybe TcAnnotation)
annotation info item = join <$> traverse (finalizeAnnotation info) item

-- | Change the annotations of a @rec@ statement. The statement keeps its
-- information only when the function keeps the tuple annotation. A knot
-- occurrence goes away when the function removes its annotation.
recStmt :: (Monad m) => Finalizers m -> TcRecStmt PendingTcAnnotation -> m (Maybe (TcRecStmt TcAnnotation))
recStmt info (TcRecStmt tuple occurrences) = do
  tuple' <- finalizeAnnotation info tuple
  occurrences' <- traverse (\(key, occurrence) -> fmap (key,) <$> finalizeAnnotation info occurrence) occurrences
  pure ((`TcRecStmt` catMaybes occurrences') <$> tuple')

-- | Change the information of an expression wrapper, if the function
-- keeps it.
exprTc :: (Monad m) => Finalizers m -> Pending.ExprTc -> m (Maybe ExprTc)
exprTc info item =
  case item of
    Pending.ExprType pending -> fmap ExprType <$> finalizeAnnotation info pending
    Pending.ExprCast pending -> fmap ExprCast <$> finalizeCast info pending

-- | Change the information of a pattern wrapper, if the function keeps
-- it.
patternTc :: (Monad m) => Finalizers m -> Pending.PatternTc -> m (Maybe PatternTc)
patternTc info item =
  case item of
    Pending.PatternType pending -> fmap PatternType <$> finalizeAnnotation info pending
    Pending.PatternCast pending -> fmap PatternCast <$> finalizeCast info pending
    Pending.PatternInstantiation -> pure (Just PatternInstantiation)

-- | Change the annotation of a declaration wrapper, if the function keeps
-- it.
declTc :: (Monad m) => Finalizers m -> WorkingTcAnnotation -> m (Maybe TcAnnotation)
declTc info item =
  case item of
    WorkingPending pending -> finalizeAnnotation info pending
    WorkingChecked checked -> Just <$> finalizeCheckedAnnotation info checked

class FromPending a where
  type Final a
  fromPending :: (Monad m) => Finalizers m -> a -> m (Final a)

instance (FromPending a) => FromPending [a] where
  type Final [a] = [Final a]
  fromPending info = traverse (fromPending info)

instance (FromPending a) => FromPending (Maybe a) where
  type Final (Maybe a) = Maybe (Final a)
  fromPending info = traverse (fromPending info)

instance (FromPending a, FromPending b) => FromPending (a, b) where
  type Final (a, b) = (Final a, Final b)
  fromPending info (left, right) = (,) <$> fromPending info left <*> fromPending info right

instance (FromPending a, FromPending b) => FromPending (Either a b) where
  type Final (Either a b) = Either (Final a) (Final b)
  fromPending info = either (fmap Left . fromPending info) (fmap Right . fromPending info)

instance FromPending Annotation where
  type Final Annotation = Annotation
  fromPending _ = pure

instance FromPending ArrAppType where
  type Final ArrAppType = ArrAppType
  fromPending _ = pure

instance FromPending Bool where
  type Final Bool = Bool
  fromPending _ = pure

instance FromPending BuiltinCon where
  type Final BuiltinCon = BuiltinCon
  fromPending _ = pure

instance FromPending CallConv where
  type Final CallConv = CallConv
  fromPending _ = pure

instance FromPending Char where
  type Final Char = Char
  fromPending _ = pure

instance FromPending DoFlavor where
  type Final DoFlavor = DoFlavor
  fromPending _ = pure

instance FromPending ExtensionSetting where
  type Final ExtensionSetting = ExtensionSetting
  fromPending _ = pure

instance FromPending FixityAssoc where
  type Final FixityAssoc = FixityAssoc
  fromPending _ = pure

instance FromPending FloatType where
  type Final FloatType = FloatType
  fromPending _ = pure

instance FromPending ForallVis where
  type Final ForallVis = ForallVis
  fromPending _ = pure

instance FromPending ForeignDirection where
  type Final ForeignDirection = ForeignDirection
  fromPending _ = pure

instance FromPending ForeignEntitySpec where
  type Final ForeignEntitySpec = ForeignEntitySpec
  fromPending _ = pure

instance FromPending ForeignSafety where
  type Final ForeignSafety = ForeignSafety
  fromPending _ = pure

instance FromPending FunctionalDependency where
  type Final FunctionalDependency = FunctionalDependency
  fromPending _ = pure

instance FromPending IEBundledNamespace where
  type Final IEBundledNamespace = IEBundledNamespace
  fromPending _ = pure

instance FromPending IEEntityNamespace where
  type Final IEEntityNamespace = IEEntityNamespace
  fromPending _ = pure

instance FromPending ImportLevel where
  type Final ImportLevel = ImportLevel
  fromPending _ = pure

instance FromPending Int where
  type Final Int = Int
  fromPending _ = pure

instance FromPending Integer where
  type Final Integer = Integer
  fromPending _ = pure

instance FromPending MatchHeadForm where
  type Final MatchHeadForm = MatchHeadForm
  fromPending _ = pure

instance FromPending NameType where
  type Final NameType = NameType
  fromPending _ = pure

instance FromPending NumericType where
  type Final NumericType = NumericType
  fromPending _ = pure

instance FromPending PatSynArgs where
  type Final PatSynArgs = PatSynArgs
  fromPending _ = pure

instance FromPending Pragma where
  type Final Pragma = Pragma
  fromPending _ = pure

instance FromPending Rational where
  type Final Rational = Rational
  fromPending _ = pure

instance FromPending RecKnotVariables where
  type Final RecKnotVariables = RecKnotVariables
  fromPending _ = pure

instance FromPending ResolutionResult where
  type Final ResolutionResult = ResolutionResult
  fromPending _ = pure

instance FromPending ResolveError where
  type Final ResolveError = ResolveError
  fromPending _ = pure

instance FromPending Role where
  type Final Role = Role
  fromPending _ = pure

instance FromPending RuleActivation where
  type Final RuleActivation = RuleActivation
  fromPending _ = pure

instance FromPending Text where
  type Final Text = Text
  fromPending _ = pure

instance FromPending TupleFlavor where
  type Final TupleFlavor = TupleFlavor
  fromPending _ = pure

instance FromPending TyVarBSpecificity where
  type Final TyVarBSpecificity = TyVarBSpecificity
  fromPending _ = pure

instance FromPending TyVarBVisibility where
  type Final TyVarBVisibility = TyVarBVisibility
  fromPending _ = pure

instance FromPending TypeFamilyInjectivity where
  type Final TypeFamilyInjectivity = TypeFamilyInjectivity
  fromPending _ = pure

instance FromPending TypeHeadForm where
  type Final TypeHeadForm = TypeHeadForm
  fromPending _ = pure

instance FromPending TypeLiteral where
  type Final TypeLiteral = TypeLiteral
  fromPending _ = pure

instance FromPending TypePromotion where
  type Final TypePromotion = TypePromotion
  fromPending _ = pure

instance FromPending TypeSyntaxForm where
  type Final TypeSyntaxForm = TypeSyntaxForm
  fromPending _ = pure

instance FromPending Pending.PatSynEquations where
  type Final Pending.PatSynEquations = PatSynEquations
  fromPending info node =
    case node of
      Pending.PatSynEquations x1 x2 x3 -> PatSynEquations <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3

instance FromPending Pending.Literal where
  type Final Pending.Literal = Literal
  fromPending info node =
    case node of
      Pending.LitAnn x1 x2 -> LitAnn <$> fromPending info x1 <*> fromPending info x2
      Pending.LitTc x1 x2 -> wrapInfo LitTc <$> finalizeAnnotation info x1 <*> fromPending info x2
      Pending.LitInt x1 x2 x3 -> LitInt <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3
      Pending.LitFloat x1 x2 x3 -> LitFloat <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3
      Pending.LitChar x1 x2 -> LitChar <$> fromPending info x1 <*> fromPending info x2
      Pending.LitCharHash x1 x2 -> LitCharHash <$> fromPending info x1 <*> fromPending info x2
      Pending.LitString x1 x2 -> LitString <$> fromPending info x1 <*> fromPending info x2
      Pending.LitStringHash x1 x2 -> LitStringHash <$> fromPending info x1 <*> fromPending info x2

instance FromPending Pending.Name where
  type Final Pending.Name = Name
  fromPending info node =
    case node of
      Pending.Name x1 x2 x3 x4 x5 x6 x7 -> Name <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4 <*> fromPending info x5 <*> (join <$> traverse (declTc info) x6) <*> (join <$> traverse (finalizeCast info) x7)

instance FromPending Pending.UnqualifiedName where
  type Final Pending.UnqualifiedName = UnqualifiedName
  fromPending info node =
    case node of
      Pending.UnqualifiedName x1 x2 x3 x4 x5 -> UnqualifiedName <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4 <*> (join <$> traverse (declTc info) x5)

instance FromPending Pending.Module where
  type Final Pending.Module = Module
  fromPending info node =
    case node of
      Pending.Module x1 x2 x3 x4 x5 x6 -> Module <$> fromPending info x1 <*> pure x2 <*> fromPending info x3 <*> fromPending info x4 <*> fromPending info x5 <*> fromPending info x6 <*> pure []

instance FromPending Pending.ModuleHead where
  type Final Pending.ModuleHead = ModuleHead
  fromPending info node =
    case node of
      Pending.ModuleHead x1 x2 x3 x4 -> ModuleHead <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4

instance FromPending Pending.IEBundledMember where
  type Final Pending.IEBundledMember = IEBundledMember
  fromPending info node =
    case node of
      Pending.IEBundledMember x1 x2 -> IEBundledMember <$> fromPending info x1 <*> fromPending info x2

instance FromPending Pending.ExportSpec where
  type Final Pending.ExportSpec = ExportSpec
  fromPending info node =
    case node of
      Pending.ExportModule x1 x2 -> ExportModule <$> fromPending info x1 <*> fromPending info x2
      Pending.ExportVar x1 x2 x3 -> ExportVar <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3
      Pending.ExportAbs x1 x2 x3 -> ExportAbs <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3
      Pending.ExportAll x1 x2 x3 -> ExportAll <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3
      Pending.ExportWith x1 x2 x3 x4 -> ExportWith <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4
      Pending.ExportWithAll x1 x2 x3 x4 x5 -> ExportWithAll <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4 <*> fromPending info x5
      Pending.ExportAnn x1 x2 -> ExportAnn <$> fromPending info x1 <*> fromPending info x2

instance FromPending Pending.ImportDecl where
  type Final Pending.ImportDecl = ImportDecl
  fromPending info node =
    case node of
      Pending.ImportDecl x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 -> ImportDecl <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4 <*> fromPending info x5 <*> fromPending info x6 <*> fromPending info x7 <*> fromPending info x8 <*> fromPending info x9 <*> fromPending info x10 <*> fromPending info x11

instance FromPending Pending.ImportSpec where
  type Final Pending.ImportSpec = ImportSpec
  fromPending info node =
    case node of
      Pending.ImportSpec x1 x2 x3 -> ImportSpec <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3

instance FromPending Pending.ImportItem where
  type Final Pending.ImportItem = ImportItem
  fromPending info node =
    case node of
      Pending.ImportItemVar x1 x2 -> ImportItemVar <$> fromPending info x1 <*> fromPending info x2
      Pending.ImportItemAbs x1 x2 -> ImportItemAbs <$> fromPending info x1 <*> fromPending info x2
      Pending.ImportItemAll x1 x2 -> ImportItemAll <$> fromPending info x1 <*> fromPending info x2
      Pending.ImportItemWith x1 x2 x3 -> ImportItemWith <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3
      Pending.ImportItemAllWith x1 x2 x3 x4 -> ImportItemAllWith <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4
      Pending.ImportAnn x1 x2 -> ImportAnn <$> fromPending info x1 <*> fromPending info x2
      Pending.ImportItemError x1 x2 -> ImportItemError <$> fromPending info x1 <*> fromPending info x2

instance FromPending Pending.Decl where
  type Final Pending.Decl = Decl
  fromPending info node =
    case node of
      Pending.DeclAnn x1 x2 -> DeclAnn <$> fromPending info x1 <*> fromPending info x2
      Pending.DeclTc x1 x2 -> wrapInfo DeclTc <$> declTc info x1 <*> fromPending info x2
      Pending.DeclError x1 x2 -> DeclError <$> fromPending info x1 <*> fromPending info x2
      Pending.DeclValue x1 -> DeclValue <$> fromPending info x1
      Pending.DeclImplicitParam x1 x2 x3 -> DeclImplicitParam <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3
      Pending.DeclTypeSig x1 x2 -> DeclTypeSig <$> fromPending info x1 <*> fromPending info x2
      Pending.DeclPatSyn x1 -> DeclPatSyn <$> fromPending info x1
      Pending.DeclPatSynSig x1 x2 -> DeclPatSynSig <$> fromPending info x1 <*> fromPending info x2
      Pending.DeclStandaloneKindSig x1 x2 -> DeclStandaloneKindSig <$> fromPending info x1 <*> fromPending info x2
      Pending.DeclFixity x1 x2 x3 x4 -> DeclFixity <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4
      Pending.DeclRoleAnnotation x1 -> DeclRoleAnnotation <$> fromPending info x1
      Pending.DeclTypeSyn x1 -> DeclTypeSyn <$> fromPending info x1
      Pending.DeclTypeData x1 -> DeclTypeData <$> fromPending info x1
      Pending.DeclData x1 -> DeclData <$> fromPending info x1
      Pending.DeclNewtype x1 -> DeclNewtype <$> fromPending info x1
      Pending.DeclClass x1 -> DeclClass <$> fromPending info x1
      Pending.DeclInstance x1 -> DeclInstance <$> fromPending info x1
      Pending.DeclStandaloneDeriving x1 -> DeclStandaloneDeriving <$> fromPending info x1
      Pending.DeclDefault x1 -> DeclDefault <$> fromPending info x1
      Pending.DeclSplice x1 -> DeclSplice <$> fromPending info x1
      Pending.DeclForeign x1 -> DeclForeign <$> fromPending info x1
      Pending.DeclTypeFamilyDecl x1 -> DeclTypeFamilyDecl <$> fromPending info x1
      Pending.DeclDataFamilyDecl x1 -> DeclDataFamilyDecl <$> fromPending info x1
      Pending.DeclTypeFamilyInst x1 -> DeclTypeFamilyInst <$> fromPending info x1
      Pending.DeclDataFamilyInst x1 -> DeclDataFamilyInst <$> fromPending info x1
      Pending.DeclPragma x1 -> DeclPragma <$> fromPending info x1
      Pending.DeclRules x1 -> DeclRules <$> fromPending info x1

instance FromPending Pending.RuleDecl where
  type Final Pending.RuleDecl = RuleDecl
  fromPending info node =
    case node of
      Pending.RuleDecl x1 x2 x3 x4 x5 x6 x7 x8 x9 -> RuleDecl <$> fromPending info x1 <*> traverse (finalizePendingType info) x2 <*> fromPending info x3 <*> fromPending info x4 <*> fromPending info x5 <*> fromPending info x6 <*> fromPending info x7 <*> fromPending info x8 <*> fromPending info x9

instance FromPending Pending.RuleBinder where
  type Final Pending.RuleBinder = RuleBinder
  fromPending info node =
    case node of
      Pending.RuleBinder x1 x2 x3 x4 -> RuleBinder <$> fromPending info x1 <*> traverse (finalizePendingType info) x2 <*> fromPending info x3 <*> fromPending info x4

instance FromPending Pending.MultiplicityTag where
  type Final Pending.MultiplicityTag = MultiplicityTag
  fromPending info node =
    case node of
      Pending.NoMultiplicityTag -> pure NoMultiplicityTag
      Pending.LinearMultiplicityTag -> pure LinearMultiplicityTag
      Pending.ExplicitMultiplicityTag x1 -> ExplicitMultiplicityTag <$> fromPending info x1

instance FromPending Pending.ValueDecl where
  type Final Pending.ValueDecl = ValueDecl
  fromPending info node =
    case node of
      Pending.FunctionBind x1 x2 -> FunctionBind <$> fromPending info x1 <*> fromPending info x2
      Pending.PatternBind x1 x2 x3 -> PatternBind <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3

instance FromPending Pending.Match where
  type Final Pending.Match = Match
  fromPending info node =
    case node of
      Pending.Match x1 x2 x3 x4 x5 -> Match <$> fromPending info x1 <*> (join <$> traverse (finalizeMatchArityCast info) x2) <*> fromPending info x3 <*> fromPending info x4 <*> fromPending info x5

instance FromPending Pending.PatSynDir where
  type Final Pending.PatSynDir = PatSynDir
  fromPending info node =
    case node of
      Pending.PatSynUnidirectional -> pure PatSynUnidirectional
      Pending.PatSynBidirectional -> pure PatSynBidirectional
      Pending.PatSynExplicitBidirectional x1 -> PatSynExplicitBidirectional <$> fromPending info x1

instance FromPending Pending.PatSynDecl where
  type Final Pending.PatSynDecl = PatSynDecl
  fromPending info node =
    case node of
      Pending.PatSynDecl x1 x2 x3 x4 x5 -> PatSynDecl <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4 <*> fromPending info x5

instance (FromPending body) => FromPending (Pending.Rhs body) where
  type Final (Pending.Rhs body) = Rhs (Final body)
  fromPending info node =
    case node of
      Pending.UnguardedRhs x1 x2 x3 x4 -> UnguardedRhs <$> fromPending info x1 <*> casts info x2 <*> fromPending info x3 <*> fromPending info x4
      Pending.GuardedRhss x1 x2 x3 x4 -> GuardedRhss <$> fromPending info x1 <*> casts info x2 <*> fromPending info x3 <*> fromPending info x4

instance (FromPending body) => FromPending (Pending.GuardedRhs body) where
  type Final (Pending.GuardedRhs body) = GuardedRhs (Final body)
  fromPending info node =
    case node of
      Pending.GuardedRhs x1 x2 x3 -> GuardedRhs <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3

instance FromPending Pending.GuardQualifier where
  type Final Pending.GuardQualifier = GuardQualifier
  fromPending info node =
    case node of
      Pending.GuardAnn x1 x2 -> GuardAnn <$> fromPending info x1 <*> fromPending info x2
      Pending.GuardExpr x1 -> GuardExpr <$> fromPending info x1
      Pending.GuardPat x1 x2 -> GuardPat <$> fromPending info x1 <*> fromPending info x2
      Pending.GuardLet x1 -> GuardLet <$> fromPending info x1

instance (FromPending a) => FromPending (Pending.RecordField a) where
  type Final (Pending.RecordField a) = RecordField (Final a)
  fromPending info node =
    case node of
      Pending.RecordField x1 x2 x3 -> RecordField <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3

instance FromPending Pending.LiteralMatch where
  type Final Pending.LiteralMatch = LiteralMatch
  fromPending info node =
    case node of
      Pending.LiteralMatchFixed -> pure LiteralMatchFixed
      Pending.LiteralMatchPrimitive x1 -> LiteralMatchPrimitive <$> fromPending info x1
      Pending.LiteralMatchOverloaded x1 -> LiteralMatchOverloaded <$> fromPending info x1

instance FromPending Pending.OverloadedLiteralMatch where
  type Final Pending.OverloadedLiteralMatch = OverloadedLiteralMatch
  fromPending info (Pending.OverloadedLiteralMatch x1 x2 x3 x4) =
    OverloadedLiteralMatch <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4

instance FromPending Pending.NPlusKMatch where
  type Final Pending.NPlusKMatch = NPlusKMatch
  fromPending info (Pending.NPlusKMatch x1 x2 x3 x4) =
    NPlusKMatch <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4

instance FromPending Pending.LiteralSyntax where
  type Final Pending.LiteralSyntax = LiteralSyntax
  fromPending info syntax =
    case syntax of
      Pending.LiteralSyntaxPrimitive x1 -> LiteralSyntaxPrimitive <$> fromPending info x1
      Pending.LiteralSyntaxOverloaded x1 x2 -> LiteralSyntaxOverloaded <$> fromPending info x1 <*> fromPending info x2

instance FromPending Pending.DoBindMethods where
  type Final Pending.DoBindMethods = DoBindMethods
  fromPending info (Pending.DoBindMethods x1 x2) = DoBindMethods <$> fromPending info x1 <*> fromPending info x2

instance FromPending Pending.DoRecMethods where
  type Final Pending.DoRecMethods = DoRecMethods
  fromPending info (Pending.DoRecMethods x1 x2 x3) = DoRecMethods <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3

instance FromPending Pending.ListMatch where
  type Final Pending.ListMatch = ListMatch
  fromPending info (Pending.ListMatch x1 x2) = ListMatch <$> fromPending info x1 <*> fromPending info x2

instance FromPending Pending.Pattern where
  type Final Pending.Pattern = Pattern
  fromPending info node =
    case node of
      Pending.PAnn x1 x2 -> PAnn <$> fromPending info x1 <*> fromPending info x2
      Pending.PTc x1 x2 -> wrapInfo PTc <$> patternTc info x1 <*> fromPending info x2
      Pending.PVar x1 -> PVar <$> fromPending info x1
      Pending.PTypeBinder x1 -> PTypeBinder <$> fromPending info x1
      Pending.PTypeSyntax x1 x2 -> PTypeSyntax <$> fromPending info x1 <*> fromPending info x2
      Pending.PWildcard -> pure PWildcard
      Pending.PLit x1 x2 -> PLit <$> fromPending info x1 <*> fromPending info x2
      Pending.PQuasiQuote x1 x2 -> PQuasiQuote <$> fromPending info x1 <*> fromPending info x2
      Pending.PTuple x1 x2 -> PTuple <$> fromPending info x1 <*> fromPending info x2
      Pending.PUnboxedSum x1 x2 x3 -> PUnboxedSum <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3
      Pending.PList x1 x2 -> PList <$> fromPending info x1 <*> fromPending info x2
      Pending.PCon x1 x2 x3 -> PCon <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3
      Pending.PBuiltinCon x1 x2 x3 -> PBuiltinCon <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3
      Pending.PInfix x1 x2 x3 -> PInfix <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3
      Pending.PView x1 x2 -> PView <$> fromPending info x1 <*> fromPending info x2
      Pending.PAs x1 x2 -> PAs <$> fromPending info x1 <*> fromPending info x2
      Pending.PStrict x1 -> PStrict <$> fromPending info x1
      Pending.PIrrefutable x1 -> PIrrefutable <$> fromPending info x1
      Pending.PNegLit x1 x2 -> PNegLit <$> fromPending info x1 <*> fromPending info x2
      Pending.PNPlusK x1 x2 x3 -> PNPlusK <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3
      Pending.PParen x1 -> PParen <$> fromPending info x1
      Pending.PRecord x1 x2 x3 -> PRecord <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3
      Pending.PTypeSig x1 x2 -> PTypeSig <$> fromPending info x1 <*> fromPending info x2
      Pending.PSplice x1 -> PSplice <$> fromPending info x1

instance FromPending Pending.ForallTelescope where
  type Final Pending.ForallTelescope = ForallTelescope
  fromPending info node =
    case node of
      Pending.ForallTelescope x1 x2 -> ForallTelescope <$> fromPending info x1 <*> fromPending info x2

instance FromPending Pending.ArrowKind where
  type Final Pending.ArrowKind = ArrowKind
  fromPending info node =
    case node of
      Pending.ArrowUnrestricted -> pure ArrowUnrestricted
      Pending.ArrowLinear -> pure ArrowLinear
      Pending.ArrowExplicit x1 -> ArrowExplicit <$> fromPending info x1

instance FromPending Pending.Type where
  type Final Pending.Type = Type
  fromPending info node =
    case node of
      Pending.TAnn x1 x2 -> TAnn <$> fromPending info x1 <*> fromPending info x2
      Pending.TVar x1 -> TVar <$> fromPending info x1
      Pending.TCon x1 x2 -> TCon <$> fromPending info x1 <*> fromPending info x2
      Pending.TBuiltinCon x1 x2 -> TBuiltinCon <$> fromPending info x1 <*> fromPending info x2
      Pending.TImplicitParam x1 x2 -> TImplicitParam <$> fromPending info x1 <*> fromPending info x2
      Pending.TTypeLit x1 -> TTypeLit <$> fromPending info x1
      Pending.TStar x1 -> TStar <$> fromPending info x1
      Pending.TQuasiQuote x1 x2 -> TQuasiQuote <$> fromPending info x1 <*> fromPending info x2
      Pending.TForall x1 x2 -> TForall <$> fromPending info x1 <*> fromPending info x2
      Pending.TApp x1 x2 -> TApp <$> fromPending info x1 <*> fromPending info x2
      Pending.TTypeApp x1 x2 -> TTypeApp <$> fromPending info x1 <*> fromPending info x2
      Pending.TInfix x1 x2 x3 x4 -> TInfix <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4
      Pending.TFun x1 x2 x3 -> TFun <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3
      Pending.TTuple x1 x2 x3 x4 -> TTuple <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4
      Pending.TUnboxedSum x1 -> TUnboxedSum <$> fromPending info x1
      Pending.TList x1 x2 x3 -> TList <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3
      Pending.TParen x1 -> TParen <$> fromPending info x1
      Pending.TKindSig x1 x2 -> TKindSig <$> fromPending info x1 <*> fromPending info x2
      Pending.TContext x1 x2 -> TContext <$> fromPending info x1 <*> fromPending info x2
      Pending.TSplice x1 -> TSplice <$> fromPending info x1
      Pending.TWildcard -> pure TWildcard

instance FromPending Pending.TyVarBinder where
  type Final Pending.TyVarBinder = TyVarBinder
  fromPending info node =
    case node of
      Pending.TyVarBinder x1 x2 x3 x4 x5 -> TyVarBinder <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4 <*> fromPending info x5

instance (FromPending name) => FromPending (Pending.BinderHead name) where
  type Final (Pending.BinderHead name) = BinderHead (Final name)
  fromPending info node =
    case node of
      Pending.PrefixBinderHead x1 x2 -> PrefixBinderHead <$> fromPending info x1 <*> fromPending info x2
      Pending.InfixBinderHead x1 x2 x3 x4 -> InfixBinderHead <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4

instance FromPending Pending.RoleAnnotation where
  type Final Pending.RoleAnnotation = RoleAnnotation
  fromPending info node =
    case node of
      Pending.RoleAnnotation x1 x2 -> RoleAnnotation <$> fromPending info x1 <*> fromPending info x2

instance FromPending Pending.TypeSynDecl where
  type Final Pending.TypeSynDecl = TypeSynDecl
  fromPending info node =
    case node of
      Pending.TypeSynDecl x1 x2 -> TypeSynDecl <$> fromPending info x1 <*> fromPending info x2

instance FromPending Pending.TypeFamilyDecl where
  type Final Pending.TypeFamilyDecl = TypeFamilyDecl
  fromPending info node =
    case node of
      Pending.TypeFamilyDecl x1 x2 x3 x4 x5 x6 -> TypeFamilyDecl <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4 <*> fromPending info x5 <*> fromPending info x6

instance FromPending Pending.TypeFamilyResultSig where
  type Final Pending.TypeFamilyResultSig = TypeFamilyResultSig
  fromPending info node =
    case node of
      Pending.TypeFamilyKindSig x1 -> TypeFamilyKindSig <$> fromPending info x1
      Pending.TypeFamilyTyVarSig x1 -> TypeFamilyTyVarSig <$> fromPending info x1
      Pending.TypeFamilyInjectiveSig x1 x2 -> TypeFamilyInjectiveSig <$> fromPending info x1 <*> fromPending info x2

instance FromPending Pending.TypeFamilyEq where
  type Final Pending.TypeFamilyEq = TypeFamilyEq
  fromPending info node =
    case node of
      Pending.TypeFamilyEq x1 x2 x3 x4 x5 -> TypeFamilyEq <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4 <*> fromPending info x5

instance FromPending Pending.DataFamilyDecl where
  type Final Pending.DataFamilyDecl = DataFamilyDecl
  fromPending info node =
    case node of
      Pending.DataFamilyDecl x1 x2 -> DataFamilyDecl <$> fromPending info x1 <*> fromPending info x2

instance FromPending Pending.TypeFamilyInst where
  type Final Pending.TypeFamilyInst = TypeFamilyInst
  fromPending info node =
    case node of
      Pending.TypeFamilyInst x1 x2 x3 x4 x5 -> TypeFamilyInst <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4 <*> traverse (finalizeTypeFamilyInstance info) x5

instance FromPending Pending.DataFamilyInst where
  type Final Pending.DataFamilyInst = DataFamilyInst
  fromPending info node =
    case node of
      Pending.DataFamilyInst x1 x2 x3 x4 x5 x6 x7 -> DataFamilyInst <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4 <*> fromPending info x5 <*> fromPending info x6 <*> traverse (finalizeDataFamilyInstance info) x7

instance FromPending Pending.DataDecl where
  type Final Pending.DataDecl = DataDecl
  fromPending info node =
    case node of
      Pending.DataDecl x1 x2 x3 x4 x5 x6 -> DataDecl <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4 <*> fromPending info x5 <*> fromPending info x6

instance FromPending Pending.NewtypeDecl where
  type Final Pending.NewtypeDecl = NewtypeDecl
  fromPending info node =
    case node of
      Pending.NewtypeDecl x1 x2 x3 x4 x5 x6 -> NewtypeDecl <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4 <*> fromPending info x5 <*> fromPending info x6

instance FromPending Pending.DataConDecl where
  type Final Pending.DataConDecl = DataConDecl
  fromPending info node =
    case node of
      Pending.DataConAnn x1 x2 -> DataConAnn <$> fromPending info x1 <*> fromPending info x2
      Pending.PrefixCon x1 x2 x3 x4 x5 -> PrefixCon <$> traverse (finalizeCheckedType info) x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4 <*> fromPending info x5
      Pending.InfixCon x1 x2 x3 x4 x5 x6 -> InfixCon <$> traverse (finalizeCheckedType info) x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4 <*> fromPending info x5 <*> fromPending info x6
      Pending.RecordCon x1 x2 x3 x4 x5 -> RecordCon <$> traverse (finalizeCheckedType info) x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4 <*> fromPending info x5
      Pending.GadtCon x1 x2 x3 x4 x5 -> GadtCon <$> traverse (finalizeCheckedType info) x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4 <*> fromPending info x5
      Pending.TupleCon x1 x2 x3 x4 x5 -> TupleCon <$> traverse (finalizeCheckedType info) x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4 <*> fromPending info x5
      Pending.UnboxedSumCon x1 x2 x3 x4 x5 x6 -> UnboxedSumCon <$> traverse (finalizeCheckedType info) x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4 <*> fromPending info x5 <*> fromPending info x6
      Pending.ListCon x1 x2 x3 x4 -> ListCon <$> traverse (finalizeCheckedType info) x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4

instance FromPending Pending.GadtBody where
  type Final Pending.GadtBody = GadtBody
  fromPending info node =
    case node of
      Pending.GadtPrefixBody x1 x2 -> GadtPrefixBody <$> fromPending info x1 <*> fromPending info x2
      Pending.GadtRecordBody x1 x2 -> GadtRecordBody <$> fromPending info x1 <*> fromPending info x2

instance FromPending Pending.BangType where
  type Final Pending.BangType = BangType
  fromPending info node =
    case node of
      Pending.BangType x1 x2 x3 x4 x5 -> BangType <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4 <*> fromPending info x5

instance FromPending Pending.FieldDecl where
  type Final Pending.FieldDecl = FieldDecl
  fromPending info node =
    case node of
      Pending.FieldDecl x1 x2 x3 x4 x5 -> FieldDecl <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4 <*> pure x5

instance FromPending Pending.DerivingClause where
  type Final Pending.DerivingClause = DerivingClause
  fromPending info node =
    case node of
      Pending.DerivingClause x1 x2 x3 -> DerivingClause <$> fromPending info x1 <*> fromPending info x2 <*> traverse (finalizeDerivingPlan info) x3

instance FromPending Pending.DerivingStrategy where
  type Final Pending.DerivingStrategy = DerivingStrategy
  fromPending info node =
    case node of
      Pending.DerivingStock -> pure DerivingStock
      Pending.DerivingNewtype -> pure DerivingNewtype
      Pending.DerivingAnyclass -> pure DerivingAnyclass
      Pending.DerivingVia x1 -> DerivingVia <$> fromPending info x1

instance FromPending Pending.StandaloneDerivingDecl where
  type Final Pending.StandaloneDerivingDecl = StandaloneDerivingDecl
  fromPending info node =
    case node of
      Pending.StandaloneDerivingDecl x1 x2 x3 x4 x5 x6 x7 -> StandaloneDerivingDecl <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4 <*> fromPending info x5 <*> fromPending info x6 <*> traverse (finalizeDerivingPlan info) x7

instance FromPending Pending.ClassDecl where
  type Final Pending.ClassDecl = ClassDecl
  fromPending info node =
    case node of
      Pending.ClassDecl x1 x2 x3 x4 x5 -> ClassDecl <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4 <*> traverse (finalizeClass info) x5

instance FromPending Pending.ClassDeclItem where
  type Final Pending.ClassDeclItem = ClassDeclItem
  fromPending info node =
    case node of
      Pending.ClassItemAnn x1 x2 -> ClassItemAnn <$> fromPending info x1 <*> fromPending info x2
      Pending.ClassItemError x1 x2 -> ClassItemError <$> fromPending info x1 <*> fromPending info x2
      Pending.ClassItemTypeSig x1 x2 -> ClassItemTypeSig <$> fromPending info x1 <*> fromPending info x2
      Pending.ClassItemDefaultSig x1 x2 -> ClassItemDefaultSig <$> fromPending info x1 <*> fromPending info x2
      Pending.ClassItemFixity x1 x2 x3 x4 -> ClassItemFixity <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4
      Pending.ClassItemDefault x1 x2 -> ClassItemDefault <$> traverse (finalizeInstanceMethod info) x1 <*> fromPending info x2
      Pending.ClassItemTypeFamilyDecl x1 -> ClassItemTypeFamilyDecl <$> fromPending info x1
      Pending.ClassItemDataFamilyDecl x1 -> ClassItemDataFamilyDecl <$> fromPending info x1
      Pending.ClassItemDefaultTypeInst x1 -> ClassItemDefaultTypeInst <$> fromPending info x1
      Pending.ClassItemPragma x1 -> ClassItemPragma <$> fromPending info x1

instance FromPending Pending.InstanceDecl where
  type Final Pending.InstanceDecl = InstanceDecl
  fromPending info node =
    case node of
      Pending.InstanceDecl x1 x2 x3 x4 x5 x6 x7 x8 -> InstanceDecl <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4 <*> fromPending info x5 <*> fromPending info x6 <*> traverse (finalizeInstance info) x7 <*> pure x8

instance FromPending Pending.InstanceDeclItem where
  type Final Pending.InstanceDeclItem = InstanceDeclItem
  fromPending info node =
    case node of
      Pending.InstanceItemAnn x1 x2 -> InstanceItemAnn <$> fromPending info x1 <*> fromPending info x2
      Pending.InstanceItemError x1 x2 -> InstanceItemError <$> fromPending info x1 <*> fromPending info x2
      Pending.InstanceItemBind x1 x2 -> InstanceItemBind <$> traverse (finalizeInstanceMethod info) x1 <*> fromPending info x2
      Pending.InstanceItemTypeSig x1 x2 -> InstanceItemTypeSig <$> fromPending info x1 <*> fromPending info x2
      Pending.InstanceItemFixity x1 x2 x3 x4 -> InstanceItemFixity <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4
      Pending.InstanceItemTypeFamilyInst x1 -> InstanceItemTypeFamilyInst <$> fromPending info x1
      Pending.InstanceItemDataFamilyInst x1 -> InstanceItemDataFamilyInst <$> fromPending info x1
      Pending.InstanceItemPragma x1 -> InstanceItemPragma <$> fromPending info x1

instance FromPending Pending.ForeignDecl where
  type Final Pending.ForeignDecl = ForeignDecl
  fromPending info node =
    case node of
      Pending.ForeignDecl x1 x2 x3 x4 x5 x6 x7 -> ForeignDecl <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4 <*> fromPending info x5 <*> fromPending info x6 <*> pure x7

instance FromPending Pending.Expr where
  type Final Pending.Expr = Expr
  fromPending info node =
    case node of
      Pending.EAnn x1 x2 -> EAnn <$> fromPending info x1 <*> fromPending info x2
      Pending.ETc x1 x2 -> wrapInfo ETc <$> exprTc info x1 <*> fromPending info x2
      Pending.EVar x1 -> EVar <$> fromPending info x1
      Pending.EImplicitParam x1 -> EImplicitParam <$> fromPending info x1
      Pending.ETypeSyntax x1 x2 -> ETypeSyntax <$> fromPending info x1 <*> fromPending info x2
      Pending.EInt x1 x2 x3 x4 -> EInt <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4
      Pending.EFloat x1 x2 x3 x4 -> EFloat <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4
      Pending.EChar x1 x2 -> EChar <$> fromPending info x1 <*> fromPending info x2
      Pending.ECharHash x1 x2 x3 -> ECharHash <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3
      Pending.EString x1 x2 x3 -> EString <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3
      Pending.EStringHash x1 x2 x3 -> EStringHash <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3
      Pending.EOverloadedLabel x1 x2 -> EOverloadedLabel <$> fromPending info x1 <*> fromPending info x2
      Pending.EQuasiQuote x1 x2 -> EQuasiQuote <$> fromPending info x1 <*> fromPending info x2
      Pending.EIf x1 x2 x3 x4 -> EIf <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4
      Pending.EMultiWayIf x1 -> EMultiWayIf <$> fromPending info x1
      Pending.ELambdaPats x1 x2 -> ELambdaPats <$> fromPending info x1 <*> fromPending info x2
      Pending.ELambdaCase x1 -> ELambdaCase <$> fromPending info x1
      Pending.ELambdaCases x1 -> ELambdaCases <$> fromPending info x1
      Pending.EInfix x1 x2 x3 -> EInfix <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3
      Pending.EViewPat x1 x2 -> EViewPat <$> fromPending info x1 <*> fromPending info x2
      Pending.ENegate x1 x2 -> ENegate <$> fromPending info x1 <*> fromPending info x2
      Pending.ESectionL x1 x2 -> ESectionL <$> fromPending info x1 <*> fromPending info x2
      Pending.ESectionR x1 x2 -> ESectionR <$> fromPending info x1 <*> fromPending info x2
      Pending.ELetDecls x1 x2 -> ELetDecls <$> fromPending info x1 <*> fromPending info x2
      Pending.ECase x1 x2 -> ECase <$> fromPending info x1 <*> fromPending info x2
      Pending.EDo x1 x2 -> EDo <$> fromPending info x1 <*> fromPending info x2
      Pending.EListComp x1 x2 -> EListComp <$> fromPending info x1 <*> fromPending info x2
      Pending.EListCompParallel x1 x2 -> EListCompParallel <$> fromPending info x1 <*> fromPending info x2
      Pending.EArithSeq x1 x2 -> EArithSeq <$> fromPending info x1 <*> fromPending info x2
      Pending.ERecordCon x1 x2 x3 -> ERecordCon <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3
      Pending.ERecordUpd x1 x2 -> ERecordUpd <$> fromPending info x1 <*> fromPending info x2
      Pending.EGetField x1 x2 x3 -> EGetField <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3
      Pending.EGetFieldProjection x1 x2 -> EGetFieldProjection <$> fromPending info x1 <*> fromPending info x2
      Pending.ETypeSig x1 x2 -> ETypeSig <$> fromPending info x1 <*> fromPending info x2
      Pending.EParen x1 -> EParen <$> fromPending info x1
      Pending.EList x1 x2 x3 -> EList <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3
      Pending.ETuple x1 x2 x3 -> ETuple <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3
      Pending.EUnboxedSum x1 x2 x3 -> EUnboxedSum <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3
      Pending.ETypeApp x1 x2 -> ETypeApp <$> fromPending info x1 <*> fromPending info x2
      Pending.EApp x1 x2 -> EApp <$> fromPending info x1 <*> fromPending info x2
      Pending.ETHExpQuote x1 -> ETHExpQuote <$> fromPending info x1
      Pending.ETHTypedQuote x1 -> ETHTypedQuote <$> fromPending info x1
      Pending.ETHDeclQuote x1 -> ETHDeclQuote <$> fromPending info x1
      Pending.ETHTypeQuote x1 -> ETHTypeQuote <$> fromPending info x1
      Pending.ETHPatQuote x1 -> ETHPatQuote <$> fromPending info x1
      Pending.ETHNameQuote x1 -> ETHNameQuote <$> fromPending info x1
      Pending.ETHTypeNameQuote x1 -> ETHTypeNameQuote <$> fromPending info x1
      Pending.ETHSplice x1 -> ETHSplice <$> fromPending info x1
      Pending.ETHTypedSplice x1 -> ETHTypedSplice <$> fromPending info x1
      Pending.EProc x1 x2 -> EProc <$> fromPending info x1 <*> fromPending info x2
      Pending.EPragma x1 x2 -> EPragma <$> fromPending info x1 <*> fromPending info x2

instance (FromPending body) => FromPending (Pending.CaseAlt body) where
  type Final (Pending.CaseAlt body) = CaseAlt (Final body)
  fromPending info node =
    case node of
      Pending.CaseAlt x1 x2 x3 -> CaseAlt <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3

instance FromPending Pending.LambdaCaseAlt where
  type Final Pending.LambdaCaseAlt = LambdaCaseAlt
  fromPending info node =
    case node of
      Pending.LambdaCaseAlt x1 x2 x3 -> LambdaCaseAlt <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3

instance (FromPending body) => FromPending (Pending.DoStmt body) where
  type Final (Pending.DoStmt body) = DoStmt (Final body)
  fromPending info node =
    case node of
      Pending.DoAnn x1 x2 -> DoAnn <$> fromPending info x1 <*> fromPending info x2
      Pending.DoRecKnot x1 x2 -> DoRecKnot <$> fromPending info x1 <*> fromPending info x2
      Pending.DoBind x1 x2 x3 -> DoBind <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3
      Pending.DoLetDecls x1 -> DoLetDecls <$> fromPending info x1
      Pending.DoExpr x1 x2 -> DoExpr <$> fromPending info x1 <*> fromPending info x2
      Pending.DoRecStmt x1 x2 x3 -> DoRecStmt <$> (join <$> traverse (recStmt info) x1) <*> fromPending info x2 <*> fromPending info x3

instance FromPending Pending.Cmd where
  type Final Pending.Cmd = Cmd
  fromPending info node =
    case node of
      Pending.CmdAnn x1 x2 -> CmdAnn <$> fromPending info x1 <*> fromPending info x2
      Pending.CmdArrApp x1 x2 x3 -> CmdArrApp <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3
      Pending.CmdInfix x1 x2 x3 -> CmdInfix <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3
      Pending.CmdDo x1 -> CmdDo <$> fromPending info x1
      Pending.CmdIf x1 x2 x3 -> CmdIf <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3
      Pending.CmdCase x1 x2 -> CmdCase <$> fromPending info x1 <*> fromPending info x2
      Pending.CmdLet x1 x2 -> CmdLet <$> fromPending info x1 <*> fromPending info x2
      Pending.CmdLam x1 x2 -> CmdLam <$> fromPending info x1 <*> fromPending info x2
      Pending.CmdApp x1 x2 -> CmdApp <$> fromPending info x1 <*> fromPending info x2
      Pending.CmdPar x1 -> CmdPar <$> fromPending info x1

instance FromPending Pending.CompStmt where
  type Final Pending.CompStmt = CompStmt
  fromPending info node =
    case node of
      Pending.CompAnn x1 x2 -> CompAnn <$> fromPending info x1 <*> fromPending info x2
      Pending.CompGen x1 x2 x3 -> CompGen <$> annotation info x1 <*> fromPending info x2 <*> fromPending info x3
      Pending.CompGuard x1 x2 -> CompGuard <$> annotation info x1 <*> fromPending info x2
      Pending.CompLetDecls x1 x2 -> CompLetDecls <$> annotation info x1 <*> fromPending info x2
      Pending.CompThen x1 x2 -> CompThen <$> annotation info x1 <*> fromPending info x2
      Pending.CompThenBy x1 x2 x3 -> CompThenBy <$> annotation info x1 <*> fromPending info x2 <*> fromPending info x3
      Pending.CompGroupUsing x1 x2 -> CompGroupUsing <$> annotation info x1 <*> fromPending info x2
      Pending.CompGroupByUsing x1 x2 x3 -> CompGroupByUsing <$> annotation info x1 <*> fromPending info x2 <*> fromPending info x3

instance FromPending Pending.ArithSeq where
  type Final Pending.ArithSeq = ArithSeq
  fromPending info node =
    case node of
      Pending.ArithSeqAnn x1 x2 -> ArithSeqAnn <$> fromPending info x1 <*> fromPending info x2
      Pending.ArithSeqFrom x1 x2 -> ArithSeqFrom <$> fromPending info x1 <*> fromPending info x2
      Pending.ArithSeqFromThen x1 x2 x3 -> ArithSeqFromThen <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3
      Pending.ArithSeqFromTo x1 x2 x3 -> ArithSeqFromTo <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3
      Pending.ArithSeqFromThenTo x1 x2 x3 x4 -> ArithSeqFromThenTo <$> fromPending info x1 <*> fromPending info x2 <*> fromPending info x3 <*> fromPending info x4
