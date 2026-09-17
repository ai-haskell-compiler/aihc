{-# LANGUAGE PatternSynonyms #-}

-- | Type checker annotations for AST nodes.
--
-- Following the pattern established by @aihc-resolve@, the type checker
-- attaches its results as 'Annotation' values on AST nodes using the
-- existing 'DeclAnn'/'EAnn'/'PAnn'/'TAnn' wrappers.
module Aihc.Tc.Annotations
  ( -- * Annotation type
    TcAnnotation (..),
    TcCastAnnotation (..),
    PendingTcCastAnnotation (..),
    CastDirection (..),
    annotateRhsCast,
    annotateExprCast,
    annotateFunCast,
    TcForeignImportAnnotation (..),
    TcForeignImportInfo (..),
    TcForeignSafety (..),
    TcForeignEffect (..),
    TcForeignTarget (..),
    TcForeignCApi (..),
    TcForeignCApiKind (..),
    TcForeignMarshal (..),
    TcForeignAbiType (..),
    PendingTcAnnotation (..),
    TcClassAnnotation (..),
    TcClassMethodAnnotation (..),
    TcDictBinderAnnotation (..),
    TcDerivingAnnotation (..),
    TcDerivingContext (..),
    TcDerivingPlan (..),
    TcDerivingStrategy (..),
    TcInstanceAnnotation (..),
    TcDerivedInstance (..),
    TcCoercedDeriving (..),
    TcCoercedInstance (..),
    TcCoercedMethod (..),
    TcPatSynAnnotation (..),
    TcInstanceMethodAnnotation (..),

    -- * Pattern synonyms for extracting annotations

    -- * Helpers
    annotateDecl,
    pendingAnnotation,
    pendingTypeLambdaAnnotation,

    -- * Pretty-printing
    renderFunDepNames,
    renderPred,
    renderTcType,
    renderTyLit,
    renderTcTypeInModule,
    renderTcSignature,
  )
where

import Aihc.Parser.Syntax
  ( Decl (..),
    Expr (..),
    Match,
    Rhs (..),
    SourceSpan,
    mkAnnotation,
  )
import Aihc.Resolve (ResolutionNamespace (..))
import Aihc.Tc.Env (AssociatedTypeInfo, CType, DataTypeInfo, FunDep, TypeFamilyInstanceInfo)
import Aihc.Tc.Evidence (Coercion, EvTerm, EvVar)
import Aihc.Tc.Types (Pred (..), TcType (..), TyCon (..), TyLit (..), TyVarId (..), Unique (..), tyConModuleName, tyConNamespace, pattern KType)
import Control.DeepSeq (NFData)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

-- | A checked cast on the result of a right-hand side.
newtype TcCastAnnotation = TcCastAnnotation Coercion
  deriving (Eq, Show)

-- | The solver must supply the proof before FC desugaring.
data PendingTcCastAnnotation = PendingTcCastAnnotation TcType EvVar CastDirection
  deriving (Eq, Show)

-- | Which endpoint of the wanted equality the annotated expression has.
-- The proof runs from the wanted's left type to its right type, so a
-- cast onto the left type is the @sym@ of the proof.
data CastDirection
  = -- | The wanted is @source ~ target@: the proof is the cast.
    CastToRight
  | -- | The wanted is @target ~ source@: the cast is its @sym@.
    CastToLeft
  deriving (Eq, Show)

annotateRhsCast :: TcType -> EvVar -> Rhs body -> Rhs body
annotateRhsCast ty evidence rhs =
  let annotation = mkAnnotation (PendingTcCastAnnotation ty evidence CastToRight)
   in case rhs of
        UnguardedRhs annotations body locals -> UnguardedRhs (annotation : annotations) body locals
        GuardedRhss annotations bodies locals -> GuardedRhss (annotation : annotations) bodies locals

-- | Cast an expression onto the left type of its wanted equality. An
-- application argument is checked as @expected ~ actual@, so a given
-- equality that proves it runs backwards for the argument itself.
annotateExprCast :: TcType -> EvVar -> Expr -> Expr
annotateExprCast ty evidence =
  EAnn (mkAnnotation (PendingTcCastAnnotation ty evidence CastToLeft))

-- | Cast an expression onto the right type of its wanted equality. The
-- function of an application whose type is not syntactically an arrow is
-- equated as @actual ~ (argument -> result)@, so the proof runs forwards
-- for the function itself.
annotateFunCast :: TcType -> EvVar -> Expr -> Expr
annotateFunCast ty evidence =
  EAnn (mkAnnotation (PendingTcCastAnnotation ty evidence CastToRight))

-- | Annotation attached to AST nodes by the type checker.
--
-- Not every field is populated for every node. A variable reference gets
-- a type; a top-level binding gets the generalized scheme, etc.
data TcAnnotation = TcAnnotation
  { -- | The inferred/checked type of this node.
    tcAnnType :: !TcType,
    -- | Type variables abstracted at this expression.
    tcAnnTypeBinders :: ![TyVarId],
    -- | Type arguments made explicit at this occurrence.
    tcAnnTypeArgs :: ![TcType],
    -- | Evidence terms whose dictionaries must be passed at this occurrence.
    tcAnnEvidenceTerms :: ![EvTerm],
    -- | Evidence terms whose dictionaries are abstracted at this expression.
    tcAnnEvidenceBinders :: ![EvTerm],
    -- | Term argument types made explicit for lambda-like binders.
    tcAnnTermArgTypes :: ![TcType]
  }
  deriving (Eq, Show)

-- | The fully checked lowering plan for a foreign import.  Keeping this in
-- the type-checker output prevents System FC desugaring from rediscovering
-- Haskell FFI representation rules from constructor names.
data TcForeignImportAnnotation = TcForeignImportAnnotation
  { tcForeignArguments :: ![TcForeignMarshal],
    tcForeignResult :: !TcForeignMarshal,
    tcForeignEffect :: !TcForeignEffect,
    -- | The C entity that the entity string names.
    tcForeignSymbol :: !Text,
    tcForeignTarget :: !TcForeignTarget,
    -- | How the entity is reached, when it is reached through a header rather
    -- than through the platform ABI.  A @ccall@ import has none.
    tcForeignCApi :: !(Maybe TcForeignCApi)
  }
  deriving (Eq, Show, Read, Generic)

instance NFData TcForeignImportAnnotation

-- | What a @capi@ import says about how its entity is reached.
--
-- A @capi@ entity is reached through the C API of its header rather than
-- through the platform ABI, so it may be a macro, a @static inline@ function
-- or a constant.  The type checker records what the declaration says; it is
-- for the code generator to decide what reaching such an entity takes.
data TcForeignCApi = TcForeignCApi
  { -- | The header the entity string names.  A @capi@ entity may name none.
    tcForeignCApiHeader :: !(Maybe Text),
    tcForeignCApiKind :: !TcForeignCApiKind
  }
  deriving (Eq, Show, Read, Generic)

instance NFData TcForeignCApi

-- | Whether a @capi@ entity is called or read as a value.
data TcForeignCApiKind
  = -- | @foreign import capi "header.h f"@: @f@ is a function.
    TcForeignCApiFunction
  | -- | @foreign import capi "header.h value x"@: @x@ is a constant.
    TcForeignCApiValue
  deriving (Eq, Show, Read, Generic)

instance NFData TcForeignCApiKind

-- | The checked calling convention of a foreign import. The interface keeps
-- this fact for each foreign import, so a module that uses the import can
-- desugar each use to a saturated foreign call.
data TcForeignImportInfo
  = -- | A @foreign import prim@. The name of the import selects the primitive.
    TcForeignPrimImport
  | -- | A @foreign import ccall@ with its safety mark and checked plan.
    TcForeignCCallImport !TcForeignSafety !TcForeignImportAnnotation
  deriving (Eq, Show, Read, Generic)

instance NFData TcForeignImportInfo

-- | The safety mark of a @ccall@ foreign import. A missing mark is safe.
data TcForeignSafety
  = TcForeignSafe
  | TcForeignUnsafe
  | TcForeignInterruptible
  deriving (Eq, Show, Read, Generic)

instance NFData TcForeignSafety

-- | Whether a foreign import calls the C symbol or takes its address.
data TcForeignTarget
  = TcForeignCall
  | TcForeignAddress
  deriving (Eq, Show, Read, Generic)

instance NFData TcForeignTarget

-- | Whether a raw foreign call is pure or explicitly threads the real-world
-- state token.
data TcForeignEffect
  = TcForeignPure
  | TcForeignRealWorld
  deriving (Eq, Show, Read, Generic)

instance NFData TcForeignEffect

-- | A source value's path to its primitive ABI representation.  Constructor
-- names are ordered outermost to innermost; for example, a @CInt@ is lowered
-- through @CInt@ and @I32#@ to @Int32#@.
data TcForeignMarshal = TcForeignMarshal
  { tcForeignSourceType :: !TcType,
    tcForeignPrimitiveType :: !TcType,
    tcForeignConstructors :: ![Text],
    tcForeignAbiType :: !TcForeignAbiType,
    -- | The C spelling of the value for a @capi@ wrapper, when a @CTYPE@
    -- pragma on the source type, or on the pointee of a pointer type, gives
    -- one.  Without it the wrapper spells the value as its ABI type.
    tcForeignCType :: !(Maybe CType)
  }
  deriving (Eq, Show, Read, Generic)

instance NFData TcForeignMarshal

-- | Primitive values understood by the C ABI bridge.  This is deliberately
-- independent from lifted Haskell wrapper types.
data TcForeignAbiType
  = TcForeignInt
  | TcForeignInt8
  | TcForeignInt16
  | TcForeignInt32
  | TcForeignInt64
  | TcForeignWord
  | TcForeignWord8
  | TcForeignWord16
  | TcForeignWord32
  | TcForeignWord64
  | TcForeignFloat
  | TcForeignDouble
  | TcForeignAddr
  | -- | The unit result of a C procedure.
    TcForeignVoid
  deriving (Eq, Show, Read, Generic)

instance NFData TcForeignAbiType

-- | Type-checker annotation payload before constraint solving has finished.
--
-- The generator attaches this directly to the syntax node that produced it.
-- A finalization pass zonks the types and resolves evidence variables into
-- ordinary 'TcAnnotation' values after solving.
data PendingTcAnnotation = PendingTcAnnotation
  { pendingTcAnnType :: !TcType,
    pendingTcAnnTypeBinders :: ![TyVarId],
    pendingTcAnnTypeArgs :: ![TcType],
    -- | How many leading type arguments instantiate inferred binders. A
    -- visible type application skips them; the desugarer applies them all.
    pendingTcAnnInferredTypeArgs :: !Int,
    pendingTcAnnEvidenceVars :: ![EvVar],
    pendingTcAnnEvidenceBinders :: ![EvVar],
    pendingTcAnnTermArgTypes :: ![TcType]
  }
  deriving (Eq, Show)

data TcDictBinderAnnotation = TcDictBinderAnnotation
  { tcDictBinderClassName :: !Text,
    tcDictBinderArgs :: ![TcType],
    tcDictBinderType :: !TcType
  }
  deriving (Eq, Show)

data TcClassMethodAnnotation = TcClassMethodAnnotation
  { tcClassMethodName :: !Text,
    tcClassMethodType :: !TcType,
    tcClassMethodTyVars :: ![TyVarId],
    tcClassMethodDictType :: !TcType,
    tcClassMethodIndex :: !Int
  }
  deriving (Eq, Show)

data TcClassAnnotation = TcClassAnnotation
  { tcClassTyCon :: !TyCon,
    tcClassKindTyVars :: ![TyVarId],
    tcClassTyVars :: ![TyVarId],
    tcClassSuperClasses :: ![TcDictBinderAnnotation],
    tcClassMethods :: ![TcClassMethodAnnotation],
    tcClassDefaultMethods :: ![Text],
    tcClassDefaultSignatures :: ![(Text, TcType)],
    tcClassAssociatedTypes :: ![AssociatedTypeInfo],
    tcClassFunDeps :: ![FunDep]
  }
  deriving (Eq, Show)

-- | The effective deriving strategy selected by the type checker. Source
-- declarations without an explicit strategy are resolved before a plan is
-- attached, so System FC never has to reproduce extension-sensitive policy.
data TcDerivingStrategy
  = TcDerivingStock
  | TcDerivingNewtype
  | TcDerivingAnyclass
  | TcDerivingVia !TcType
  deriving (Eq, Show)

-- | How the derived instance context is supplied. Standalone deriving has an
-- explicit checked context; attached clauses require strategy-specific
-- inference in a later type-checker step.
data TcDerivingContext
  = TcDerivingInferContext
  | TcDerivingExplicitContext ![Pred]
  deriving (Eq, Show)

-- | The checked shape of one deriving request. The context is inferred
-- for an attached clause and checked for a standalone declaration; the
-- generated instance declaration is an ordinary 'DeclInstance' that the
-- instance checker and System FC lowering treat like source.
data TcDerivingPlan = TcDerivingPlan
  { tcDerivingSourceSpan :: !(Maybe SourceSpan),
    tcDerivingStrategy :: !TcDerivingStrategy,
    tcDerivingStockFallback :: !Bool,
    tcDerivingClassName :: !Text,
    tcDerivingClassTyCon :: !TyCon,
    tcDerivingClassOrigin :: !(Maybe (Text, Text)),
    tcDerivingTyVars :: ![TyVarId],
    tcDerivingHeadTypes :: ![TcType],
    -- | Checked constructor layout of the final instance-head type, when the
    -- target is a data or newtype constructor known to this compilation.
    tcDerivingDataType :: !(Maybe DataTypeInfo),
    tcDerivingContext :: !TcDerivingContext,
    tcDerivingClassTyVars :: ![TyVarId],
    tcDerivingClassSuperClasses :: ![TcDictBinderAnnotation],
    tcDerivingClassMethods :: ![TcClassMethodAnnotation],
    tcDerivingDefaultMethods :: ![Text],
    tcDerivingDefaultSignatures :: ![(Text, [Pred])]
  }
  deriving (Eq, Show)

newtype TcDerivingAnnotation = TcDerivingAnnotation
  { tcDerivingPlans :: [TcDerivingPlan]
  }
  deriving (Eq, Show)

-- | The checked matcher equation, builder equations, and record field
-- selector equations of a pattern synonym. The desugarer emits them as
-- ordinary functions. A selector pairs its field label with its equation.
data TcPatSynAnnotation = TcPatSynAnnotation
  { tcPatSynMatcher :: !Match,
    tcPatSynBuilder :: !(Maybe [Match]),
    tcPatSynSelectors :: ![(Text, Match)]
  }
  deriving (Eq, Show)

-- | A generated instance retains its source derivation plan.
newtype TcCoercedDeriving = TcCoercedDeriving TcDerivingPlan
  deriving (Eq, Show)

-- | An instance that @deriving@ generated rather than the source. Such an
-- instance may leave methods to the desugarer without a warning.
data TcDerivedInstance = TcDerivedInstance
  deriving (Eq, Show)

-- | Checked evidence and casts for a derived instance that reuses another
-- type's instance and coerces its methods: newtype deriving, which coerces
-- from the representation, and deriving via, from the via type.
data TcCoercedInstance = TcCoercedInstance
  { tcCoercedHeadTypes :: ![TcType],
    tcCoercedEvidence :: !(Maybe EvTerm),
    tcCoercedFieldTypes :: ![TcType],
    tcCoercedDictionaryCast :: !(Maybe Coercion),
    tcCoercedMethods :: ![TcCoercedMethod]
  }
  deriving (Eq, Show)

-- | A method cast applies after its type and dictionary arguments.
data TcCoercedMethod = TcCoercedMethod
  { tcCoercedMethodName :: !Text,
    tcCoercedMethodIndex :: !Int,
    tcCoercedMethodTyVars :: ![TyVarId],
    tcCoercedMethodPredicates :: ![Pred],
    tcCoercedMethodCoercion :: !Coercion
  }
  deriving (Eq, Show)

data TcInstanceAnnotation = TcInstanceAnnotation
  { tcInstanceDictName :: !Text,
    tcInstanceDictType :: !TcType,
    tcInstanceClassTyCon :: !TyCon,
    tcInstanceTyVars :: ![TyVarId],
    tcInstanceHeadTypes :: ![TcType],
    tcInstanceClassTyVars :: ![TyVarId],
    tcInstanceClassOrigin :: !(Maybe (Text, Text)),
    tcInstanceClassSuperClasses :: ![TcDictBinderAnnotation],
    tcInstanceClassMethods :: ![TcClassMethodAnnotation],
    tcInstanceContextDicts :: ![TcDictBinderAnnotation],
    tcInstanceSuperClasses :: ![(TcDictBinderAnnotation, EvTerm)],
    tcInstanceMethodOrder :: ![Text],
    tcInstanceDefaultMethods :: ![Text],
    -- | For each default method whose class gives it a default signature,
    -- evidence for the constraints of that signature at the instance head,
    -- in signature order. The default-method worker takes them after the
    -- instance dictionary itself.
    tcInstanceDefaultMethodEvidence :: ![(Text, [EvTerm])],
    -- | For each default method whose class gives it a default signature,
    -- the type arguments for the binders the signature quantifies on its
    -- own, in signature order. The class head supplies the binders before
    -- them; a binder the signature's constraints determine -- @f@ in
    -- @(RandomGen f, FrozenGen f m, g ~ MutableGen f m)@ -- is solved at
    -- the instance and has no other source.
    tcInstanceDefaultMethodTypes :: ![(Text, [TcType])],
    -- | The checked associated type family equations of the instance,
    -- explicit ones and instantiated class defaults.
    tcInstanceAssociatedTypes :: ![TypeFamilyInstanceInfo],
    -- | Present when the instance was derived by coercing another
    -- instance's methods rather than by generating its own.
    tcInstanceCoerced :: !(Maybe TcCoercedInstance)
  }
  deriving (Eq, Show)

data TcInstanceMethodAnnotation = TcInstanceMethodAnnotation
  { tcInstanceMethodName :: !Text,
    tcInstanceMethodType :: !TcType
  }
  deriving (Eq, Show)

-- | Wrap a declaration with a type annotation.
annotateDecl :: TcAnnotation -> Decl -> Decl
annotateDecl ann = DeclAnn (mkAnnotation ann)

pendingAnnotation :: TcType -> [TcType] -> [EvVar] -> [TcType] -> PendingTcAnnotation
pendingAnnotation ty typeArgs evidenceVars =
  PendingTcAnnotation ty [] typeArgs 0 evidenceVars []

pendingTypeLambdaAnnotation :: TcType -> [TyVarId] -> [EvVar] -> PendingTcAnnotation
pendingTypeLambdaAnnotation ty binders evidenceBinders =
  PendingTcAnnotation ty binders [] 0 [] evidenceBinders []

-- | Render a binder and its 'TcType' as a human-readable signature.
renderTcSignature :: Text -> TcType -> String
renderTcSignature name ty = T.unpack name ++ " ∷ " ++ renderTcType ty

-- | Render the two sides of a functional dependency as source-like text.
renderFunDepNames :: [Text] -> [Text] -> String
renderFunDepNames determiners determined =
  unwords (map T.unpack determiners) ++ " → " ++ unwords (map T.unpack determined)

-- | Render a class or equality predicate as source-like text.
renderPred :: Pred -> String
renderPred pred' =
  case pred' of
    ClassPred classTyCon args ->
      renderTcType (TcTyCon classTyCon args)
    EqPred left right ->
      renderTcType left ++ " ~ " ++ renderTcType right
    IParamPred name payload ->
      T.unpack name ++ " ∷ " ++ renderTcType payload
    IrredPred constraint ->
      renderTcType constraint
    QuantifiedPred variables antecedents consequent ->
      "∀ "
        ++ unwords (map (T.unpack . tvName) variables)
        ++ ". "
        ++ (if null antecedents then "" else "(" ++ commaSep (map renderPred antecedents) ++ ") ⇒ ")
        ++ renderPred consequent
  where
    commaSep = T.unpack . T.intercalate (T.pack ", ") . map T.pack

-- | Render a type-level literal as its source spelling.
renderTyLit :: TyLit -> String
renderTyLit literal =
  case literal of
    TyLitNat value -> show value
    TyLitSymbol value -> show (T.unpack value)
    TyLitChar value -> show value

-- | Render a 'TcType' as a human-readable string.
--
-- Uses a precedence level to decide when to insert parentheses:
--   0 = no parens needed (top level or right of ->)
--   1 = parens needed for function types (left of ->)
--   2 = parens needed for function types and type applications (inside type con args)
renderTcType :: TcType -> String
renderTcType = renderTcTypeInModule Nothing

renderTcTypeInModule :: Maybe Text -> TcType -> String
renderTcTypeInModule currentModule = go 0
  where
    go :: Int -> TcType -> String
    go _ (TcTyVar tv) = T.unpack (tvName tv)
    go _ (TcMetaTv (Unique u)) = "?" ++ show u
    go _ KType = "Type"
    go _ TcArrowTy = "(->)"
    go _ (TcTyLit literal) = renderTyLit literal
    go _ (TcTyCon (TyCon name 1) [arg])
      | name == T.pack "[]" = "[" ++ go 0 arg ++ "]"
    go _ (TcTyCon tc@(TyCon name arity) args)
      | tyConNamespace tc == ResolutionNamespaceType,
        isBoxedTupleCon name arity,
        arity == length args =
          "(" ++ commaSep (map (go 0) args) ++ ")"
    go _ (TcTyCon tc []) = T.unpack (renderTyConName tc)
    go p (TcTyCon tc args) =
      parenIf (p >= 2) $
        unwords (T.unpack (renderTyConName tc) : map (go 2) args)
    go p (TcFunTy a b) =
      parenIf (p >= 1) $
        go 1 a ++ " → " ++ go 0 b
    go p (TcForAllTy tv body) =
      let (tvs, inner) = collectForAlls body
       in parenIf (p >= 1) $
            "∀ " ++ unwords (map (T.unpack . tvName) (tv : tvs)) ++ ". " ++ go 0 inner
    go p (TcQualTy preds body) =
      parenIf (p >= 1) $
        "(" ++ commaSep (map showPred preds) ++ ") ⇒ " ++ go 0 body
    go p (TcAppTy f a) =
      parenIf (p >= 2) $
        go 1 f ++ " " ++ go 2 a
    showPred (ClassPred cls args) =
      T.unpack (renderTyConName cls) ++ " " ++ unwords (map (go 2) args)
    showPred (EqPred t1 t2) =
      go 2 t1 ++ " ~ " ++ go 2 t2
    showPred (IParamPred name payload) =
      T.unpack name ++ " ∷ " ++ go 0 payload
    showPred (IrredPred constraint) = go 0 constraint
    showPred predicate@QuantifiedPred {} = renderPred predicate

    parenIf False s = s
    parenIf True s = "(" ++ s ++ ")"

    commaSep = T.unpack . T.intercalate (T.pack ", ") . map T.pack

    -- Rendering only: a name that reads as a tuple prints in tuple syntax.
    -- The identity of a tuple type constructor is the wiring's to say
    -- ('Aihc.Tc.Wiring'), but a renderer has no configuration to consult,
    -- and printing @(a, b)@ for a lookalike misleads no one.
    isBoxedTupleCon name arity =
      arity /= 1 && name == tupleDisplayName arity

    tupleDisplayName arity =
      case arity of
        0 -> T.pack "Unit"
        1 -> T.pack "Solo"
        _ -> T.pack ("Tuple" <> show arity)

    renderTyConName tyCon =
      namespacePrefix <> qualifiedName
      where
        definingModule = tyConModuleName tyCon
        namespacePrefix
          | tyConNamespace tyCon == ResolutionNamespaceTerm = T.pack "'"
          | otherwise = T.empty
        qualifiedName
          | Nothing <- currentModule = tyConName tyCon
          | Just definingModule == currentModule = tyConName tyCon
          | otherwise = definingModule <> T.pack "." <> tyConName tyCon

-- | Collect nested forall binders into a list.
collectForAlls :: TcType -> ([TyVarId], TcType)
collectForAlls (TcForAllTy tv body) =
  let (tvs, inner) = collectForAlls body
   in (tv : tvs, inner)
collectForAlls ty = ([], ty)
