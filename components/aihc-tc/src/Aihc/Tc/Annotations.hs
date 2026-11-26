{-# LANGUAGE PatternSynonyms #-}

-- | The information that the type checker finds for the syntax tree.
--
-- The tree of "Aihc.Tc.Syntax" holds these types in explicit fields.
module Aihc.Tc.Annotations
  ( -- * Annotation type
    TcAnnotation (..),
    TcCastAnnotation (..),
    CastDirection (..),
    TcMatchArityCast (..),
    TcForeignImportAnnotation (..),
    TcForeignImportInfo (..),
    TcForeignSafety (..),
    TcForeignEffect (..),
    TcForeignTarget (..),
    TcForeignCApi (..),
    TcForeignCApiKind (..),
    TcForeignMarshal (..),
    TcForeignAbiType (..),
    TcClassAnnotation (..),
    TcClassMethodAnnotation (..),
    TcDictBinderAnnotation (..),
    TcDerivingContext (..),
    TcDerivingPlan (..),
    TcDerivedInstance (..),
    TcDerivingStrategy (..),
    TcInstanceAnnotation (..),
    TcRecStmt (..),
    TcOccurrence (..),
    TcAbstraction (..),
    TcComposite (..),
    TcSection (..),
    TcBoundTuple (..),
    TcBinderAnnotation (..),
    TcPatternAnnotation (..),
    TcAnnotated (..),
    TcCoercedInstance (..),
    TcCoercedMethod (..),
    TcInstanceMethodAnnotation (..),
    TcModuleIdentity (..),

    -- * Pattern synonyms for extracting annotations

    -- * Helpers
    typeAnnotation,
    pendingAnnotation,
    pendingTypeLambdaAnnotation,
    pendingCast,
    pendingMatchArityCast,

    -- * Pretty-printing
    renderFunDepNames,
    renderPred,
    renderTcType,
    renderTyLit,
    renderTcTypeInModule,
    renderTcSignature,
  )
where

import Aihc.Parser.Syntax (SourceSpan)
import Aihc.Resolve (Entity, PackageId, ResolutionNamespace (..))
import Aihc.Tc.Env (AssociatedTypeInfo, CType, DataTypeInfo, FunDep, TypeFamilyInstanceInfo)
import Aihc.Tc.Evidence (Coercion (..), EvTerm (..), EvVar)
import Aihc.Tc.Types (Pred (..), TcType (..), TyCon (..), TyLit (..), TyVarId (..), Unique (..), tyConModuleName, tyConNamespace, pattern KType)
import Control.DeepSeq (NFData)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

-- | The package and the name of a checked module. The type checker attaches
-- it to the module, and the phases after it read it from there.
data TcModuleIdentity = TcModuleIdentity !PackageId !Text
  deriving (Eq, Show)

-- | A checked cast on an expression or a right-hand side. The type is the
-- type after the cast, which is the right type of the proof. A reflexive
-- proof has no coercion. The annotation then keeps only the type, because
-- a function whose type is a family application needs its arrow type.
--
-- Before the solver has finished, the proof is the evidence variable of a
-- wanted equality: see 'pendingCast'.
data TcCastAnnotation = TcCastAnnotation (Maybe Coercion) TcType
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

-- | Function equations that bind more arguments than the checked type
-- shows. The annotation is on the first equation. The result of the
-- checked type is then not an arrow, but it is equal to one. An example is
-- an associated family application: with @type D Unit = Unit -> Unit@, the
-- method @f :: a -> D a@ can have the equation @f u x = x@.
--
-- The type is the arrow type of the arguments that the checked type does
-- not show. The proof runs from that arrow type to the result of the
-- checked type. A reflexive proof has no coercion.
data TcMatchArityCast = TcMatchArityCast (Maybe Coercion) TcType
  deriving (Eq, Show)

-- | The general form of an annotation of the type checker.
--
-- The tree does not hold this record. Each node holds a record that has
-- only the fields of its annotation, such as 'TcOccurrence' or
-- 'TcComposite'. The class 'TcAnnotated' changes such a record to and
-- from the general form. The generator, the finalization pass, and the
-- other walks use the general form.
--
-- The generator attaches the annotation before the solver has finished.
-- The types can then have meta variables, and each evidence term is an
-- 'EvVarTerm' with the evidence variable of a wanted constraint. The
-- finalization pass zonks the types and puts the solved evidence in place
-- of each variable.
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
    tcAnnTermArgTypes :: ![TcType],
    -- | How many leading type arguments instantiate inferred binders. A
    -- visible type application skips them; the desugarer applies them all.
    tcAnnInferredTypeArgs :: !Int
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
  = -- | A @foreign import prim@. The entity string selects the primitive.
    -- If the declaration has no entity string, the name of the import
    -- selects the primitive. In GHC, the entity string names a Cmm function.
    TcForeignPrimImport !(Maybe Text)
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
  | TcForeignDynamic
  | TcForeignWrapper !TcForeignMarshal
  | -- | The address of a C function, an import of type @FunPtr f@. The
    -- marshal is the one of the pointer.  The arguments, the result and the
    -- effect of the plan are the ones of @f@, which a target that declares
    -- the function needs, as wasm does.
    TcForeignFunctionAddress !TcForeignMarshal
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

-- | One dictionary that a binding takes or holds. The predicate is the
-- one the type checker solved with: the type alone does not say whether
-- its head is a class or a stuck type family, and the desugarer needs to
-- know, because a stuck constraint takes its kind from the context.
data TcDictBinderAnnotation = TcDictBinderAnnotation
  { tcDictBinderClassName :: !Text,
    tcDictBinderArgs :: ![TcType],
    tcDictBinderType :: !TcType,
    tcDictBinderPred :: !Pred
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
    -- | A standalone declaration supplies its own instance context.
    tcDerivingStandalone :: !Bool,
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

-- | The origin of an instance that @deriving@ generated.
data TcDerivedInstance = TcDerivedInstance
  { -- | A @deriving@ clause of a data declaration generated the instance.
    -- A standalone @deriving instance@ does not have this mark. GHC
    -- checks the functional dependencies of such an instance with the
    -- liberal coverage condition, also without UndecidableInstances,
    -- because the user did not write its context.
    tcDerivedByClause :: !Bool,
    -- | The plan of a generated instance that reuses another instance.
    tcDerivedCoercedPlan :: !(Maybe TcDerivingPlan)
  }
  deriving (Eq, Show)

-- | What the type checker found for a @rec@ statement.
data TcRecStmt = TcRecStmt
  { -- | The tuple of the group variables. Its only term type is the type
    -- of the statement block.
    tcRecTuple :: !TcBoundTuple,
    -- | The occurrence of each generalized group variable at the knot of
    -- the group. A let statement of the group generalized the variable,
    -- and the knot takes one instance of it.
    tcRecKnotOccurrences :: ![(Entity, TcOccurrence)]
  }
  deriving (Eq, Show)

-- | An occurrence of a name, or an expression that is instantiated as an
-- occurrence is: its type, the type arguments, and the evidence that it
-- is applied to.
data TcOccurrence = TcOccurrence
  { -- | The type of the instance.
    tcOccType :: !TcType,
    -- | Type arguments made explicit at this occurrence.
    tcOccTypeArgs :: ![TcType],
    -- | Evidence terms whose dictionaries must be passed at this occurrence.
    tcOccEvidence :: ![EvTerm],
    -- | How many leading type arguments instantiate inferred binders. A
    -- visible type application skips them; the desugarer applies them all.
    tcOccInferredTypeArgs :: !Int
  }
  deriving (Eq, Show)

-- | An expression that is checked against a polytype: the polytype, and
-- the type variables and the evidence that the expression is abstracted
-- over.
data TcAbstraction = TcAbstraction
  { tcAbsType :: !TcType,
    tcAbsTypeBinders :: ![TyVarId],
    tcAbsEvidenceBinders :: ![EvTerm]
  }
  deriving (Eq, Show)

-- | The type of an expression that has parts, and the types of the parts:
-- the fields of a tuple, the alternatives of an unboxed sum, or the
-- element of a list comprehension.
data TcComposite = TcComposite
  { tcCompositeType :: !TcType,
    tcCompositeParts :: ![TcType]
  }
  deriving (Eq, Show)

-- | The type of a section or of a record dot projection, and the type of
-- the argument that it takes. A postfix operator section takes no
-- argument.
data TcSection = TcSection
  { tcSectionType :: !TcType,
    tcSectionArgumentType :: !(Maybe TcType)
  }
  deriving (Eq, Show)

-- | The tuple of the variables that a statement binds: the tuple type and
-- the types of the variables. The term types are the key type of a
-- @then ... by@ statement, or the block type of a @rec@ statement.
data TcBoundTuple = TcBoundTuple
  { tcBoundTupleType :: !TcType,
    tcBoundTupleBinderTypes :: ![TcType],
    tcBoundTupleTermTypes :: ![TcType]
  }
  deriving (Eq, Show)

-- | The type of a binder. A generalized binder of a pattern binding also
-- has the type variables that it abstracts and the type arguments that
-- its right-hand side takes.
data TcBinderAnnotation = TcBinderAnnotation
  { tcBinderAnnType :: !TcType,
    tcBinderAnnTypeBinders :: ![TyVarId],
    tcBinderAnnTypeArgs :: ![TcType]
  }
  deriving (Eq, Show)

-- | The type of a pattern. A constructor pattern also has the type
-- variables that it binds, the type arguments of the constructor, and
-- the evidence of its context.
data TcPatternAnnotation = TcPatternAnnotation
  { tcPatType :: !TcType,
    tcPatTypeBinders :: ![TyVarId],
    tcPatTypeArgs :: ![TcType],
    tcPatEvidence :: ![EvTerm]
  }
  deriving (Eq, Show)

-- | An annotation of the tree that has a general form. A walk can visit
-- each kind of annotation in this one form. The change from the general
-- form keeps only the fields that the annotation has.
class TcAnnotated a where
  toTcAnnotation :: a -> TcAnnotation
  fromTcAnnotation :: TcAnnotation -> a

instance TcAnnotated TcAnnotation where
  toTcAnnotation = id
  fromTcAnnotation = id

instance TcAnnotated TcOccurrence where
  toTcAnnotation (TcOccurrence ty typeArgs evidence inferred) = TcAnnotation ty [] typeArgs evidence [] [] inferred
  fromTcAnnotation annotation = TcOccurrence (tcAnnType annotation) (tcAnnTypeArgs annotation) (tcAnnEvidenceTerms annotation) (tcAnnInferredTypeArgs annotation)

instance TcAnnotated TcAbstraction where
  toTcAnnotation (TcAbstraction ty binders evidence) = TcAnnotation ty binders [] [] evidence [] 0
  fromTcAnnotation annotation = TcAbstraction (tcAnnType annotation) (tcAnnTypeBinders annotation) (tcAnnEvidenceBinders annotation)

instance TcAnnotated TcComposite where
  toTcAnnotation (TcComposite ty parts) = TcAnnotation ty [] parts [] [] [] 0
  fromTcAnnotation annotation = TcComposite (tcAnnType annotation) (tcAnnTypeArgs annotation)

instance TcAnnotated TcSection where
  toTcAnnotation (TcSection ty argument) = TcAnnotation ty [] [] [] [] (maybe [] pure argument) 0
  fromTcAnnotation annotation =
    TcSection
      (tcAnnType annotation)
      ( case tcAnnTermArgTypes annotation of
          argument : _ -> Just argument
          [] -> Nothing
      )

instance TcAnnotated TcBoundTuple where
  toTcAnnotation (TcBoundTuple ty binders terms) = TcAnnotation ty [] binders [] [] terms 0
  fromTcAnnotation annotation = TcBoundTuple (tcAnnType annotation) (tcAnnTypeArgs annotation) (tcAnnTermArgTypes annotation)

instance TcAnnotated TcBinderAnnotation where
  toTcAnnotation (TcBinderAnnotation ty binders typeArgs) = TcAnnotation ty binders typeArgs [] [] [] 0
  fromTcAnnotation annotation = TcBinderAnnotation (tcAnnType annotation) (tcAnnTypeBinders annotation) (tcAnnTypeArgs annotation)

instance TcAnnotated TcPatternAnnotation where
  toTcAnnotation (TcPatternAnnotation ty binders typeArgs evidence) = TcAnnotation ty binders typeArgs evidence [] [] 0
  fromTcAnnotation annotation = TcPatternAnnotation (tcAnnType annotation) (tcAnnTypeBinders annotation) (tcAnnTypeArgs annotation) (tcAnnEvidenceTerms annotation)

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
    -- | For each predicate, the cast from the dictionary of the derived
    -- method to the dictionary that the reused method needs, if the two
    -- types are different.
    tcCoercedMethodPredicateCasts :: ![Maybe Coercion],
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

-- | An annotation that has only a type.
typeAnnotation :: TcType -> TcAnnotation
typeAnnotation ty = TcAnnotation ty [] [] [] [] [] 0

-- | The annotation of an occurrence before the solver has finished: its
-- type, its type arguments, the evidence variables of its wanted
-- constraints, and its term argument types.
pendingAnnotation :: TcType -> [TcType] -> [EvVar] -> [TcType] -> TcAnnotation
pendingAnnotation ty typeArgs evidenceVars termArgTypes =
  TcAnnotation ty [] typeArgs (map EvVarTerm evidenceVars) [] termArgTypes 0

-- | The annotation of a type abstraction before the solver has finished.
pendingTypeLambdaAnnotation :: TcType -> [TyVarId] -> [EvVar] -> TcAnnotation
pendingTypeLambdaAnnotation ty binders evidenceBinders =
  TcAnnotation ty binders [] [] (map EvVarTerm evidenceBinders) [] 0

-- | A cast before the solver has finished. The proof is the evidence
-- variable of the wanted equality, or its @sym@ for a cast onto the left
-- type. The finalization pass puts the solved proof in its place.
pendingCast :: CastDirection -> TcType -> EvVar -> TcCastAnnotation
pendingCast direction ty evidence =
  case direction of
    CastToRight -> TcCastAnnotation (Just (CoVar evidence)) ty
    CastToLeft -> TcCastAnnotation (Just (Sym (CoVar evidence))) ty

-- | An arity cast before the solver has finished. The proof is the
-- evidence variable of the wanted equality @arrow ~ result@.
pendingMatchArityCast :: TcType -> EvVar -> TcMatchArityCast
pendingMatchArityCast arrow evidence =
  TcMatchArityCast (Just (CoVar evidence)) arrow

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
    -- The kind arguments are invisible, as in the source.
    go _ (TcKindedTyCon tc _) = T.unpack (renderTyConName tc)
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
