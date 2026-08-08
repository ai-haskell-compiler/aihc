-- | Canonical, lossless pretty-printer for System FC.
--
-- The rendered language is deliberately explicit: identities, kinds, runtime
-- representations, imported names, and coercions are syntax rather than
-- out-of-band compiler state.  "Aihc.Fc.Parser" accepts exactly this format.
module Aihc.Fc.Pretty
  ( renderProgram,
    renderExpr,
    renderType,
    renderTopBind,
  )
where

import Aihc.Fc.Syntax
import Aihc.Tc.Evidence (Coercion (..), EvVar (..))
import Aihc.Tc.Types
  ( Kind (..),
    Levity (..),
    Pred (..),
    RuntimeRep (..),
    TcType (..),
    TyCon,
    TyVarId,
    Unique (..),
    VecCount (..),
    VecElem (..),
    tvKind,
    tvName,
    tvUnique,
    tyConArity,
    tyConKind,
    tyConName,
  )
import Data.ByteString qualified as BS
import Data.List (intercalate)
import Data.Text (Text)
import Data.Text qualified as T

renderProgram :: FcProgram -> String
renderProgram = intercalate "\n" . map renderTopBind . fcTopBinds

renderTopBind :: FcTopBind -> String
renderTopBind topBind =
  case topBind of
    FcData name tyVars constructors ->
      tagged "data" [renderText name, renderList renderTyVar tyVars, renderList renderConstructor constructors]
    FcAxiom declaration -> tagged "axiom" [renderAxiomDecl declaration]
    FcNewtype declaration -> tagged "newtype" [renderNewtypeDecl declaration]
    FcPrimitive var arity -> tagged "primitive" [renderVar var, show arity]
    FcForeignImport foreignCall -> tagged "foreign-import" [renderForeignCall foreignCall]
    FcTopBind bind -> tagged "top-bind" [renderBind bind]

renderConstructor :: (Text, [TcType]) -> String
renderConstructor (name, fields) = tagged "constructor" [renderText name, renderList renderType fields]

renderAxiomDecl :: FcAxiomDecl -> String
renderAxiomDecl declaration =
  tagged
    "axiom-decl"
    [ renderText (fcAxiomName declaration),
      renderList renderTyVar (fcAxiomTyVars declaration),
      renderAxiomRole (fcAxiomRole declaration),
      renderType (fcAxiomLeft declaration),
      renderType (fcAxiomRight declaration)
    ]

renderAxiomRole :: FcAxiomRole -> String
renderAxiomRole role =
  case role of
    FcNominal -> "nominal"
    FcRepresentational -> "representational"

renderNewtypeDecl :: FcNewtypeDecl -> String
renderNewtypeDecl declaration =
  tagged
    "newtype-decl"
    [ renderText (fcNewtypeName declaration),
      renderList renderTyVar (fcNewtypeTyVars declaration),
      renderText (fcNewtypeConstructor declaration),
      renderType (fcNewtypeRepresentation declaration),
      renderType (fcNewtypeResult declaration)
    ]

renderForeignCall :: FcForeignCall -> String
renderForeignCall foreignCall =
  tagged
    "foreign-call"
    [ renderText (fcForeignCallName foreignCall),
      renderText (fcForeignCallSymbol foreignCall),
      renderForeignSignature (fcForeignCallSignature foreignCall)
    ]

renderForeignSignature :: FcForeignSignature -> String
renderForeignSignature signature =
  tagged
    "foreign-signature"
    [ renderList renderForeignType (fcForeignArgumentTypes signature),
      renderForeignType (fcForeignResultType signature),
      renderForeignEffect (fcForeignEffect signature)
    ]

renderForeignEffect :: FcForeignEffect -> String
renderForeignEffect effect =
  case effect of
    FcForeignPure -> "pure"
    FcForeignRealWorld -> "real-world"

renderForeignType :: FcForeignType -> String
renderForeignType foreignType =
  case foreignType of
    FcForeignInt -> "int"
    FcForeignInt32 -> "int32"
    FcForeignWord64 -> "word64"
    FcForeignAddr -> "addr"

renderBind :: FcBind -> String
renderBind bind =
  case bind of
    FcNonRec var expression -> tagged "non-rec" [renderVar var, renderExpr expression]
    FcRec bindings -> tagged "rec" [renderList renderBinding bindings]
  where
    renderBinding (var, expression) = tagged "binding" [renderVar var, renderExpr expression]

renderExpr :: FcExpr -> String
renderExpr expression =
  case expression of
    FcVar var -> tagged "var-expr" [renderVar var]
    FcLit literal -> tagged "lit" [renderLiteral literal]
    FcApp function argument -> tagged "app" [renderExpr function, renderExpr argument]
    FcTyApp function argument -> tagged "type-app-expr" [renderExpr function, renderType argument]
    FcLam var body -> tagged "lambda" [renderVar var, renderExpr body]
    FcTyLam tyVar body -> tagged "type-lambda" [renderTyVar tyVar, renderExpr body]
    FcLet bind body -> tagged "let" [renderBind bind, renderExpr body]
    FcCase scrutinee binder alternatives ->
      tagged "case" [renderExpr scrutinee, renderVar binder, renderList renderAlt alternatives]
    FcCast body coercion -> tagged "cast" [renderExpr body, renderCoercion coercion]
    FcCallForeign foreignCall arguments ->
      tagged "call-foreign" [renderForeignCall foreignCall, renderList renderExpr arguments]

renderAlt :: FcAlt -> String
renderAlt alternative =
  tagged
    "alt"
    [ renderAltCon (altCon alternative),
      renderList renderVar (altBinders alternative),
      renderExpr (altRhs alternative)
    ]

renderAltCon :: FcAltCon -> String
renderAltCon alternative =
  case alternative of
    DataAlt name -> tagged "data-alt" [renderText name]
    LitAlt literal -> tagged "lit-alt" [renderLiteral literal]
    DefaultAlt -> "default-alt"

renderVar :: Var -> String
renderVar var =
  tagged
    "var"
    [ renderText (varName var),
      renderUnique (varUnique var),
      renderType (varType var),
      renderMaybe renderText (varResolvedName var)
    ]

renderLiteral :: Literal -> String
renderLiteral literal =
  case literal of
    LitInt runtimeRep value -> tagged "int-literal" [renderRuntimeRep runtimeRep, show value]
    LitChar runtimeRep value -> tagged "char-literal" [renderRuntimeRep runtimeRep, show value]
    LitString value -> tagged "string-literal" [renderText value]
    LitAddr value -> tagged "addr-literal" [renderList show (BS.unpack value)]

renderType :: TcType -> String
renderType ty =
  case ty of
    TcTyVar tyVar -> tagged "type-var" [renderTyVar tyVar]
    TcMetaTv unique -> tagged "meta-type" [renderUnique unique]
    TcTyCon tyCon arguments -> tagged "type-con" [renderTyCon tyCon, renderList renderType arguments]
    TcFunTy argument result -> tagged "function-type" [renderType argument, renderType result]
    TcForAllTy tyVar body -> tagged "forall-type" [renderTyVar tyVar, renderType body]
    TcQualTy predicates body -> tagged "qualified-type" [renderList renderPred predicates, renderType body]
    TcAppTy function argument -> tagged "type-app" [renderType function, renderType argument]

renderPred :: Pred -> String
renderPred predicate =
  case predicate of
    ClassPred name arguments -> tagged "class-pred" [renderText name, renderList renderType arguments]
    EqPred left right -> tagged "equality-pred" [renderType left, renderType right]

renderTyVar :: TyVarId -> String
renderTyVar tyVar =
  tagged "ty-var" [renderText (tvName tyVar), renderUnique (tvUnique tyVar), renderKind (tvKind tyVar)]

renderTyCon :: TyCon -> String
renderTyCon tyCon =
  tagged "ty-con" [renderText (tyConName tyCon), show (tyConArity tyCon), renderKind (tyConKind tyCon)]

renderKind :: Kind -> String
renderKind kind =
  case kind of
    KTYPE runtimeRep -> tagged "type-kind" [renderRuntimeRep runtimeRep]
    KConstraint -> "constraint-kind"
    KRuntimeRep -> "runtime-rep-kind"
    KLevity -> "levity-kind"
    KVecCount -> "vec-count-kind"
    KVecElem -> "vec-elem-kind"
    KFun argument result -> tagged "kind-function" [renderKind argument, renderKind result]
    KMeta unique -> tagged "meta-kind" [renderUnique unique]

renderRuntimeRep :: RuntimeRep -> String
renderRuntimeRep runtimeRep =
  case runtimeRep of
    VecRep count element -> tagged "vec-rep" [renderVecCount count, renderVecElem element]
    TupleRep fields -> tagged "tuple-rep" [renderList renderRuntimeRep fields]
    SumRep fields -> tagged "sum-rep" [renderList renderRuntimeRep fields]
    BoxedRep levity -> tagged "boxed-rep" [renderLevity levity]
    IntRep -> "int-rep"
    Int8Rep -> "int8-rep"
    Int16Rep -> "int16-rep"
    Int32Rep -> "int32-rep"
    Int64Rep -> "int64-rep"
    WordRep -> "word-rep"
    Word8Rep -> "word8-rep"
    Word16Rep -> "word16-rep"
    Word32Rep -> "word32-rep"
    Word64Rep -> "word64-rep"
    AddrRep -> "addr-rep"
    FloatRep -> "float-rep"
    DoubleRep -> "double-rep"
    RuntimeRepVar unique -> tagged "runtime-rep-var" [renderUnique unique]
    RuntimeRepMeta unique -> tagged "runtime-rep-meta" [renderUnique unique]

renderLevity :: Levity -> String
renderLevity levity =
  case levity of
    Lifted -> "lifted"
    Unlifted -> "unlifted"

renderVecCount :: VecCount -> String
renderVecCount count =
  case count of
    Vec2 -> "vec2"
    Vec4 -> "vec4"
    Vec8 -> "vec8"
    Vec16 -> "vec16"
    Vec32 -> "vec32"
    Vec64 -> "vec64"

renderVecElem :: VecElem -> String
renderVecElem element =
  case element of
    Int8ElemRep -> "int8-elem-rep"
    Int16ElemRep -> "int16-elem-rep"
    Int32ElemRep -> "int32-elem-rep"
    Int64ElemRep -> "int64-elem-rep"
    Word8ElemRep -> "word8-elem-rep"
    Word16ElemRep -> "word16-elem-rep"
    Word32ElemRep -> "word32-elem-rep"
    Word64ElemRep -> "word64-elem-rep"
    FloatElemRep -> "float-elem-rep"
    DoubleElemRep -> "double-elem-rep"

renderCoercion :: Coercion -> String
renderCoercion coercion =
  case coercion of
    CoVar (EvVar unique) -> tagged "co-var" [renderUnique unique]
    Refl ty -> tagged "refl" [renderType ty]
    Sym inner -> tagged "sym" [renderCoercion inner]
    Trans left right -> tagged "trans" [renderCoercion left, renderCoercion right]
    TyConAppCo tyCon arguments -> tagged "ty-con-app-co" [renderTyCon tyCon, renderList renderCoercion arguments]
    AxiomInstCo name arguments -> tagged "axiom-inst-co" [renderText name, renderList renderType arguments]

renderUnique :: Unique -> String
renderUnique (Unique value) = show value

renderText :: Text -> String
renderText = show . T.unpack

renderMaybe :: (a -> String) -> Maybe a -> String
renderMaybe _ Nothing = "none"
renderMaybe render (Just value) = tagged "some" [render value]

renderList :: (a -> String) -> [a] -> String
renderList render values = "[" <> intercalate "," (map render values) <> "]"

tagged :: String -> [String] -> String
tagged name arguments = name <> "(" <> intercalate "," arguments <> ")"
