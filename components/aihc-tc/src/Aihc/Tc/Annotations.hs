{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Type checker annotations for AST nodes.
--
-- Following the pattern established by @aihc-resolve@, the type checker
-- attaches its results as 'Annotation' values on AST nodes using the
-- existing 'DeclAnn'/'EAnn'/'PAnn'/'TAnn' wrappers.
module Aihc.Tc.Annotations
  ( -- * Annotation type
    TcAnnotation (..),
    TcForeignImportAnnotation (..),
    TcForeignEffect (..),
    TcForeignMarshal (..),
    TcForeignAbiType (..),
    PendingTcAnnotation (..),
    TcClassAnnotation (..),
    TcClassMethodAnnotation (..),
    TcDictBinderAnnotation (..),
    TcInstanceAnnotation (..),
    TcInstanceMethodAnnotation (..),

    -- * Pattern synonyms for extracting annotations
    pattern ETcAnn,
    pattern DTcAnn,
    pattern PTcAnn,
    pattern TTcAnn,

    -- * Helpers
    annotateExpr,
    annotateDecl,
    pendingAnnotation,

    -- * Pretty-printing
    renderPred,
    renderTcType,
    renderTcSignature,
  )
where

import Aihc.Parser.Syntax
  ( Decl (..),
    Expr (..),
    Pattern (..),
    Type (..),
    fromAnnotation,
    mkAnnotation,
  )
import Aihc.Tc.Evidence (EvTerm, EvVar)
import Aihc.Tc.Types (Pred (..), TcType (..), TyCon (..), TyVarId (..), Unique (..))
import Data.Text (Text)
import Data.Text qualified as T

-- | Annotation attached to AST nodes by the type checker.
--
-- Not every field is populated for every node. A variable reference gets
-- a type; a top-level binding gets the generalized scheme, etc.
data TcAnnotation = TcAnnotation
  { -- | The inferred/checked type of this node.
    tcAnnType :: !TcType,
    -- | Type arguments made explicit at this occurrence.
    tcAnnTypeArgs :: ![TcType],
    -- | Evidence terms whose dictionaries must be passed at this occurrence.
    tcAnnEvidenceTerms :: ![EvTerm],
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
    tcForeignEffect :: !TcForeignEffect
  }
  deriving (Eq, Show)

-- | Whether a raw foreign call is pure or explicitly threads the real-world
-- state token.
data TcForeignEffect
  = TcForeignPure
  | TcForeignRealWorld
  deriving (Eq, Show)

-- | A source value's path to its primitive ABI representation.  Constructor
-- names are ordered outermost to innermost; for example, a @CInt@ is lowered
-- through @CInt@ and @I32#@ to @Int32#@.
data TcForeignMarshal = TcForeignMarshal
  { tcForeignSourceType :: !TcType,
    tcForeignPrimitiveType :: !TcType,
    tcForeignConstructors :: ![Text],
    tcForeignAbiType :: !TcForeignAbiType
  }
  deriving (Eq, Show)

-- | Primitive values understood by the C ABI bridge.  This is deliberately
-- independent from lifted Haskell wrapper types.
data TcForeignAbiType
  = TcForeignInt32
  | TcForeignWord64
  | TcForeignAddr
  deriving (Eq, Show)

-- | Type-checker annotation payload before constraint solving has finished.
--
-- The generator attaches this directly to the syntax node that produced it.
-- A finalization pass zonks the types and resolves evidence variables into
-- ordinary 'TcAnnotation' values after solving.
data PendingTcAnnotation = PendingTcAnnotation
  { pendingTcAnnType :: !TcType,
    pendingTcAnnTypeArgs :: ![TcType],
    pendingTcAnnEvidenceVars :: ![EvVar],
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

newtype TcClassAnnotation = TcClassAnnotation
  { tcClassMethods :: [TcClassMethodAnnotation]
  }
  deriving (Eq, Show)

data TcInstanceAnnotation = TcInstanceAnnotation
  { tcInstanceDictName :: !Text,
    tcInstanceDictType :: !TcType,
    tcInstanceTyVars :: ![TyVarId],
    tcInstanceHeadTypes :: ![TcType],
    tcInstanceContextDicts :: ![TcDictBinderAnnotation],
    tcInstanceMethodOrder :: ![Text]
  }
  deriving (Eq, Show)

data TcInstanceMethodAnnotation = TcInstanceMethodAnnotation
  { tcInstanceMethodName :: !Text,
    tcInstanceMethodType :: !TcType
  }
  deriving (Eq, Show)

-- | Extract a 'TcAnnotation' from an 'Expr'.
pattern ETcAnn :: TcAnnotation -> Expr -> Expr
pattern ETcAnn ann inner <- EAnn (fromAnnotation -> Just ann) inner

-- | Extract a 'TcAnnotation' from a 'Decl'.
pattern DTcAnn :: TcAnnotation -> Decl -> Decl
pattern DTcAnn ann inner <- DeclAnn (fromAnnotation -> Just ann) inner

-- | Extract a 'TcAnnotation' from a 'Pattern'.
pattern PTcAnn :: TcAnnotation -> Pattern -> Pattern
pattern PTcAnn ann inner <- PAnn (fromAnnotation -> Just ann) inner

-- | Extract a 'TcAnnotation' from a 'Type'.
pattern TTcAnn :: TcAnnotation -> Type -> Type
pattern TTcAnn ann inner <- TAnn (fromAnnotation -> Just ann) inner

-- | Wrap an expression with a type annotation.
annotateExpr :: TcAnnotation -> Expr -> Expr
annotateExpr ann = EAnn (mkAnnotation ann)

-- | Wrap a declaration with a type annotation.
annotateDecl :: TcAnnotation -> Decl -> Decl
annotateDecl ann = DeclAnn (mkAnnotation ann)

pendingAnnotation :: TcType -> [TcType] -> [EvVar] -> [TcType] -> PendingTcAnnotation
pendingAnnotation = PendingTcAnnotation

-- | Render a binder and its 'TcType' as a human-readable signature.
renderTcSignature :: Text -> TcType -> String
renderTcSignature name ty = T.unpack name ++ " ∷ " ++ renderTcType ty

-- | Render a class or equality predicate as source-like text.
renderPred :: Pred -> String
renderPred pred' =
  case pred' of
    ClassPred className args ->
      renderTcType (TcTyCon (TyCon className (length args)) args)
    EqPred left right ->
      renderTcType left ++ " ~ " ++ renderTcType right

-- | Render a 'TcType' as a human-readable string.
--
-- Uses a precedence level to decide when to insert parentheses:
--   0 = no parens needed (top level or right of ->)
--   1 = parens needed for function types (left of ->)
--   2 = parens needed for function types and type applications (inside type con args)
renderTcType :: TcType -> String
renderTcType = go 0
  where
    go :: Int -> TcType -> String
    go _ (TcTyVar tv) = T.unpack (tvName tv)
    go _ (TcMetaTv (Unique u)) = "?" ++ show u
    go _ (TcTyCon tc []) = T.unpack (tyConName tc)
    go _ (TcTyCon (TyCon name 1) [arg])
      | name == T.pack "[]" = "[" ++ go 0 arg ++ "]"
    go _ (TcTyCon (TyCon name arity) args)
      | isBoxedTupleCon name arity,
        arity == length args =
          "(" ++ commaSep (map (go 0) args) ++ ")"
    go _ (TcTyCon (TyCon name arity) args)
      | isUnboxedTupleCon name arity,
        arity == length args =
          "(# " ++ commaSep (map (go 0) args) ++ " #)"
    go p (TcTyCon tc args) =
      parenIf (p >= 2) $
        unwords (T.unpack (tyConName tc) : map (go 2) args)
    go p (TcFunTy a b) =
      parenIf (p >= 1) $
        go 1 a ++ " → " ++ go 0 b
    go p (TcForAllTy tv body) =
      let (tvs, inner) = collectForAlls body
       in parenIf (p >= 1) $
            "∀ " ++ unwords (map (T.unpack . tvName) (tv : tvs)) ++ ". " ++ go 0 inner
    go p (TcQualTy preds body) =
      parenIf (p >= 1) $
        "(" ++ unwords (map showPred preds) ++ ") ⇒ " ++ go 0 body
    go p (TcAppTy f a) =
      parenIf (p >= 2) $
        go 1 f ++ " " ++ go 2 a

    showPred (ClassPred cls args) =
      T.unpack cls ++ " " ++ unwords (map (go 2) args)
    showPred (EqPred t1 t2) =
      go 2 t1 ++ " ~ " ++ go 2 t2

    parenIf False s = s
    parenIf True s = "(" ++ s ++ ")"

    commaSep = T.unpack . T.intercalate (T.pack ", ") . map T.pack

    isBoxedTupleCon name arity =
      name == T.pack "(" <> commas arity <> T.pack ")"

    isUnboxedTupleCon name arity =
      name == T.pack "(#" <> commas arity <> T.pack "#)"

    commas arity
      | arity <= 1 = T.empty
      | otherwise = T.replicate (arity - 1) (T.pack ",")

-- | Collect nested forall binders into a list.
collectForAlls :: TcType -> ([TyVarId], TcType)
collectForAlls (TcForAllTy tv body) =
  let (tvs, inner) = collectForAlls body
   in (tv : tvs, inner)
collectForAlls ty = ([], ty)
