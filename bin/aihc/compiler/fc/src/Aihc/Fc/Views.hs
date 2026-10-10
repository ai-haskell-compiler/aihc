{-# LANGUAGE ViewPatterns #-}

-- | Views of System FC expressions that several passes share: the spine
-- of an application, the value names an expression uses, and the largest
-- local unique of a program.
module Aihc.Fc.Views
  ( Arg,
    isConstructorName,
    collectSpine,
    castedSpine,
    exprValueNames,
    maxLocalUnique,
  )
where

import Aihc.Fc.Imports (declReferences)
import Aihc.Fc.Name
import Aihc.Fc.Syntax
import Aihc.Tc.Types (Unique (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

-- | An argument of an application: a type or a value.
type Arg = Either Type Expr

-- | Whether a name is a data constructor. The sort of the name tells it,
-- in the desugarer's output and in a parsed program alike.
isConstructorName :: Name -> Bool
isConstructorName name = nameSort name == SortDataConstructor

collectSpine :: Expr -> (Expr, [Arg])
collectSpine = go []
  where
    go args expr =
      case expr of
        ExApp function argument -> go (Right argument : args) function
        ExTyApp function ty -> go (Left ty : args) function
        _ -> (expr, args)

-- | Collect an application spine through the casts on its head. A cast
-- is erased in the lowered code, so it neither hides a call nor stands
-- between a function and the arguments a call gives it.
castedSpine :: Expr -> (Expr, [Arg])
castedSpine = go []
  where
    go args expr =
      case expr of
        ExApp function argument -> go (Right argument : args) function
        ExTyApp function ty -> go (Left ty : args) function
        ExCast body _ -> go args body
        _ -> (expr, args)

-- | The value-class names an expression uses.
exprValueNames :: Expr -> Set Name
exprValueNames = go
  where
    go expr =
      case expr of
        ExVar name -> Set.singleton name
        ExLit {} -> Set.empty
        ExCoercion {} -> Set.empty
        ExApp function argument -> go function <> go argument
        ExTyApp function _ -> go function
        ExLam _ body -> go body
        ExTyLam _ body -> go body
        ExLet bind body -> go (bindRhs bind) <> go body
        ExRec binds body -> foldMap (go . bindRhs) binds <> go body
        ExCase scrutinee _ (NE.toList -> alternatives) -> go scrutinee <> foldMap (go . altRhs) alternatives
        ExAbsurd scrutinee _ -> go scrutinee
        ExCast body _ -> go body
        ExForeignCall _ _ arguments -> foldMap go arguments

-- | The largest local unique of the program.
maxLocalUnique :: Program -> Int
maxLocalUnique program = maximum (0 : mapMaybe localValue (Set.toList names))
  where
    names = foldMap declReferences (programDecls program) <> foldMap declBinderNames (programDecls program)
    localValue name =
      case nameOrigin name of
        OriginLocal (Unique unique) -> Just unique
        OriginTop {} -> Nothing

declBinderNames :: Decl -> Set Name
declBinderNames decl =
  case decl of
    DeclVal declaration -> exprBinderNames (valBody declaration) <> typeBinderNames (valType declaration)
    DeclRule declaration ->
      Set.fromList (map binderName (ruleTypeBinders declaration <> ruleBinders declaration))
        <> foldMap (typeBinderNames . binderType) (ruleTypeBinders declaration <> ruleBinders declaration)
        <> typeBinderNames (ruleType declaration)
        <> exprBinderNames (ruleLhs declaration)
        <> exprBinderNames (ruleRhs declaration)
    DeclType declaration -> Set.fromList (map binderName (typeBinders declaration)) <> foldMap (typeBinderNames . conType) (typeCons declaration)
    DeclSynonym declaration -> Set.fromList (map binderName (synBinders declaration)) <> typeBinderNames (synBody declaration)
    DeclAxiom declaration -> Set.fromList (map binderName (axiomBinders declaration))

typeBinderNames :: Type -> Set Name
typeBinderNames ty =
  case ty of
    TyVar {} -> Set.empty
    TyCon {} -> Set.empty
    TyLit {} -> Set.empty
    TyApp function argument -> typeBinderNames function <> typeBinderNames argument
    TyFun r1 r2 argument result -> foldMap typeBinderNames [r1, r2, argument, result]
    TyForAll binder body -> Set.insert (binderName binder) (typeBinderNames (binderType binder) <> typeBinderNames body)
    TyEq left right -> typeBinderNames left <> typeBinderNames right

exprBinderNames :: Expr -> Set Name
exprBinderNames = go
  where
    binderNames binder = Set.insert (binderName binder) (typeBinderNames (binderType binder))
    go expr =
      case expr of
        ExVar {} -> Set.empty
        ExLit _ ty -> typeBinderNames ty
        ExCoercion {} -> Set.empty
        ExApp function argument -> go function <> go argument
        ExTyApp function ty -> go function <> typeBinderNames ty
        ExLam binder body -> binderNames binder <> go body
        ExTyLam binder body -> binderNames binder <> go body
        ExLet bind body -> binderNames (bindBinder bind) <> go (bindRhs bind) <> go body
        ExRec binds body -> foldMap (\bind -> binderNames (bindBinder bind) <> go (bindRhs bind)) binds <> go body
        ExAbsurd scrutinee resultType -> go scrutinee <> typeBinderNames resultType
        ExCase scrutinee binder (NE.toList -> alternatives) ->
          go scrutinee <> foldMap binderNames binder <> foldMap altNames alternatives
        ExCast body _ -> go body
        ExForeignCall call types arguments -> typeBinderNames (foreignCallType call) <> foldMap typeBinderNames types <> foldMap go arguments
    altNames alternative = foldMap binderNames (altTypeBinders alternative <> altBinders alternative) <> go (altRhs alternative)
