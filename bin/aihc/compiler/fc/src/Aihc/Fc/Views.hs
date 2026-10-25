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
    exprMentionsAny,
    maxLocalUnique,
  )
where

import Aihc.Fc.Name
import Aihc.Fc.Syntax
import Aihc.Tc.Types (Unique (..))
import Data.List.NonEmpty qualified as NE
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

-- | Whether an expression uses one of the given value-class names. This is
-- @not (Set.disjoint names (exprValueNames expr))@ without the set of the
-- expression: the walk stops at the first use.
exprMentionsAny :: Set Name -> Expr -> Bool
exprMentionsAny names = go
  where
    go expr =
      case expr of
        ExVar name -> Set.member name names
        ExLit {} -> False
        ExCoercion {} -> False
        ExApp function argument -> go function || go argument
        ExTyApp function _ -> go function
        ExLam _ body -> go body
        ExTyLam _ body -> go body
        ExLet bind body -> go (bindRhs bind) || go body
        ExRec binds body -> any (go . bindRhs) binds || go body
        ExCase scrutinee _ alternatives -> go scrutinee || any (go . altRhs) alternatives
        ExAbsurd scrutinee _ -> go scrutinee
        ExCast body _ -> go body
        ExForeignCall _ _ arguments -> any go arguments

-- | The largest local unique of the program.
-- Compute the maximum directly. No set of names is necessary.
maxLocalUnique :: Program -> Int
maxLocalUnique = largest declaration . programDecls
  where
    largest f = foldl' (\result value -> max result (f value)) 0
    nameUnique name = case nameOrigin name of
      OriginLocal (Unique unique) -> max 0 unique
      OriginTop {} -> 0
    declaration decl = case decl of
      DeclVal value -> max (ty True (valType value)) (expression (valBody value))
      DeclRule rule -> maximum [largest (binder True) (ruleTypeBinders rule <> ruleBinders rule), ty True (ruleType rule), expression (ruleLhs rule), expression (ruleRhs rule)]
      DeclType value -> maximum [largest (binder False) (typeBinders value), ty False (typeResult value), largest (ty True . conType) (typeCons value)]
      DeclSynonym value -> maximum [largest (binder False) (synBinders value), ty False (synResult value), ty True (synBody value)]
      DeclAxiom value -> maximum [largest (binder False) (axiomBinders value), ty False (axiomLeft value), ty False (axiomRight value)]
    binder includeTypes value = max (nameUnique (binderName value)) (ty includeTypes (binderType value))
    -- Keep the same type-binder scope as the reference and binder walks.
    ty includeBinders value = case value of
      TyVar name -> nameUnique name
      TyCon name -> nameUnique name
      TyLit name _ -> nameUnique name
      TyApp function argument -> max (ty includeBinders function) (ty includeBinders argument)
      TyFun r1 r2 argument result -> largest (ty includeBinders) [r1, r2, argument, result]
      TyForAll bound body -> maximum [if includeBinders then nameUnique (binderName bound) else 0, ty includeBinders (binderType bound), ty includeBinders body]
      TyEq left right -> max (ty includeBinders left) (ty includeBinders right)
    expression expr = case expr of
      ExVar name -> nameUnique name
      ExLit value resultType -> max (literal value) (ty True resultType)
      ExCoercion proof -> coercion proof
      ExApp function argument -> max (expression function) (expression argument)
      ExTyApp function argument -> max (expression function) (ty True argument)
      ExLam bound body -> max (binder True bound) (expression body)
      ExTyLam bound body -> max (binder True bound) (expression body)
      ExLet bound body -> max (binding bound) (expression body)
      ExRec binds body -> max (largest binding binds) (expression body)
      ExAbsurd scrutinee resultType -> max (expression scrutinee) (ty True resultType)
      ExCase scrutinee bound alternatives -> maximum [expression scrutinee, largest (binder True) bound, largest alternative alternatives]
      ExCast body proof -> max (expression body) (coercion proof)
      ExForeignCall call types arguments -> maximum [ty True (foreignCallType call), largest dependency (foreignCallDependencies call), largest (ty True) types, largest expression arguments]
    binding value = max (binder True (bindBinder value)) (expression (bindRhs value))
    alternative value = maximum [patternUnique (altCon value), largest (binder True) (altTypeBinders value <> altBinders value), expression (altRhs value)]
    patternUnique con = case con of
      AltData name -> nameUnique name
      AltLit value -> literal value
      AltDefault -> 0
    literal value = case value of
      LitInt representation _ -> ty False representation
      LitChar representation _ -> ty False representation
      LitAddr representation _ -> ty False representation
    dependency value = case value of
      ForeignAxiom name -> nameUnique name
      ForeignConstructor name -> nameUnique name
    coercion proof = case proof of
      CoVar name -> nameUnique name
      CoRefl value -> ty False value
      CoSym inner -> coercion inner
      CoTrans left right -> max (coercion left) (coercion right)
      CoApp function argument -> max (coercion function) (coercion argument)
      CoFun domain range -> max (coercion domain) (coercion range)
      CoForAll bound body -> max (ty False (binderType bound)) (coercion body)
      CoNth _ inner -> coercion inner
      CoTyConApp name arguments -> max (nameUnique name) (largest coercion arguments)
      CoAxiom name arguments -> max (nameUnique name) (largest (ty False) arguments)
