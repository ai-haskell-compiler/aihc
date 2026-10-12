{-# LANGUAGE ViewPatterns #-}

-- | Canonical forms that make equal parts of a program equal as values.
--
-- The passes give the binders of a foreign type new names at each copy of
-- a call. Thus two calls of one primitive can have types that are equal
-- except for the names of their binders. 'canonicalForeignCalls' gives the
-- binders of each foreign call type one fixed scheme of names, so these
-- calls become equal. The printer can then give them one alias, and the
-- parser gives back the same program.
module Aihc.Fc.Canonical
  ( canonicalForeignCalls,
    canonicalForeignType,
  )
where

import Aihc.Fc.Name
import Aihc.Fc.Syntax
import Aihc.Tc.Types (Unique (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

-- | Give the type of each foreign call in the program its canonical form.
canonicalForeignCalls :: Program -> Program
canonicalForeignCalls program = program {programDecls = map declaration (programDecls program)}
  where
    declaration decl = case decl of
      DeclVal value -> DeclVal value {valBody = expression (valBody value)}
      DeclRule value -> DeclRule value {ruleLhs = expression (ruleLhs value), ruleRhs = expression (ruleRhs value)}
      _ -> decl

expression :: Expr -> Expr
expression expr = case expr of
  ExApp function argument -> ExApp (expression function) (expression argument)
  ExTyApp function argument -> ExTyApp (expression function) argument
  ExLam binder body -> ExLam binder (expression body)
  ExTyLam binder body -> ExTyLam binder (expression body)
  ExLet binding body -> ExLet (bind binding) (expression body)
  ExRec bindings body -> ExRec (map bind bindings) (expression body)
  ExCase scrutinee binder (NE.toList -> alternatives) ->
    ExCase (expression scrutinee) binder (NE.fromList [alternative {altRhs = expression (altRhs alternative)} | alternative <- alternatives])
  ExAbsurd scrutinee resultType -> ExAbsurd (expression scrutinee) resultType
  ExCast body proof -> ExCast (expression body) proof
  ExForeignCall call types arguments ->
    ExForeignCall call {foreignCallType = canonicalForeignType (foreignCallType call)} types (map expression arguments)
  _ -> expr
  where
    bind binding = binding {bindRhs = expression (bindRhs binding)}

-- | Rename each bound type variable of a type to a fixed scheme: the
-- binders get the names @a@, @b@, … @z@, @aa@, @ab@, … in the order of
-- the type, each with the unique 0. A foreign type has no free variable. A
-- type with a free variable stays as it is, because a new binder name could
-- capture the free variable.
canonicalForeignType :: Type -> Type
canonicalForeignType ty
  | Set.null (freeVariables ty) = fst (go Map.empty 0 ty)
  | otherwise = ty
  where
    freeVariables current = case current of
      TyVar name -> Set.singleton name
      TyApp function argument -> freeVariables function <> freeVariables argument
      TyFun r1 r2 argument result -> foldMap freeVariables [r1, r2, argument, result]
      TyEq left right -> freeVariables left <> freeVariables right
      TyForAll (Binder name kind) body -> freeVariables kind <> Set.delete name (freeVariables body)
      _ -> mempty
    go :: Map.Map Name Name -> Int -> Type -> (Type, Int)
    go renaming count current = case current of
      TyVar name -> (TyVar (Map.findWithDefault name name renaming), count)
      TyApp function argument ->
        let (function', count1) = go renaming count function
            (argument', count2) = go renaming count1 argument
         in (TyApp function' argument', count2)
      TyFun r1 r2 argument result ->
        let (r1', count1) = go renaming count r1
            (r2', count2) = go renaming count1 r2
            (argument', count3) = go renaming count2 argument
            (result', count4) = go renaming count3 result
         in (TyFun r1' r2' argument' result', count4)
      TyEq left right ->
        let (left', count1) = go renaming count left
            (right', count2) = go renaming count1 right
         in (TyEq left' right', count2)
      TyForAll (Binder name kind) body ->
        let (kind', count1) = go renaming count kind
            name' = Name (binderText count1) (nameSort name) (OriginLocal (Unique 0))
            (body', count2) = go (Map.insert name name' renaming) (count1 + 1) body
         in (TyForAll (Binder name' kind') body', count2)
      _ -> (current, count)

-- | The text of the binder at a position: @a@ … @z@, then @aa@, @ab@, ….
binderText :: Int -> Text
binderText index
  | index < 26 = T.singleton (letter index)
  | otherwise = binderText (index `div` 26 - 1) <> T.singleton (letter (index `mod` 26))
  where
    letter offset = toEnum (fromEnum 'a' + offset)
