{-# LANGUAGE OverloadedStrings #-}

-- | Express local list producers with foldr/build before list fusion.
module Aihc.Fc.ListFusion (listFusionProgram) where

import Aihc.Fc.Imports (pruneImports)
import Aihc.Fc.Name
import Aihc.Fc.Simplify (collectSpine, exprValueNames, maxLocalUnique)
import Aihc.Fc.Syntax
import Aihc.Fc.TypeOf (TypeEnv, typeEnvFromProgram, typesEqual)
import Aihc.Resolve (PackageId)
import Aihc.Tc.Types (Unique (..))
import Control.Monad (guard)
import Control.Monad.Trans.State.Strict (State, runState, state)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)

-- | The library functions must already occur in the program.
-- A module without these definitions retains its original producers.
listFusionProgram :: PackageId -> Program -> (Program, Int)
listFusionProgram primPackage program =
  case libraryPair of
    Just (build, foldrName) ->
      let (decls, (_, count)) = runState (mapM (onDecl build foldrName) (programDecls program)) (maxLocalUnique program + 1, 0)
       in (pruneImports program {programDecls = decls}, count)
    _ -> (program, 0)
  where
    env = typeEnvFromProgram primPackage program
    library text = [value | DeclVal value <- programDecls program, nameText (valName value) == text, OriginTop _ "GHC.Base" <- [nameOrigin (valName value)]]
    -- Check the definitions. A module name alone does not establish semantics.
    libraryPair = listToMaybe $ do
      foldValue <- library "foldr"
      buildValue <- library "build"
      ExTyLam _ (ExTyLam _ (ExLam combine (ExLam zero (ExLam values body)))) <- pure (valBody foldValue)
      (item, rest, _, _, nil, cons, base, step, input) <- maybe [] pure (producer env body)
      guard (base == ExVar (binderName zero) && input == ExVar (binderName values))
      guard (step == ExApp (ExApp (ExVar (binderName combine)) (ExVar (binderName item))) (ExVar (binderName rest)))
      ExTyLam element (ExLam generate built) <- pure (valBody buildValue)
      let elementType = TyVar (binderName element)
          listType = TyApp (TyCon (Name "[]" SortTypeConstructor (nameOrigin nil))) elementType
          expected = ExApp (ExApp (ExTyApp (ExVar (binderName generate)) listType) (ExTyApp (ExVar cons) elementType)) (ExTyApp (ExVar nil) elementType)
      guard (built == expected)
      pure (valName buildValue, valName foldValue)
    onDecl build foldrName declaration@(DeclVal value)
      | valName value == build || valName value == foldrName = pure declaration
      | otherwise = do
          body <- walk env build foldrName (valBody value)
          pure (DeclVal value {valBody = body})
    onDecl _ _ declaration = pure declaration

type Fresh = State (Int, Int)

fresh :: Text -> Sort -> Fresh Name
fresh text sort = state (\(unique, count) -> (Name text sort (OriginLocal (Unique unique)), (unique + 1, count)))

walk :: TypeEnv -> Name -> Name -> Expr -> Fresh Expr
walk env build foldrName expression = do
  result <- case expression of
    ExApp f x -> ExApp <$> go f <*> go x
    ExTyApp f ty -> (`ExTyApp` ty) <$> go f
    ExLam binder body -> ExLam binder <$> go body
    ExTyLam binder body -> ExTyLam binder <$> go body
    ExLet bind body -> ExLet <$> onBind bind <*> go body
    ExRec binds body -> ExRec <$> mapM onBind binds <*> go body
    ExCase value binder alternatives -> ExCase <$> go value <*> pure binder <*> traverse onAlt alternatives
    ExAbsurd value ty -> (`ExAbsurd` ty) <$> go value
    ExCast value co -> (`ExCast` co) <$> go value
    ExForeignCall call types args -> ExForeignCall call types <$> mapM go args
    _ -> pure expression
  case producer env result of
    Nothing -> pure result
    Just (item, tailBinder, inputType, outputType, nil, cons, base, body, input) -> do
      accumulatorName <- fresh "_fold_tail" SortValue
      let accumulator = Binder accumulatorName outputType
          body' = replaceTail (binderName tailBinder) (ExVar accumulatorName) body
          folded = foldCall foldrName inputType outputType (ExLam item (ExLam accumulator body')) base input
      case outputType of
        TyApp (TyCon list) elementType
          | nameText list == "[]",
            nameOrigin list == nameOrigin nil -> do
              resultName <- fresh "_fold_result" SortTypeVariable
              consName <- fresh "_fold_cons" SortValue
              nilName <- fresh "_fold_nil" SortValue
              let kind = kindOfList nil
                  resultType = TyVar resultName
                  resultBinder = Binder resultName kind
                  lifted = liftedRep nil
                  arrow = TyFun lifted lifted
                  combine = Binder consName (arrow elementType (arrow resultType resultType))
                  zero = Binder nilName resultType
                  abstract = abstractResult foldrName nil cons outputType resultType (ExVar consName) (ExVar nilName) (Set.singleton accumulatorName)
                  fused = do
                    base' <- abstract base
                    step' <- abstract body'
                    pure (ExApp (ExTyApp (ExVar build) elementType) (ExTyLam resultBinder (ExLam combine (ExLam zero (foldCall foldrName inputType resultType (ExLam item (ExLam (accumulator {binderType = resultType}) step')) base' input)))))
              state (\(unique, count) -> (fromMaybe folded fused, (unique, count + 1)))
        _ -> state (\(unique, count) -> (folded, (unique, count + 1)))
  where
    go = walk env build foldrName
    onBind bind = (\rhs -> bind {bindRhs = rhs}) <$> go (bindRhs bind)
    onAlt alt = (\rhs -> alt {altRhs = rhs}) <$> go (altRhs alt)

-- | Recognize a structural fold. Reject a use of the original tail or
-- loop outside its recursive call. The step then needs only the element
-- and the result of the tail.
producer :: TypeEnv -> Expr -> Maybe (Binder, Binder, Type, Type, Name, Name, Expr, Expr, Expr)
producer env (ExRec [Bind function (ExLam argument (ExCase (ExVar scrutinee) caseBinder alternatives))] (ExApp (ExVar called) input)) = do
  guard (called == binderName function && scrutinee == binderName argument)
  (nilAlt, consAlt) <- case NE.toList alternatives of
    [a, b] | null (altBinders a) -> Just (a, b)
    [a, b] | null (altBinders b) -> Just (b, a)
    _ -> Nothing
  AltData nil <- pure (altCon nilAlt)
  AltData cons <- pure (altCon consAlt)
  guard (nameText nil == "[]" && nameText cons == ":" && nameOrigin nil == nameOrigin cons)
  [item, tailBinder] <- pure (altBinders consAlt)
  TyFun _ resultRep _ outputType <- pure (binderType function)
  guard (typesEqual env resultRep (liftedRep nil))
  guard (null (altTypeBinders nilAlt) && null (altTypeBinders consAlt))
  body <- removeRecursion (binderName function) (binderName tailBinder) (altRhs consAlt)
  let forbidden = Set.fromList (binderName argument : binderName function : maybe [] (pure . binderName) caseBinder)
  guard (Set.null (forbidden `Set.intersection` (exprValueNames body <> exprValueNames (altRhs nilAlt) <> exprValueNames input)))
  pure (item, tailBinder, binderType item, outputType, nil, cons, altRhs nilAlt, body, input)
producer _ _ = Nothing

-- The tail name is a temporary marker for the recursive result.
removeRecursion :: Name -> Name -> Expr -> Maybe Expr
removeRecursion loop tailName expression = case expression of
  ExApp (ExVar f) (ExVar xs) | f == loop && xs == tailName -> Just (ExVar tailName)
  ExVar name | name == loop || name == tailName -> Nothing
  _ -> traverseExpr (removeRecursion loop tailName) expression

replaceTail :: Name -> Expr -> Expr -> Expr
replaceTail name value expression = case expression of
  ExVar found | found == name -> value
  _ -> fromMaybe expression (traverseExpr (Just . replaceTail name value) expression)

traverseExpr :: (Expr -> Maybe Expr) -> Expr -> Maybe Expr
traverseExpr go expression = case expression of
  ExApp f x -> ExApp <$> go f <*> go x
  ExTyApp f ty -> (`ExTyApp` ty) <$> go f
  ExLam binder body -> ExLam binder <$> go body
  ExTyLam binder body -> ExTyLam binder <$> go body
  ExLet bind body -> ExLet <$> onBind bind <*> go body
  ExRec binds body -> ExRec <$> mapM onBind binds <*> go body
  ExCase value binder alternatives -> ExCase <$> go value <*> pure binder <*> traverse onAlt alternatives
  ExAbsurd value ty -> (`ExAbsurd` ty) <$> go value
  ExCast value co -> (`ExCast` co) <$> go value
  ExForeignCall call types args -> ExForeignCall call types <$> mapM go args
  _ -> Just expression
  where
    onBind bind = (\rhs -> bind {bindRhs = rhs}) <$> go (bindRhs bind)
    onAlt alt = (\rhs -> alt {altRhs = rhs}) <$> go (altRhs alt)

foldCall :: Name -> Type -> Type -> Expr -> Expr -> Expr -> Expr
foldCall name input output step zero values = foldl ExApp (ExTyApp (ExTyApp (ExVar name) input) output) [step, zero, values]

-- | Abstract constructors only at result positions. A use of an abstract
-- tail as data would inspect the list and is not a valid producer.
abstractResult :: Name -> Name -> Name -> Type -> Type -> Expr -> Expr -> Set.Set Name -> Expr -> Maybe Expr
abstractResult foldrName nil cons oldType resultType combine zero tails expression =
  case collectSpine expression of
    (ExVar name, [Left _, Right item, Right rest]) | name == cons -> do
      guard (independent item)
      ExApp (ExApp combine item) <$> go rest
    (ExVar name, [Left _]) | name == nil -> Just zero
    (ExVar name, []) | name `Set.member` tails -> Just expression
    (ExVar name, [Left inputType, Left outputType, Right (ExLam item (ExLam rest body)), Right base, Right input])
      | name == foldrName && outputType == oldType -> do
          guard (independent input)
          base' <- go base
          body' <- abstractResult foldrName nil cons oldType resultType combine zero (Set.insert (binderName rest) tails) body
          pure (foldCall name inputType resultType (ExLam item (ExLam (rest {binderType = resultType}) body')) base' input)
    _ -> case expression of
      ExCase value binder alternatives -> do
        guard (independent value)
        ExCase value binder <$> traverse (\alt -> (\rhs -> alt {altRhs = rhs}) <$> go (altRhs alt)) alternatives
      ExLet bind body -> do
        guard (independent (bindRhs bind))
        ExLet bind <$> go body
      ExAbsurd value _ | independent value -> Just (ExAbsurd value resultType)
      _ -> Nothing
  where
    go = abstractResult foldrName nil cons oldType resultType combine zero tails
    independent value = Set.null (tails `Set.intersection` exprValueNames value)

kindOfList :: Name -> Type
kindOfList name = TyCon (Name "Type" SortSynonym (nameOrigin name))

liftedRep :: Name -> Type
liftedRep name = TyCon (Name "LiftedRep" SortSynonym (nameOrigin name))
