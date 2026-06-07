{-# LANGUAGE OverloadedStrings #-}

-- | Semantic annotation helpers used while constraint generation rebuilds
-- syntax.
--
-- These helpers deliberately take a 'TcSolveReport'. Callers can tie the
-- report back to the solver result, allowing annotation payloads to be zonked
-- and evidence-filled after solving without running a separate replay pass.
module Aihc.Tc.Generate.Annotate
  ( annotationForElaborationWithReport,
    attachExprElaboration,
    annotateNameElaboration,
    annotateBindingNameWithReport,
    annotateDeclWithTypeWithReport,
    annotateValueDeclBindingsWithReport,
    annotatePatternBindingWithReport,
    annotatePatternWithReport,
    listElementType,
    tupleElementTypes,
  )
where

import Aihc.Parser.Syntax
  ( Decl (..),
    Expr,
    Name (..),
    Pattern (..),
    UnqualifiedName (..),
    ValueDecl (..),
    mkAnnotation,
  )
import Aihc.Tc.Annotations (TcAnnotation (..), TcBindingAnnotation (..), annotateDecl, annotateExpr)
import Aihc.Tc.Evidence (EvTerm (..), EvVar (..))
import Aihc.Tc.Monad (OccurrenceElaboration (..), TcSolveReport (..))
import Aihc.Tc.Types (TcType (..), TyCon (..))
import Aihc.Tc.Zonk (zonkTypeWithReport)
import Data.Map.Strict qualified as Map

annotationForElaborationWithReport :: TcSolveReport -> OccurrenceElaboration -> TcAnnotation
annotationForElaborationWithReport report elaboration =
  TcAnnotation
    { tcAnnType = zonkTypeWithReport report (occurrenceElabType elaboration),
      tcAnnTypeArgs = map (zonkTypeWithReport report) (occurrenceElabTypeArgs elaboration),
      tcAnnEvidenceTerms = map (evidenceForEvVarWithReport report) (occurrenceElabEvidenceVars elaboration),
      tcAnnTermArgTypes = map (zonkTypeWithReport report) (occurrenceElabTermArgTypes elaboration)
    }

attachExprElaboration :: TcSolveReport -> OccurrenceElaboration -> Expr -> Expr
attachExprElaboration report elaboration expr =
  if occurrenceElaborationSolved report elaboration
    then annotateExpr (annotationForElaborationWithReport report elaboration) expr
    else expr

annotateNameElaboration :: TcSolveReport -> OccurrenceElaboration -> Name -> Name
annotateNameElaboration report elaboration name =
  if occurrenceElaborationSolved report elaboration
    then name {nameAnns = nameAnns name <> [mkAnnotation (annotationForElaborationWithReport report elaboration)]}
    else name

annotateBindingNameWithReport :: TcSolveReport -> TcType -> UnqualifiedName -> UnqualifiedName
annotateBindingNameWithReport report ty name =
  name {unqualifiedNameAnns = unqualifiedNameAnns name <> [mkAnnotation bindingAnn]}
  where
    bindingAnn = TcBindingAnnotation name (zonkTypeWithReport report ty)

annotateDeclWithTypeWithReport :: TcSolveReport -> TcType -> Decl -> Decl
annotateDeclWithTypeWithReport report ty decl =
  annotateDecl tcAnn (annotateValueDeclBindingsWithReport report ty decl)
  where
    tcAnn = TcAnnotation (zonkTypeWithReport report ty) [] [] []

annotateValueDeclBindingsWithReport :: TcSolveReport -> TcType -> Decl -> Decl
annotateValueDeclBindingsWithReport report ty decl =
  case decl of
    DeclValue valueDecl -> DeclValue (goValue valueDecl)
    _ -> decl
  where
    goValue valueDecl =
      case valueDecl of
        FunctionBind name matches ->
          FunctionBind (annotateBindingNameWithReport report ty name) matches
        PatternBind multiplicity pat rhs ->
          PatternBind multiplicity (annotatePatternBindingWithReport report ty pat) rhs

annotatePatternWithReport :: TcSolveReport -> TcType -> Pattern -> Pattern
annotatePatternWithReport report ty pat =
  annotatePatternBindingZonked report zonkedTy (PAnn (mkAnnotation tcAnn) pat)
  where
    zonkedTy = zonkTypeWithReport report ty
    tcAnn = TcAnnotation zonkedTy [] [] []

annotatePatternBindingWithReport :: TcSolveReport -> TcType -> Pattern -> Pattern
annotatePatternBindingWithReport report ty =
  annotatePatternBindingZonked report (zonkTypeWithReport report ty)

annotatePatternBindingZonked :: TcSolveReport -> TcType -> Pattern -> Pattern
annotatePatternBindingZonked report ty pat =
  case pat of
    PAnn ann inner -> PAnn ann (annotatePatternBindingZonked report ty inner)
    PVar name -> PVar (annotateBindingNameWithReport report ty name)
    PParen inner -> PParen (annotatePatternBindingZonked report ty inner)
    PAs name inner ->
      PAs
        (annotateBindingNameWithReport report ty name)
        (annotatePatternBindingZonked report ty inner)
    PStrict inner -> PStrict (annotatePatternBindingZonked report ty inner)
    PIrrefutable inner -> PIrrefutable (annotatePatternBindingZonked report ty inner)
    PList items ->
      case listElementType ty of
        Just elemTy -> PList (map (annotatePatternBindingZonked report elemTy) items)
        Nothing -> pat
    PTuple flavor items ->
      PTuple flavor (zipWith (annotatePatternBindingZonked report) (tupleElementTypes ty) items)
    PUnboxedSum alt arity inner ->
      PUnboxedSum alt arity (annotatePatternBindingZonked report ty inner)
    PInfix lhs op rhs
      | nameTextIsCons op ->
          case listElementType ty of
            Just elemTy ->
              PInfix
                (annotatePatternBindingZonked report elemTy lhs)
                op
                (annotatePatternBindingZonked report ty rhs)
            Nothing -> pat
      | otherwise -> pat
    PView expr inner -> PView expr (annotatePatternBindingZonked report ty inner)
    PTypeSig inner sigTy -> PTypeSig (annotatePatternBindingZonked report ty inner) sigTy
    _ -> pat

occurrenceElaborationSolved :: TcSolveReport -> OccurrenceElaboration -> Bool
occurrenceElaborationSolved report elaboration =
  all (evidenceAvailable report) (occurrenceElabEvidenceVars elaboration)

evidenceAvailable :: TcSolveReport -> EvVar -> Bool
evidenceAvailable report ev@(EvVar unique)
  | Map.member ev (tcSolveReportConstraintFailures report) = False
  | Map.member unique (tcSolveReportEvBinds report) = True
  | otherwise =
      error ("internal type annotation error: missing solved evidence for " <> show ev)

evidenceForEvVarWithReport :: TcSolveReport -> EvVar -> EvTerm
evidenceForEvVarWithReport report ev@(EvVar unique) =
  case Map.lookup unique (tcSolveReportEvBinds report) of
    Just evidence -> zonkEvidenceWithReport report evidence
    Nothing -> error ("internal type annotation error: missing solved evidence for " <> show ev)

zonkEvidenceWithReport :: TcSolveReport -> EvTerm -> EvTerm
zonkEvidenceWithReport report evidence =
  case evidence of
    EvVarTerm ev -> EvVarTerm ev
    EvGiven pred' -> EvGiven pred'
    EvDict name typeArgs evidenceArgs ->
      EvDict name (map (zonkTypeWithReport report) typeArgs) (map (zonkEvidenceWithReport report) evidenceArgs)
    EvCoercion coercion -> EvCoercion coercion
    EvSuperClass inner index -> EvSuperClass (zonkEvidenceWithReport report inner) index
    EvCast inner coercion -> EvCast (zonkEvidenceWithReport report inner) coercion

listElementType :: TcType -> Maybe TcType
listElementType ty =
  case ty of
    TcTyCon (TyCon "[]" 1) [elemTy] -> Just elemTy
    _ -> Nothing

tupleElementTypes :: TcType -> [TcType]
tupleElementTypes ty =
  case ty of
    TcTyCon (TyCon _ arity) elemTys
      | arity == length elemTys -> elemTys
    _ -> []

nameTextIsCons :: Name -> Bool
nameTextIsCons name = nameText name == ":"
