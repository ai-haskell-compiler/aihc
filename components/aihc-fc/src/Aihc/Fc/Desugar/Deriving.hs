-- | Strategy dispatch and dependency grouping for derived dictionaries.
module Aihc.Fc.Desugar.Deriving
  ( dsDerivingPlans,
    moduleDerivingPlans,
  )
where

import Aihc.Fc.Desugar.Deriving.AnyClass (dsAnyClassDictionaryPlan)
import Aihc.Fc.Desugar.Expr (DsM)
import Aihc.Fc.Syntax (FcBind (..), FcTopBind (..))
import Aihc.Parser.Syntax (Decl (..), fromAnnotation)
import Aihc.Tc.Annotations (TcClassMethodAnnotation (..), TcDerivingAnnotation (..), TcDerivingPlan (..), TcDerivingStrategy (..))
import Aihc.Tc.Evidence (EvTerm (..))
import Data.Graph (SCC (..), stronglyConnComp)
import Data.Text (Text)

dsDerivingPlans :: [TcDerivingPlan] -> DsM [FcTopBind]
dsDerivingPlans plans =
  mapM dsDerivingScc (stronglyConnComp planNodes)
  where
    anyClassPlans = filter ((== TcDerivingAnyclass) . tcDerivingStrategy) plans
    planNodes =
      [ (plan, tcDerivingDictName plan, derivingPlanDependencies plan)
      | plan <- anyClassPlans
      ]

-- Evidence may make sibling dictionaries depend on one another regardless of
-- their source order. Preserve acyclic dependency order and use recursive FC
-- groups only for genuine cycles (including a default worker's self dictionary).
dsDerivingScc :: SCC TcDerivingPlan -> DsM FcTopBind
dsDerivingScc scc =
  case scc of
    AcyclicSCC plan -> do
      (dictVar, body) <- dsAnyClassDictionaryPlan plan
      pure (FcTopBind (FcNonRec dictVar body))
    CyclicSCC plans -> do
      bindings <- mapM dsAnyClassDictionaryPlan plans
      pure (FcTopBind (FcRec bindings))

derivingPlanDependencies :: TcDerivingPlan -> [Text]
derivingPlanDependencies plan =
  selfDependency
    <> concatMap (evidenceDependencies . snd) (tcDerivingSuperClasses plan)
    <> concatMap (concatMap evidenceDependencies . snd) (tcDerivingDefaultMethodEvidence plan)
  where
    selfDependency =
      [ tcDerivingDictName plan
      | any
          ((`elem` tcDerivingDefaultMethods plan) . tcClassMethodName)
          (tcDerivingClassMethods plan)
      ]

evidenceDependencies :: EvTerm -> [Text]
evidenceDependencies evidence =
  case evidence of
    EvGiven {} -> []
    EvDict dictName _ contextEvidence -> dictName : concatMap evidenceDependencies contextEvidence
    EvCoercion {} -> []
    EvSuperClass source _ _ _ -> evidenceDependencies source
    EvCast source _ -> evidenceDependencies source
    EvTypeable _ arguments -> concatMap evidenceDependencies arguments
    EvVarTerm {} -> []

moduleDerivingPlans :: [Decl] -> [TcDerivingPlan]
moduleDerivingPlans = concatMap declDerivingPlans

declDerivingPlans :: Decl -> [TcDerivingPlan]
declDerivingPlans decl =
  case decl of
    DeclAnn ann inner ->
      maybe [] tcDerivingPlans (fromAnnotation ann) <> declDerivingPlans inner
    _ -> []
