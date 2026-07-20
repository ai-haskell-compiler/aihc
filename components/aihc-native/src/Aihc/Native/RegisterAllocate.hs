{-# LANGUAGE OverloadedStrings #-}

-- | Architecture-neutral register allocation over CPS-GRIN functions.
--
-- GRIN variables are immutable, so allocation can operate on their lexical
-- live ranges directly. Backends provide the registers which are safe for
-- generated code to own. Values live across a C call are assigned heap spill
-- slots. Moving-GC roots are separately staged by the code generated for
-- 'GrinEnsureHeap', which writes their relocated values back to these homes.
module Aihc.Native.RegisterAllocate
  ( AllocatorConfig (..),
    Location (..),
    Allocation (..),
    allocateFunction,
    grinExprFreeVariables,
  )
where

import Aihc.Grin.Syntax
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set

data AllocatorConfig physical = AllocatorConfig
  { allocatorRegisters :: [physical],
    allocatorFixedLocations :: !(Map GrinVar (Location physical))
  }

data Location physical
  = InRegister !physical
  | InHeapSpill !Int
  deriving (Eq, Show)

data Allocation physical = Allocation
  { allocationLocations :: !(Map GrinVar (Location physical)),
    allocationSpillCount :: !Int
  }
  deriving (Eq, Show)

data Analysis = Analysis
  { analysisInterference :: !(Map GrinVar (Set GrinVar)),
    analysisCrossesCall :: !(Set GrinVar)
  }

instance Semigroup Analysis where
  left <> right =
    Analysis
      { analysisInterference = Map.unionWith (<>) (analysisInterference left) (analysisInterference right),
        analysisCrossesCall = analysisCrossesCall left <> analysisCrossesCall right
      }

instance Monoid Analysis where
  mempty = Analysis Map.empty Set.empty

allocateFunction :: (Ord physical) => AllocatorConfig physical -> GrinFunction -> Allocation physical
allocateFunction config function =
  Allocation
    { allocationLocations = locations,
      allocationSpillCount = spillCount
    }
  where
    (_, bodyAnalysis) = analyzeExpr (grinFunctionBody function)
    parameters = Set.fromList (grinFunctionParameters function)
    localVariables = parameters <> grinExprBoundVariables (grinFunctionBody function)
    -- Entry lowering establishes every parameter home before executing the
    -- body, including homes for parameters which the body does not use.
    -- Consequently incoming parameters must not alias one another.
    analysis = bodyAnalysis <> clique parameters
    graph = analysisInterference analysis
    variables =
      sortOn
        (\var -> (Down (Set.size (Map.findWithDefault Set.empty var graph)), var))
        (Set.toList localVariables)
    fixedLocations = allocatorFixedLocations config
    unallocatedVariables = filter (`Map.notMember` fixedLocations) variables
    (registerLocations, spilled) = foldl' assignRegister (fixedLocations, []) unallocatedVariables
    assignRegister (assigned, spills) var
      | var `Set.member` analysisCrossesCall analysis = (assigned, var : spills)
      | otherwise =
          case availableRegisters assigned var of
            register : _ -> (Map.insert var (InRegister register) assigned, spills)
            [] -> (assigned, var : spills)
    availableRegisters assigned var =
      [ register
      | register <- allocatorRegisters config,
        register `Set.notMember` occupiedRegisters assigned var
      ]
    occupiedRegisters assigned var =
      Set.fromList
        [ register
        | neighbour <- Set.toList (Map.findWithDefault Set.empty var graph),
          Just (InRegister register) <- [Map.lookup neighbour assigned]
        ]
    firstSpillCount = maximum (0 : [slot + 1 | InHeapSpill slot <- Map.elems fixedLocations])
    (locations, spillCount) = foldl' assignSpill (registerLocations, firstSpillCount) (reverse spilled)
    assignSpill (assigned, nextSlot) var =
      let occupiedSlots =
            Set.fromList
              [ occupiedSlot
              | neighbour <- Set.toList (Map.findWithDefault Set.empty var graph),
                Just (InHeapSpill occupiedSlot) <- [Map.lookup neighbour assigned]
              ]
          slot = firstAvailableSlot occupiedSlots
       in (Map.insert var (InHeapSpill slot) assigned, max nextSlot (slot + 1))

firstAvailableSlot :: Set Int -> Int
firstAvailableSlot occupied = go 0
  where
    go slot
      | slot `Set.member` occupied = go (slot + 1)
      | otherwise = slot

-- | Return the variables live before an expression and the constraints found
-- while walking it. CPS terminal expressions have no successor, while a bind
-- connects its value expression to the body which consumes the bound values.
analyzeExpr :: GrinExpr -> (Set GrinVar, Analysis)
analyzeExpr expression =
  case expression of
    GrinBind bound valueExpression body ->
      let (bodyLive, bodyAnalysis) = analyzeExpr body
          boundSet = Set.fromList bound
          survivors = bodyLive `Set.difference` boundSet
          expressionUses = grinExprFreeVariables valueExpression
          liveBefore = expressionUses <> survivors
          simultaneousResults = clique (boundSet <> survivors)
          callCrossing =
            if exprCallsC valueExpression
              then recordCallCrossing survivors
              else mempty
          valueCallOperands =
            if exprHasInternalCalls valueExpression
              then recordCallCrossing expressionUses
              else mempty
       in ( liveBefore,
            bodyAnalysis
              <> recordLive liveBefore
              <> recordLive bodyLive
              <> simultaneousResults
              <> callCrossing
              <> valueCallOperands
          )
    GrinStoreRec bindings body -> analyzeStoreRec bindings body
    GrinStoreRecUnchecked bindings body -> analyzeStoreRec bindings body
    GrinCase scrutinee binder alternatives ->
      let alternativesAnalysis = map (analyzeAlternative binder) alternatives
          outerLive = Set.unions (map fst alternativesAnalysis)
          liveBefore = valueUses scrutinee <> outerLive
       in ( liveBefore,
            mconcat (map snd alternativesAnalysis)
              <> recordLive liveBefore
          )
    _ ->
      let uses = grinExprFreeVariables expression
          crossing =
            if exprHasInternalCalls expression
              then recordCallCrossing uses
              else mempty
       in (uses, recordLive uses <> crossing)

analyzeAlternative :: GrinVar -> GrinAlt -> (Set GrinVar, Analysis)
analyzeAlternative caseBinder alternative =
  (outerLive, analysis <> resultInterference)
  where
    (rhsLive, analysis) = analyzeExpr (grinAltRhs alternative)
    definitions = Set.fromList (caseBinder : grinAltBinders alternative)
    outerLive = rhsLive `Set.difference` definitions
    resultInterference = clique ((rhsLive `Set.intersection` definitions) <> outerLive)

analyzeStoreRec :: [(GrinVar, GrinNode)] -> GrinExpr -> (Set GrinVar, Analysis)
analyzeStoreRec bindings body =
  ( liveBefore,
    bodyAnalysis
      <> recordLive liveBefore
      <> recordLive bodyLive
      <> recordCallCrossing (bound <> nodeUses <> survivors)
      <> clique (bound <> survivors)
  )
  where
    (bodyLive, bodyAnalysis) = analyzeExpr body
    bound = Set.fromList (map fst bindings)
    nodeUses = foldMap (foldMap valueUses . grinNodeFields . snd) bindings
    survivors = bodyLive `Set.difference` bound
    liveBefore = nodeUses <> survivors

recordLive :: Set GrinVar -> Analysis
recordLive = clique

recordCallCrossing :: Set GrinVar -> Analysis
recordCallCrossing variables =
  mempty {analysisCrossesCall = variables}

clique :: Set GrinVar -> Analysis
clique variables =
  Analysis
    { analysisInterference =
        Map.fromList
          [ (var, Set.delete var variables)
          | var <- Set.toList variables
          ],
      analysisCrossesCall = Set.empty
    }

grinExprFreeVariables :: GrinExpr -> Set GrinVar
grinExprFreeVariables expression =
  case expression of
    GrinConstant values -> foldMap valueUses values
    GrinBind bound valueExpression body ->
      grinExprFreeVariables valueExpression <> (grinExprFreeVariables body `Set.difference` Set.fromList bound)
    GrinStore node -> nodeUses node
    GrinEnsureHeap _ roots -> foldMap valueUses roots
    GrinStoreUnchecked node -> nodeUses node
    GrinStoreRec bindings body -> storeRecUses bindings body
    GrinStoreRecUnchecked bindings body -> storeRecUses bindings body
    GrinFetch _ pointer -> valueUses pointer
    GrinUpdate pointer value -> valueUses pointer <> valueUses value
    GrinEval _ value -> valueUses value
    GrinCpsEval _ value continuation updateContinuation -> foldMap valueUses [value, continuation, updateContinuation]
    GrinCall _ _ arguments -> foldMap valueUses arguments
    GrinPrimitiveCall _ _ arguments -> foldMap valueUses arguments
    GrinCpsPrimitiveCall _ _ arguments continuation -> foldMap valueUses arguments <> valueUses continuation
    GrinApply _ function arguments -> valueUses function <> foldMap valueUses arguments
    GrinCpsApply _ function arguments continuation -> valueUses function <> foldMap valueUses arguments <> valueUses continuation
    GrinContinue continuation values -> valueUses continuation <> foldMap valueUses values
    GrinUpdateBlackhole pointer value -> valueUses pointer <> valueUses value
    GrinHalt values -> foldMap valueUses values
    GrinCase scrutinee binder alternatives ->
      valueUses scrutinee
        <> foldMap
          (\alternative -> grinExprFreeVariables (grinAltRhs alternative) `Set.difference` Set.fromList (binder : grinAltBinders alternative))
          alternatives
    GrinThrow exception -> valueUses exception
    GrinCatch _ action handler state -> foldMap valueUses (action : handler : state)
    GrinForeignCallExpr _ arguments -> foldMap valueUses arguments
  where
    nodeUses = foldMap valueUses . grinNodeFields
    storeRecUses bindings body =
      let bound = Set.fromList (map fst bindings)
       in (foldMap (nodeUses . snd) bindings <> grinExprFreeVariables body) `Set.difference` bound

grinExprBoundVariables :: GrinExpr -> Set GrinVar
grinExprBoundVariables expression =
  case expression of
    GrinBind bound valueExpression body ->
      Set.fromList bound <> grinExprBoundVariables valueExpression <> grinExprBoundVariables body
    GrinStoreRec bindings body -> Set.fromList (map fst bindings) <> grinExprBoundVariables body
    GrinStoreRecUnchecked bindings body -> Set.fromList (map fst bindings) <> grinExprBoundVariables body
    GrinCase _ binder alternatives ->
      Set.insert binder $ foldMap (\alternative -> Set.fromList (grinAltBinders alternative) <> grinExprBoundVariables (grinAltRhs alternative)) alternatives
    _ -> Set.empty

valueUses :: GrinValue -> Set GrinVar
valueUses value =
  case value of
    GrinVarValue var -> Set.singleton var
    GrinLitValue {} -> Set.empty

-- | A call whose result is bound by an enclosing 'GrinBind'. Variables live
-- after it must survive the platform C ABI.
exprCallsC :: GrinExpr -> Bool
exprCallsC expression =
  case expression of
    GrinStore {} -> True
    GrinEnsureHeap {} -> True
    GrinStoreUnchecked {} -> True
    GrinStoreRec {} -> True
    GrinStoreRecUnchecked {} -> True
    GrinUpdate {} -> True
    GrinUpdateBlackhole {} -> True
    GrinForeignCallExpr {} -> True
    GrinPrimitiveCall _ name _ -> name /= "+#" && name /= "realWorld#"
    _ -> False

-- | Some expressions make several C calls while consuming their operands, so
-- those operands themselves need stable homes for the duration of lowering.
exprHasInternalCalls :: GrinExpr -> Bool
exprHasInternalCalls expression =
  case expression of
    GrinStore node -> not (null (grinNodeFields node))
    GrinStoreUnchecked node -> not (null (grinNodeFields node))
    GrinStoreRec {} -> True
    GrinStoreRecUnchecked {} -> True
    _ -> False
