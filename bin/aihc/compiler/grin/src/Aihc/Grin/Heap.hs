-- | Merge static heap reservations between operations that can start collection.
module Aihc.Grin.Heap
  ( normalizeHeapReservations,
  )
where

import Aihc.Grin.Primitive (primitiveAllocates)
import Aihc.Grin.Syntax

normalizeHeapReservations :: GrinProgram -> GrinProgram
normalizeHeapReservations program =
  program {grinFunctions = map normalizeFunction (grinFunctions program)}

normalizeFunction :: GrinFunction -> GrinFunction
normalizeFunction function =
  let (requiredWords, body) = normalizeExpr (grinFunctionBody function)
   in function {grinFunctionBody = placeReservation requiredWords body}

-- | The merged reservation of an expression up to its next barrier.
--
-- A reservation of zero words is still a safepoint: a continuation frame
-- takes no heap words, but the runtime charges a new stack chunk to the
-- nursery, so its push must be able to collect. See
-- 'Aihc.Grin.Gc.insertHeapReservations'. A reservation above branches puts
-- a compare on each branch. Thus a reservation of zero words does not go
-- above a branch that needs no reservation, such as the ready branch of a
-- WHNF test. It stays in the branches that need it, unless a reservation
-- of some words above the branches covers them.
data Reservation
  = -- | No reservation.
    NoReservation
  | -- | Some branches need a reservation of zero words.
    BranchSafepoints
  | -- | One reservation of the given words above the expression.
    Reserve !Integer
  deriving (Eq)

-- | The reservation that covers two parts that run one after the other.
sequential :: Reservation -> Reservation -> Reservation
sequential first second =
  case (first, second) of
    (Reserve firstWords, Reserve secondWords) -> Reserve (firstWords + secondWords)
    (Reserve _, _) -> first
    (_, Reserve _) -> second
    (BranchSafepoints, _) -> BranchSafepoints
    (_, BranchSafepoints) -> BranchSafepoints
    _ -> NoReservation

-- | The reservation that covers whichever of the branches runs.
alternatives :: [Reservation] -> Reservation
alternatives reservations
  | null reserved = if BranchSafepoints `elem` reservations then BranchSafepoints else NoReservation
  | maximum reserved > 0 || all isReserve reservations = Reserve (maximum reserved)
  | otherwise = BranchSafepoints
  where
    reserved = [words' | Reserve words' <- reservations]

isReserve :: Reservation -> Bool
isReserve reservation =
  case reservation of
    Reserve _ -> True
    _ -> False

-- | An expression whose reservations are not placed yet. The argument says
-- whether a reservation above the expression, with no barrier between,
-- already covers it.
type Placement = Bool -> GrinExpr

normalizeExpr :: GrinExpr -> (Reservation, Placement)
normalizeExpr expression =
  case expression of
    GrinBind [] reservation@(GrinEnsureHeap _ _) body ->
      case staticReservationWords reservation of
        Just reservedWords ->
          let (bodyWords, body') = normalizeExpr body
           in (sequential (Reserve reservedWords) bodyWords, body')
        Nothing ->
          -- A dynamic reservation is a compare as well, so it covers the
          -- safepoints of the body.
          let (bodyWords, body') = normalizeExpr body
           in (NoReservation, const (GrinBind [] reservation (placeReservation bodyWords (const (body' True)))))
    GrinBind vars valueExpression body ->
      let (valueWords, valueExpression') = normalizeExpr valueExpression
          (bodyWords, body') = normalizeExpr body
       in if isReservationBarrier valueExpression
            then
              ( valueWords,
                \covered -> GrinBind vars (valueExpression' covered) (placeReservation bodyWords body')
              )
            else
              ( sequential valueWords bodyWords,
                \covered -> GrinBind vars (valueExpression' covered) (body' covered)
              )
    -- The WHNF test neither allocates nor collects, so one reservation above
    -- it can cover whichever branch runs.
    GrinIfWhnf value ready slow ->
      let (readyWords, ready') = normalizeExpr ready
          (slowWords, slow') = normalizeExpr slow
          requiredWords = alternatives [readyWords, slowWords]
       in ( requiredWords,
            \covered -> GrinIfWhnf value (placeBranch requiredWords covered readyWords ready') (placeBranch requiredWords covered slowWords slow')
          )
    GrinCase scrutinee binder alts ->
      let normalized = map normalizeAlternative alts
          requiredWords = alternatives (map fst normalized)
       in ( requiredWords,
            \covered ->
              GrinCase
                scrutinee
                binder
                [ alt {grinAltRhs = placeBranch requiredWords covered branchWords rhs}
                | (branchWords, (alt, rhs)) <- normalized
                ]
          )
    GrinStoreRec bindings body ->
      let (bodyWords, body') = normalizeExpr body
       in (bodyWords, GrinStoreRec bindings . body')
    GrinStoreRecUnchecked bindings body ->
      let (bodyWords, body') = normalizeExpr body
       in (bodyWords, GrinStoreRecUnchecked bindings . body')
    _ -> (NoReservation, const expression)

normalizeAlternative :: GrinAlt -> (Reservation, (GrinAlt, Placement))
normalizeAlternative alt =
  let (requiredWords, rhs) = normalizeExpr (grinAltRhs alt)
   in (requiredWords, (alt, rhs))

-- | Place a branch under branches with the given merged reservation. A
-- merged reservation of some words goes above the branches and covers
-- them. Otherwise a branch that needs a reservation of zero words gets it,
-- unless a reservation above covers it.
placeBranch :: Reservation -> Bool -> Reservation -> Placement -> GrinExpr
placeBranch merged covered branchWords branch
  | covered || isReserve merged = branch True
  | otherwise = placeReservation branchWords branch

-- | Put a reservation above an expression.
placeReservation :: Reservation -> Placement -> GrinExpr
placeReservation requiredWords body =
  case requiredWords of
    Reserve words' ->
      GrinBind
        []
        (GrinEnsureHeap (GrinLitValue (GrinLitInt WordRep words')) [])
        (body True)
    _ -> body False

staticReservationWords :: GrinExpr -> Maybe Integer
staticReservationWords expression =
  case expression of
    GrinEnsureHeap (GrinLitValue (GrinLitInt WordRep requiredWords)) []
      | requiredWords >= 0 -> Just requiredWords
    _ -> Nothing

-- | Whether an operation ends the reservation that reaches it. A primitive
-- that cannot allocate does not: the stores on either side of it share one
-- reservation.
isReservationBarrier :: GrinExpr -> Bool
isReservationBarrier expression =
  case expression of
    GrinEnsureHeap {} -> True
    GrinEval {} -> True
    GrinCpsEval {} -> True
    GrinCall {} -> True
    GrinPrimitiveCall _ name _ -> primitiveAllocates name
    GrinCpsPrimitiveCall {} -> True
    GrinApply {} -> True
    GrinForward -> False
    GrinCpsApply {} -> True
    GrinCpsRaise {} -> True
    GrinThrow {} -> True
    GrinCatch {} -> True
    GrinForeignCallExpr {} -> True
    _ -> False
