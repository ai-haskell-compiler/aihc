-- | Merge static heap reservations between operations that can start collection.
module Aihc.Grin.Heap
  ( normalizeHeapReservations,
  )
where

import Aihc.Grin.Syntax

normalizeHeapReservations :: GrinProgram -> GrinProgram
normalizeHeapReservations program =
  program {grinFunctions = map normalizeFunction (grinFunctions program)}

normalizeFunction :: GrinFunction -> GrinFunction
normalizeFunction function =
  let (requiredWords, body) = normalizeExpr (grinFunctionBody function)
   in function {grinFunctionBody = addReservation requiredWords body}

normalizeExpr :: GrinExpr -> (Integer, GrinExpr)
normalizeExpr expression =
  case expression of
    GrinBind [] reservation@(GrinEnsureHeap _ _) body ->
      case staticReservationWords reservation of
        Just reservedWords ->
          let (bodyWords, body') = normalizeExpr body
           in (reservedWords + bodyWords, body')
        Nothing ->
          let (bodyWords, body') = normalizeExpr body
           in (0, GrinBind [] reservation (addReservation bodyWords body'))
    GrinBind vars valueExpression body ->
      let (valueWords, valueExpression') = normalizeExpr valueExpression
          (bodyWords, body') = normalizeExpr body
       in if isReservationBarrier valueExpression
            then
              ( valueWords,
                GrinBind vars valueExpression' (addReservation bodyWords body')
              )
            else
              ( valueWords + bodyWords,
                GrinBind vars valueExpression' body'
              )
    GrinCase scrutinee binder alternatives ->
      let normalized = map normalizeAlternative alternatives
          requiredWords = maximum (0 : map fst normalized)
       in ( requiredWords,
            GrinCase scrutinee binder (map snd normalized)
          )
    GrinStoreRec bindings body ->
      let (bodyWords, body') = normalizeExpr body
       in (bodyWords, GrinStoreRec bindings body')
    GrinStoreRecUnchecked bindings body ->
      let (bodyWords, body') = normalizeExpr body
       in (bodyWords, GrinStoreRecUnchecked bindings body')
    _ -> (0, expression)

normalizeAlternative :: GrinAlt -> (Integer, GrinAlt)
normalizeAlternative alternative =
  let (requiredWords, rhs) = normalizeExpr (grinAltRhs alternative)
   in (requiredWords, alternative {grinAltRhs = rhs})

addReservation :: Integer -> GrinExpr -> GrinExpr
addReservation requiredWords body
  | requiredWords <= 0 = body
  | otherwise =
      GrinBind
        []
        (GrinEnsureHeap (GrinLitValue (GrinLitInt WordRep requiredWords)) [])
        body

staticReservationWords :: GrinExpr -> Maybe Integer
staticReservationWords expression =
  case expression of
    GrinEnsureHeap (GrinLitValue (GrinLitInt WordRep requiredWords)) []
      | requiredWords >= 0 -> Just requiredWords
    _ -> Nothing

isReservationBarrier :: GrinExpr -> Bool
isReservationBarrier expression =
  case expression of
    GrinEnsureHeap {} -> True
    GrinEval {} -> True
    GrinCpsEval {} -> True
    GrinCall {} -> True
    GrinPrimitiveCall {} -> True
    GrinCpsPrimitiveCall {} -> True
    GrinApply {} -> True
    GrinCpsApply {} -> True
    GrinCpsRaise {} -> True
    GrinThrow {} -> True
    GrinCatch {} -> True
    GrinForeignCallExpr {} -> True
    _ -> False
