{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Replace bounded uniform array fills with a bulk byte store.
module Aihc.Grin.ArrayLoops (optimizeArrayLoops) where

import Aihc.Grin.Analysis (freeExprVars, freeValueVars, maximumProgramVarUnique)
import Aihc.Grin.Syntax
import Control.Monad (guard)
import Control.Monad.Trans.State.Strict (State, evalState, state)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)

-- The fallback retains the original loop. Only zero and all-one fills use
-- the bulk store. Bounds checks prove that byte arithmetic cannot overflow.
optimizeArrayLoops :: GrinProgram -> GrinProgram
optimizeArrayLoops program = program {grinFunctions = rewritten, grinPrimitives = primitives}
  where
    rewritten = evalState (concat <$> mapM rewrite (grinFunctions program)) (maximumProgramVarUnique program + 1, functionNamesFrom (Map.keysSet functions))
    primitives
      | length rewritten == length (grinFunctions program) = grinPrimitives program
      | otherwise =
          grinPrimitives program
            <> [(GrinVar name (-2000000000 + arity) rep, arity) | (name, arity, rep) <- declarations, name `Set.notMember` declared]
    declared = Set.fromList (map (grinVarName . fst) (grinPrimitives program))
    declarations =
      [("setByteArray#", 5, TupleRep []), ("sizeofMutableByteArray#", 1, IntRep)]
        <> [(name, 2, IntRep) | name <- ["-#", "+#", "*#", "quotInt#", ">=#", "<=#", "<#", "==#"]]
    functions = Map.fromList [(grinFunctionName f, f) | f <- grinFunctions program]
    rewrite function = case recognize functions function of
      Nothing -> pure [function]
      Just (prefix, array, index, end, value, width, done) -> do
        fallbackName <- freshFunction (unFunctionName (grinFunctionName function) <> "$fill_fallback")
        let fallback = function {grinFunctionName = fallbackName}
            -- All back edges use the unchanged fallback, including helper calls.
            fallbackCall = GrinCall (grinFunctionResultRep function) fallbackName (map GrinVarValue (grinFunctionParameters function))
        filled <- operation IntRep "-#" [end, index] $ \difference ->
          operation IntRep "+#" [difference, int 1] $ \count ->
            operation IntRep "*#" [count, int width] $ \bytes ->
              operation IntRep "*#" [index, int width] $ \offset ->
                pure (GrinBind [] (GrinPrimitiveCall (TupleRep []) "setByteArray#" [array, offset, bytes, value]) done)
        bounded <- operation IntRep "sizeofMutableByteArray#" [array] $ \size ->
          operation IntRep "quotInt#" [size, int width] $ \length' ->
            condition ">=#" [index, int 0] fallbackCall
              =<< (condition "<=#" [index, end] fallbackCall =<< condition "<#" [end, length'] fallbackCall filled)
        ones <- condition "==#" [value, int (-1)] fallbackCall bounded
        selected <- condition "==#" [value, int 0] ones bounded
        let body = foldr (\(vars, expr) rest -> GrinBind vars expr rest) selected prefix
            -- Redirect helper back edges through a private copy. This keeps
            -- the fallback free of the bulk checks on each iteration.
            redirect = replaceCalls (grinFunctionName function) fallbackName
            helpers = Set.toList (Set.fromList (grinExprFunctionNames (grinFunctionBody function)))
            needed = [f | name <- helpers, Just f <- [Map.lookup name functions], isStep (grinFunctionName function) f]
        renamed <- mapM (\f -> (grinFunctionName f,) <$> freshFunction (unFunctionName (grinFunctionName f) <> "$fill_step")) needed
        let redirectHelpers expr = foldr (uncurry replaceCalls) expr renamed
            copies = [f {grinFunctionName = newName, grinFunctionBody = redirect (grinFunctionBody f)} | (f, (_, newName)) <- zip needed renamed]
        pure (function {grinFunctionBody = body} : fallback {grinFunctionBody = redirectHelpers (redirect (grinFunctionBody fallback))} : copies)

-- | Recognize one store per index and an increment of one on the back edge.
recognize :: Map.Map FunctionName GrinFunction -> GrinFunction -> Maybe ([([GrinVar], GrinExpr)], GrinValue, GrinValue, GrinValue, GrinValue, Integer, GrinExpr)
recognize functions function = do
  let (prefix, body) = fetchPrefix (grinFunctionBody function)
  GrinBind [test] (GrinPrimitiveCall IntRep "==#" [index, end]) (GrinCase (GrinVarValue selected) binder alternatives) <- pure body
  guard (test == selected)
  [again, last'] <- pure alternatives
  guard (grinAltCon again == GrinLitAlt (GrinLitInt IntRep 0) && grinAltCon last' == GrinDefaultAlt)
  (operationName, array, storedIndex, value, back) <- write (grinAltRhs again)
  (lastName, lastArray, lastIndex, lastValue, done) <- write (grinAltRhs last')
  guard (operationName == lastName && array == lastArray && index == storedIndex && index == lastIndex && value == lastValue)
  width <- Map.lookup operationName (Map.fromList [("writeIntArray#", 8), ("writeWordArray#", 8), ("writeInt64Array#", 8), ("writeWord64Array#", 8), ("writeInt32Array#", 4), ("writeWord32Array#", 4), ("writeInt16Array#", 2), ("writeWord16Array#", 2), ("writeInt8Array#", 1), ("writeWord8Array#", 1)])
  GrinVarValue indexVar <- pure index
  guard (indexVar `elem` grinFunctionParameters function)
  guard (grinValueRuntimeRep value == IntRep)
  guard (Set.notMember indexVar (foldMap freeValueVars [array, end, value] <> foldMap (freeExprVars . snd) prefix))
  guard (Set.null (Set.fromList [indexVar, test, binder] `Set.intersection` freeExprVars done))
  increment <- stepBody back
  GrinBind [next] (GrinPrimitiveCall IntRep "+#" [old, GrinLitValue (GrinLitInt IntRep 1)]) (GrinCall _ target args) <- pure increment
  guard (old == index && target == grinFunctionName function)
  let expected = [if p == indexVar then GrinVarValue next else GrinVarValue p | p <- grinFunctionParameters function]
  guard (args == expected)
  pure (prefix, array, index, end, value, width, done)
  where
    write (GrinBind [] (GrinPrimitiveCall (TupleRep []) name [array, index, value]) rest) = Just (name, array, index, value, rest)
    write _ = Nothing
    stepBody body@GrinBind {} = Just body
    stepBody (GrinCall _ name args) = do
      helper <- Map.lookup name functions
      guard (length args == length (grinFunctionParameters helper))
      pure (substitute (Map.fromList (zip (grinFunctionParameters helper) args)) (grinFunctionBody helper))
    stepBody _ = Nothing

fetchPrefix :: GrinExpr -> ([([GrinVar], GrinExpr)], GrinExpr)
fetchPrefix (GrinBind vars fetch@(GrinFetch tag _) rest)
  | immutable tag = let (prefix, body) = fetchPrefix rest in ((vars, fetch) : prefix, body)
fetchPrefix body = ([], body)

immutable :: GrinNodeTag -> Bool
immutable (GrinConstructor _ 0) = True
immutable GrinClosure {} = True
immutable _ = False

isStep :: FunctionName -> GrinFunction -> Bool
isStep target function = case grinFunctionBody function of
  GrinBind [_] (GrinPrimitiveCall IntRep "+#" _) (GrinCall _ name _) -> name == target
  _ -> False

replaceCalls :: FunctionName -> FunctionName -> GrinExpr -> GrinExpr
replaceCalls old new = walk
  where
    walk (GrinCall rep name args) | name == old = GrinCall rep new args
    walk (GrinBind vars value body) = GrinBind vars (walk value) (walk body)
    walk (GrinCase value binder alts) = GrinCase value binder [a {grinAltRhs = walk (grinAltRhs a)} | a <- alts]
    walk body = body

substitute :: Map.Map GrinVar GrinValue -> GrinExpr -> GrinExpr
substitute values = walk
  where
    value v@(GrinVarValue var) = Map.findWithDefault v var values
    value v = v
    walk (GrinBind vars expr body) = GrinBind vars (walk expr) (walk body)
    walk (GrinPrimitiveCall rep name args) = GrinPrimitiveCall rep name (map value args)
    walk (GrinCall rep name args) = GrinCall rep name (map value args)
    walk body = body

int :: Integer -> GrinValue
int n = GrinLitValue (GrinLitInt IntRep n)

type Rewrite = State (Int, FunctionNames)

fresh :: Rewrite GrinVar
fresh = state (\(n, names) -> (GrinVar "fill" n IntRep, (n + 1, names)))

freshFunction :: Text -> Rewrite FunctionName
freshFunction base = state (\(n, names) -> let (name, next) = claimFunctionName base names in (name, (n, next)))

operation :: GrinRep -> Text -> [GrinValue] -> (GrinValue -> Rewrite GrinExpr) -> Rewrite GrinExpr
operation rep name args next = do
  result <- fresh
  rest <- next (GrinVarValue result)
  pure (GrinBind [result] (GrinPrimitiveCall rep name args) rest)

condition :: Text -> [GrinValue] -> GrinExpr -> GrinExpr -> Rewrite GrinExpr
condition name args no yes = operation IntRep name args $ \result -> do
  binder <- fresh
  pure (GrinCase result binder [GrinAlt (GrinLitAlt (GrinLitInt IntRep 0)) [] no, GrinAlt GrinDefaultAlt [] yes])
