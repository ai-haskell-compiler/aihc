{- ORACLE_TEST pass -}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Arrows #-}
module ImplicitLayoutStress where

-- =============================================================================
-- Case expressions in various contexts
-- =============================================================================

-- Case in boxed tuple
caseInBoxedTuple x = (case x of y -> y, 1)

-- Case in unboxed tuple
caseInUnboxedTuple x = (# case x of y -> y, 1 #)

-- Case in unboxed sum
caseInUnboxedSum x = (# case x of y -> y | #)

-- Case in list
caseInList x = [case x of y -> y]

-- Case as function argument
caseAsArg x = id (case x of y -> y)

-- Case in infix expression
caseInInfix x = case x of y -> y + 1

-- Nested case expressions
nestedCase x y = case x of
  0 -> case y of
    0 -> "both zero"
    _ -> "x zero"
  _ -> case y of
    0 -> "y zero"
    _ -> "neither zero"

-- Case with multiple alternatives
caseMultiAlt x = case x of
  0 -> "zero"
  1 -> "one"
  _ -> "other"

-- Case with guards
caseWithGuards x = case x of
  n | n < 0 -> "negative"
    | n > 0 -> "positive"
    | otherwise -> "zero"

-- Case with where clause
caseWithWhere x = case x of
  n -> result
  where
    result = n + 1

-- =============================================================================
-- Do expressions in various contexts
-- =============================================================================

-- Do in boxed tuple
doInBoxedTuple = (do x <- pure 1; pure x, 2)

-- Do in unboxed tuple
doInUnboxedTuple = (# do x <- pure 1; pure x, 2 #)

-- Do in list
doInList = [do x <- pure 1; pure x]

-- Do as function argument
doAsArg = id (do x <- pure 1; pure x)

-- Nested do expressions
nestedDo = do
  x <- do
    y <- pure 1
    pure (y + 1)
  pure x

-- Do with let
doWithLet = do
  let x = 1
  pure x

-- Do with multiple statements
doMultiStmt = do
  x <- pure 1
  y <- pure 2
  z <- pure 3
  pure (x + y + z)

-- =============================================================================
-- Let expressions in various contexts
-- =============================================================================

-- Let in boxed tuple
letInBoxedTuple = (let x = 1 in x, 2)

-- Let in unboxed tuple
letInUnboxedTuple = (# let x = 1 in x, 2 #)

-- Let in list
letInList = [let x = 1 in x]

-- Let as function argument
letAsArg = id (let x = 1 in x)

-- Nested let expressions
nestedLet = let x = let y = 1 in y + 1 in x + 1

-- Let with multiple bindings
letMultiBindings = let
  x = 1
  y = 2
  z = 3
  in x + y + z

-- Let with where in binding
letWithWhere = let
  f x = y where y = x + 1
  in f 1

-- =============================================================================
-- If expressions in various contexts
-- =============================================================================

-- If in boxed tuple
ifInBoxedTuple x = (if x then 1 else 2, 3)

-- If in unboxed tuple
ifInUnboxedTuple x = (# if x then 1 else 2, 3 #)

-- If in list
ifInList x = [if x then 1 else 2]

-- Nested if expressions
nestedIf x y = if x
  then if y then 1 else 2
  else if y then 3 else 4

-- If with do in branches
ifWithDo x = if x
  then do
    y <- pure 1
    pure y
  else do
    z <- pure 2
    pure z

-- If with case in branches
ifWithCase x y = if x
  then case y of
    0 -> "zero"
    _ -> "nonzero"
  else "x is false"

-- =============================================================================
-- Lambda expressions in various contexts
-- =============================================================================

-- Lambda in boxed tuple
lambdaInBoxedTuple = (\x -> x + 1, 2)

-- Lambda in unboxed tuple
lambdaInUnboxedTuple = (# \x -> x + 1, 2 #)

-- Lambda in list
lambdaInList = [\x -> x + 1]

-- Lambda with case body
lambdaWithCase = \x -> case x of
  0 -> "zero"
  _ -> "nonzero"

-- Lambda with do body
lambdaWithDo = \x -> do
  y <- pure x
  pure (y + 1)

-- Lambda with let body
lambdaWithLet = \x -> let y = x + 1 in y

-- =============================================================================
-- Lambda-case in various contexts
-- =============================================================================

-- Lambda-case in boxed tuple
lambdaCaseInBoxedTuple = (\case 0 -> "zero"; _ -> "other", 1)

-- Lambda-case in unboxed tuple
lambdaCaseInUnboxedTuple = (# \case 0 -> "zero"; _ -> "other", 1 #)

-- Lambda-case in list
lambdaCaseInList = [\case 0 -> "zero"; _ -> "other"]

-- Lambda-case with guards
lambdaCaseWithGuards = \case
  n | n < 0 -> "negative"
    | n > 0 -> "positive"
    | otherwise -> "zero"

-- Lambda-case with where
lambdaCaseWithWhere = \case
  n -> result
  where
    result = "processed"

-- =============================================================================
-- Multi-way if in various contexts
-- =============================================================================

-- Multi-way if basic
multiWayIfBasic x = if
  | x < 0 -> "negative"
  | x > 0 -> "positive"
  | otherwise -> "zero"

-- Multi-way if in boxed tuple
multiWayIfInBoxedTuple x = (if | x -> 1 | otherwise -> 2, 3)

-- Multi-way if in unboxed tuple
multiWayIfInUnboxedTuple x = (# if | x -> 1 | otherwise -> 2, 3 #)

-- Multi-way if nested
multiWayIfNested x y = if
  | x < 0 -> if
      | y < 0 -> "both negative"
      | otherwise -> "x negative"
  | otherwise -> if
      | y < 0 -> "y negative"
      | otherwise -> "both non-negative"

-- =============================================================================
-- Complex nesting and combinations
-- =============================================================================

-- Case inside do inside let
complexNesting1 x = let
  f y = do
    z <- pure y
    case z of
      0 -> pure "zero"
      _ -> pure "nonzero"
  in f x

-- Do inside case inside lambda
complexNesting2 = \x -> case x of
  0 -> do
    y <- pure "zero"
    pure y
  _ -> do
    y <- pure "nonzero"
    pure y

-- Let inside lambda inside case
complexNesting3 x = case x of
  0 -> \y -> let z = y + 1 in z
  _ -> \y -> let z = y - 1 in z

-- Multiple layout constructs at same level
multipleLayoutSameLevel = do
  x <- case True of
    True -> pure 1
    False -> pure 2
  y <- let z = 3 in pure z
  pure (x + y)

-- Deeply nested case
deeplyNestedCase a b c d = case a of
  True -> case b of
    True -> case c of
      True -> case d of
        True -> "all true"
        False -> "d false"
      False -> "c false"
    False -> "b false"
  False -> "a false"

-- =============================================================================
-- Layout with operators and sections
-- =============================================================================

-- Case in operator section
caseInSection x = (+ case x of y -> y)

-- Do in operator application
doInOperator = 1 + (do x <- pure 2; pure x)

-- Let in infix chain
letInInfixChain = 1 + let x = 2 in x + 3

-- =============================================================================
-- Layout in record contexts
-- =============================================================================

data MyRecord = MyRecord { myField :: Int }

-- Case in record construction
caseInRecord x = MyRecord { myField = case x of y -> y }

-- Do in record update
doInRecordUpdate r = r { myField = head (do x <- [1]; pure x) }

-- =============================================================================
-- Layout in type signature contexts
-- =============================================================================

-- Expression with type signature containing layout
withTypeSig :: Int -> String
withTypeSig x = case x of
  0 -> "zero"
  _ -> "nonzero"

-- Let with type signature on binding
letWithTypeSig = let
  f :: Int -> Int
  f x = x + 1
  in f 1

-- =============================================================================
-- Edge cases with semicolons and layout
-- =============================================================================

-- Explicit semicolons in implicit layout
explicitSemicolons x = case x of
  0 -> "zero"; 1 -> "one"
  _ -> "other"

-- Mixed explicit and implicit
mixedLayout x = case x of { 0 -> "zero"; 1 -> "one" }

-- Empty case alternatives (needs EmptyCase extension, skip)
-- emptyCase x = case x of {}

-- =============================================================================
-- Layout with comments
-- =============================================================================

-- Case with comments between alternatives
caseWithComments x = case x of
  -- First alternative
  0 -> "zero"
  -- Second alternative
  1 -> "one"
  -- Default
  _ -> "other"

-- Do with comments
doWithComments = do
  -- First statement
  x <- pure 1
  -- Second statement
  y <- pure 2
  -- Result
  pure (x + y)
