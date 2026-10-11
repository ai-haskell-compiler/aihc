{-# LANGUAGE OverloadedStrings #-}

-- | What the simplifier knows about primitive values in one scope: the
-- results of the comparisons that enclose it.
--
-- An alternative of a case on a comparison knows the result of that
-- comparison, and an alternative of a case on a variable with literal
-- alternatives knows the value of the variable or the values it cannot
-- have. These facts decide a later comparison of the same operands, and
-- they remove the alternatives of a later case on the variable that it
-- cannot select. For example, in the default alternative of
-- @case x <# 0# of { 1# -> a; _ -> b }@, the variable is not negative, so
-- @x >=# 0#@ is @1#@ there.
--
-- A fact about a variable and a literal is a range of the values of the
-- variable. A fact about two variables is the set of orders that the two
-- can still have. The facts only shrink inside a scope, and a binder that
-- binds a name again removes every fact about that name.
--
-- Floating-point comparisons give no facts, because a NaN is not ordered.
module Aihc.Fc.Facts
  ( Facts,
    emptyFacts,
    learnAlternative,
    learnBinding,
    decideComparison,
    possibleLiteral,
    knownValue,
    finiteValues,
    forgetNames,
  )
where

import Aihc.Fc.Name (Name, nameText)
import Aihc.Fc.Syntax
import Control.Applicative ((<|>))
import Data.Char qualified as Char
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

-- | The facts of one scope.
data Facts = Facts
  { -- | The values that each variable can have.
    factRanges :: !(Map Name Range),
    -- | The orders that two variables can have, keyed by the pair in
    -- 'Ord' order of the names. The order is that of the first name
    -- against the second.
    factOrders :: !(Map (Name, Name) (Set Ordering)),
    -- | The variables that a strict let binds to a comparison. A case on
    -- such a variable is a case on the comparison.
    factDefinitions :: !(Map Name (Comparison, Operand, Operand))
  }

-- | A set of integers: the integers between the bounds, without the
-- excluded values. An absent bound is no bound.
data Range = Range
  { rangeLow :: !(Maybe Integer),
    rangeHigh :: !(Maybe Integer),
    rangeExcluded :: !(Set Integer)
  }

emptyFacts :: Facts
emptyFacts = Facts Map.empty Map.empty Map.empty

-- | An operand of a comparison: a variable or a literal value.
data Operand
  = OperandVar !Name
  | OperandValue !Integer

operand :: Expr -> Maybe Operand
operand expr =
  case expr of
    ExVar name -> Just (OperandVar name)
    ExLit literal _ -> OperandValue <$> literalValue literal
    _ -> Nothing

literalValue :: Literal -> Maybe Integer
literalValue literal =
  case literal of
    LitInt _ value -> Just value
    LitChar _ value -> Just (toInteger (Char.ord value))
    LitAddr {} -> Nothing

-- | What a comparison primitive gives for each order of its operands.
data Comparison
  = -- | One for the orders in the set, and zero for the others.
    Test !(Set Ordering)
  | -- | Minus one, zero or one for less, equal or greater.
    ThreeWay

-- | The comparison that a primitive makes, by its name. Only the @Int#@,
-- word and character comparisons are here. A literal of a sized signed
-- integer can have more than one spelling of its value, so those
-- comparisons give no facts.
comparison :: Text -> Maybe Comparison
comparison name =
  case name of
    "<#" -> test [LT]
    "<=#" -> test [LT, EQ]
    "==#" -> test [EQ]
    "/=#" -> test [LT, GT]
    ">#" -> test [GT]
    ">=#" -> test [EQ, GT]
    "compareInt#" -> Just ThreeWay
    _
      | T.length name > 2,
        T.takeEnd 1 name == "#",
        T.drop 2 name `elem` orderedTypes ->
          case T.take 2 name of
            "lt" -> test [LT]
            "le" -> test [LT, EQ]
            "eq" -> test [EQ]
            "ne" -> test [LT, GT]
            "gt" -> test [GT]
            "ge" -> test [EQ, GT]
            _ -> Nothing
      | otherwise -> Nothing
  where
    test = Just . Test . Set.fromList
    orderedTypes =
      ["Word#", "Char#", "Word8#", "Word16#", "Word32#", "Word64#"]

-- | The result of a comparison for one order of its operands.
result :: Comparison -> Ordering -> Integer
result kind order =
  case kind of
    Test orders -> if Set.member order orders then 1 else 0
    ThreeWay -> case order of
      LT -> -1
      EQ -> 0
      GT -> 1

-- | A primitive comparison call with operands that are variables or
-- literals.
comparisonCall :: Expr -> Maybe (Comparison, Operand, Operand)
comparisonCall expr =
  case expr of
    ExForeignCall call [] [left, right]
      | foreignCallConvention call == Prim -> do
          kind <- comparison (nameText (foreignCallName call))
          (,,) kind <$> operand left <*> operand right
    _ -> Nothing

-- | The facts inside an alternative of a case. The other alternatives of
-- the case tell which values a default alternative does not get. The
-- case binder holds the value of the scrutinee, so a fact about a
-- variable scrutinee is also a fact about the binder.
learnAlternative :: Expr -> Maybe Name -> [AltCon] -> AltCon -> Facts -> Facts
learnAlternative scrutinee binder alternatives alternative facts
  | Just (kind, left, right) <- comparisonCall scrutinee = byComparison kind left right facts
  | ExVar name <- scrutinee,
    any isLiteral alternatives =
      let defined = maybe id (\(kind, left, right) -> byComparison kind left right) (Map.lookup name (factDefinitions facts))
       in defined (foldr restrict facts (name : maybe [] pure binder))
  | otherwise = facts
  where
    byComparison kind left right = constrain left right (Set.filter (selected . result kind) allOrders)
    others = mapMaybe altValue [con | con <- alternatives, con /= AltDefault]
    selected value =
      case alternative of
        AltLit literal -> Just value == literalValue literal
        AltDefault -> value `notElem` others
        AltData _ -> False
    -- A case on a variable restricts its range directly.
    restrict variable current =
      case alternative of
        AltLit literal
          | Just value <- literalValue literal -> narrow variable (\range -> range {rangeLow = maxBound' (rangeLow range) value, rangeHigh = minBound' (rangeHigh range) value}) current
        AltDefault -> narrow variable (\range -> range {rangeExcluded = rangeExcluded range <> Set.fromList others}) current
        _ -> current
    altValue con =
      case con of
        AltLit literal -> literalValue literal
        _ -> Nothing
    isLiteral con =
      case con of
        AltLit _ -> True
        _ -> False

allOrders :: Set Ordering
allOrders = Set.fromList [LT, EQ, GT]

-- | The facts after a strict let of a variable: a variable bound to a
-- comparison stands for that comparison.
learnBinding :: Name -> Expr -> Facts -> Facts
learnBinding name rhs facts =
  case comparisonCall rhs of
    Just definition -> facts {factDefinitions = Map.insert name definition (factDefinitions facts)}
    Nothing -> facts

-- | Record that the order of two operands is one of the given orders. An
-- empty set records nothing: no value can enter such a scope.
constrain :: Operand -> Operand -> Set Ordering -> Facts -> Facts
constrain left right orders facts
  | Set.null orders || orders == allOrders = facts
  | otherwise =
      case (left, right) of
        (OperandVar name, OperandValue value) -> narrow name (bound value orders) facts
        (OperandValue value, OperandVar name) -> narrow name (bound value (Set.map flipOrder orders)) facts
        (OperandVar first, OperandVar second)
          | first == second -> facts
          | first < second -> order first second orders
          | otherwise -> order second first (Set.map flipOrder orders)
        (OperandValue _, OperandValue _) -> facts
  where
    order first second given =
      facts {factOrders = Map.insertWith Set.intersection (first, second) given (factOrders facts)}
    -- The range of a variable whose order against a value is one of the
    -- given orders.
    bound value given range =
      range
        { rangeLow = if Set.member LT given then rangeLow range else maxBound' (rangeLow range) value,
          rangeHigh = if Set.member GT given then rangeHigh range else minBound' (rangeHigh range) value,
          rangeExcluded = if Set.member EQ given then rangeExcluded range else Set.insert value (rangeExcluded range)
        }

-- | Change the range of a variable, and move each bound past the excluded
-- values next to it.
narrow :: Name -> (Range -> Range) -> Facts -> Facts
narrow name change facts =
  facts {factRanges = Map.insert name (tighten (change current)) (factRanges facts)}
  where
    current = Map.findWithDefault (Range Nothing Nothing Set.empty) name (factRanges facts)
    tighten range =
      let low = skip 1 (rangeExcluded range) <$> rangeLow range
          high = skip (-1) (rangeExcluded range) <$> rangeHigh range
       in range {rangeLow = low, rangeHigh = high}
    -- At most a few steps: a bound moves past a short run of excluded
    -- values only.
    skip step excluded = go (64 :: Int)
      where
        go fuel candidate
          | fuel > 0, Set.member candidate excluded = go (fuel - 1) (candidate + step)
          | otherwise = candidate

maxBound' :: Maybe Integer -> Integer -> Maybe Integer
maxBound' current value = Just (maybe value (max value) current)

minBound' :: Maybe Integer -> Integer -> Maybe Integer
minBound' current value = Just (maybe value (min value) current)

flipOrder :: Ordering -> Ordering
flipOrder order =
  case order of
    LT -> GT
    EQ -> EQ
    GT -> LT

-- | The orders that two operands can have.
possibleOrders :: Facts -> Operand -> Operand -> Set Ordering
possibleOrders facts left right =
  case (left, right) of
    (OperandVar name, OperandValue value) -> againstValue name value
    (OperandValue value, OperandVar name) -> Set.map flipOrder (againstValue name value)
    (OperandVar first, OperandVar second)
      | first == second -> Set.singleton EQ
      | first < second -> Map.findWithDefault allOrders (first, second) (factOrders facts)
      | otherwise -> Set.map flipOrder (Map.findWithDefault allOrders (second, first) (factOrders facts))
    (OperandValue first, OperandValue second) -> Set.singleton (compare first second)
  where
    againstValue name value =
      case rangeOf facts name of
        Nothing -> allOrders
        Just range ->
          Set.fromList
            ( [LT | maybe True (< value) (rangeLow range)]
                <> [EQ | inRange range value]
                <> [GT | maybe True (> value) (rangeHigh range)]
            )

-- | The range of a variable: its own range, and the range of each
-- variable that is equal to it.
rangeOf :: Facts -> Name -> Maybe Range
rangeOf facts name =
  case mapMaybe (`Map.lookup` factRanges facts) (name : equals) of
    [] -> Nothing
    ranges -> Just (foldr1 intersect ranges)
  where
    equals =
      [ other
      | ((first, second), orders) <- Map.toList (factOrders facts),
        orders == Set.singleton EQ,
        other <- if first == name then [second] else [first | second == name]
      ]
    intersect left right =
      Range
        { rangeLow = maybe (rangeLow right) (maxBound' (rangeLow right)) (rangeLow left),
          rangeHigh = maybe (rangeHigh right) (minBound' (rangeHigh right)) (rangeHigh left),
          rangeExcluded = rangeExcluded left <> rangeExcluded right
        }

inRange :: Range -> Integer -> Bool
inRange range value =
  maybe True (<= value) (rangeLow range)
    && maybe True (>= value) (rangeHigh range)
    && Set.notMember value (rangeExcluded range)

-- | The result of a comparison call when the facts decide it. A scope
-- whose facts contradict each other is never entered, and it decides
-- nothing.
decideComparison :: Facts -> Expr -> Maybe Integer
decideComparison facts expr = do
  (kind, left, right) <- comparisonCall expr
  decide facts kind left right

decide :: Facts -> Comparison -> Operand -> Operand -> Maybe Integer
decide facts kind left right =
  case Set.toList (Set.map (result kind) (possibleOrders facts left right)) of
    [value] -> Just value
    _ -> Nothing

-- | Whether a variable can have the value of a literal.
possibleLiteral :: Facts -> Name -> Literal -> Bool
possibleLiteral facts name literal =
  case (rangeOf facts name, literalValue literal) of
    (Just range, Just value) -> inRange range value
    _ -> True

-- | The one value that a variable can have, when there is one.
knownValue :: Facts -> Name -> Maybe Integer
knownValue facts name = fromRange <|> fromDefinition
  where
    fromRange = do
      range <- rangeOf facts name
      low <- rangeLow range
      high <- rangeHigh range
      if low == high && Set.notMember low (rangeExcluded range) then Just low else Nothing
    fromDefinition = do
      (kind, left, right) <- Map.lookup name (factDefinitions facts)
      decide facts kind left right

-- | Every value that a variable can have, when the range is small.
finiteValues :: Facts -> Name -> Maybe [Integer]
finiteValues facts name = do
  range <- rangeOf facts name
  low <- rangeLow range
  high <- rangeHigh range
  if high - low < 16
    then Just [value | value <- [low .. high], Set.notMember value (rangeExcluded range)]
    else Nothing

-- | Forget every fact about the given names.
forgetNames :: Set Name -> Facts -> Facts
forgetNames names facts
  | Set.null names = facts
  | otherwise =
      Facts
        { factRanges = Map.withoutKeys (factRanges facts) names,
          factOrders = Map.filterWithKey (\(first, second) _ -> Set.notMember first names && Set.notMember second names) (factOrders facts),
          factDefinitions = Map.filterWithKey (\name (_, left, right) -> Set.notMember name names && kept left && kept right) (factDefinitions facts)
        }
  where
    kept value =
      case value of
        OperandVar name -> Set.notMember name names
        OperandValue _ -> True
