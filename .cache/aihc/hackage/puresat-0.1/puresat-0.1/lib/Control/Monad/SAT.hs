{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}
-- | A monadic interface to the SAT (@PureSAT@) solver.
--
-- The interface is inspired by ST monad. 'SAT' and 'Lit' are indexed by a "state" token,
-- so you cannot mixup literals from different SAT computations.
module Control.Monad.SAT (
    -- * SAT Monad
    SAT,
    runSATMaybe,
    -- * Literals
    Lit,
    newLit,
    boostScore,
    -- ** Negation
    Neg (..),
    -- * Clauses
    addClause,
    assertAtLeastOne,
    assertAtMostOne,
    assertAtMostOnePairwise,
    assertAtMostOneSequential,
    assertEqual,
    assertAllEqual,
    -- ** Propositional formulas
    Prop,
    true, false,
    lit, (\/), (/\), (<->), (-->), xor, ite,
    addDefinition,
    addProp,
    -- ** Clause definitions
    trueLit,
    falseLit,
    addConjDefinition,
    addDisjDefinition,
    -- * Solving
    solve,
    solve_,
    -- * Simplification
    simplify,
    -- * Statistics
    numberOfVariables,
    numberOfClauses,
    numberOfLearnts,
    numberOfLearntLiterals,
    numberOfConflicts,
    numberOfRestarts,
) where

import Control.Monad   (forM_, unless)
import Data.Bits       (shiftR, testBit)
import Data.List       (tails)
import Data.Map.Strict (Map)
import Data.Set        (Set)
import Data.STRef      (STRef, newSTRef, readSTRef, writeSTRef)

import EST

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified PureSAT

-------------------------------------------------------------------------------
-- SAT Monad
-------------------------------------------------------------------------------

-- | Satisfiability monad.
newtype SAT s a = SAT { unSAT :: PureSAT.Solver s -> Lit s -> STRef s (Definitions s) -> EST UnsatException s a }
  deriving Functor

-- The SAT monad environment consists of
-- * A solver instance
-- * A literal constraint to be true.
-- * A map of asserted definitions, to dedup these for 'addDefinitions' and 'addProp' calls.
--   (we don't dedup clauses though - it's up to the solver).

type role SAT nominal representational

type Definitions s = Map (Set (Lit s)) (Lit s)

-- | Unsatisfiable exception.
--
-- It may be thrown by various functions: in particular 'solve' and 'solve_', but also 'addClause', 'simplify'.
--
-- The reason to use an exception is because after unsatisfiable state is reached the underlying solver instance is unusable.
-- You may use 'runSATMaybe' to catch it.
data UnsatException = UnsatException
  deriving (Show)

throwUnsatException :: EST UnsatException s a
throwUnsatException = earlyExitEST UnsatException

instance Applicative (SAT s) where
    pure x = SAT (\_ _ _ -> pure x)
    SAT f <*> SAT x = SAT (\s t r -> f s t r <*> x s t r)

instance Monad (SAT s) where
    m >>= k = SAT $ \s t r -> do
        x <- unSAT m s t r
        unSAT (k x) s t r

-- | Run 'SAT' computation.
runSATMaybe :: forall a. (forall s. SAT s a) -> Maybe a
runSATMaybe f = either (const Nothing) Just $ runEST $ do
    s <- liftST PureSAT.newSolver
    t <- liftST (PureSAT.newLit s)
    add_clause s [L t]
    r <- liftST (newSTRef Map.empty)
    unSAT f s (L t) r

-------------------------------------------------------------------------------
-- Literals
-------------------------------------------------------------------------------

-- | Literal.
--
-- To negate literate use 'neg'.
newtype Lit s = L { unL :: PureSAT.Lit }
  deriving (Eq, Ord)

type role Lit nominal

instance Show (Lit s) where
    showsPrec d (L (PureSAT.MkLit l))
        | p         = showParen (d > 6) (showChar '-' . shows v)
        | otherwise = shows v
      where
        i :: Int
        i = fromIntegral l

        -- PureSAT encodes polarity of literal in 0th bit.
        -- (this way normal order groups same variable literals).
        p :: Bool
        p = testBit i 0

        v :: Int
        v = shiftR i 1

class Neg a where
    neg :: a -> a

-- | Negate literal.
instance Neg (Lit s) where
   neg (L l) = L (PureSAT.neg l)

-- | Create fresh literal.
newLit :: SAT s (Lit s)
newLit = SAT $ \s _t _r -> do
    l <- liftST (PureSAT.newLit s)
    return (L l)

-- | Boost score of the literal
boostScore :: Lit s -> SAT s ()
boostScore (L l) = SAT $ \s _t _r -> liftST (PureSAT.boostScore s l)

-------------------------------------------------------------------------------
-- Prop
-------------------------------------------------------------------------------

-- | Propositional formula.
data Prop s
    = PTrue
    | PFalse
    | P (Prop1 s)
  deriving (Eq, Ord)

infixr 5 \/
infixr 6 /\

instance Show (Prop s) where
    showsPrec _ PTrue  = showString "true"
    showsPrec _ PFalse = showString "false"
    showsPrec d (P p)  = showsPrec d p

-- | True 'Prop'.
true :: Prop s
true = PTrue

-- | False 'Prop'.
false :: Prop s
false = PFalse

-- | Make 'Prop' from a literal.
lit :: Lit s -> Prop s
lit l = P (P1Lit l)

-- | Disjunction of propositional formulas, or.
(\/) :: Prop s -> Prop s -> Prop s
x \/ y = neg (neg x /\ neg y)

-- | Conjunction of propositional formulas, and.
(/\) :: Prop s -> Prop s -> Prop s
PFalse /\ _      = PFalse
_      /\ PFalse = PFalse
PTrue  /\ y      = y
x      /\ PTrue  = x
P x    /\ P y    = P (p1and x y)

-- | Implication of propositional formulas.
(-->) :: Prop s -> Prop s -> Prop s
x --> y = neg x \/ y

-- | Equivalence of propositional formulas.
(<->) :: Prop s -> Prop s -> Prop s
x <-> y = (x --> y) /\ (y --> x)

-- | Exclusive or, not equal of propositional formulas.
xor :: Prop s -> Prop s -> Prop s
xor x y = x <-> neg y

-- | If-then-else.
--
-- Semantics of @'ite' c t f@ are @ (c '/\' t) '\/' ('neg' c '/\' f)@.
--
ite :: Prop s -> Prop s -> Prop s -> Prop s
-- ite c t f = (c /\ t) \/ (neg c /\ f)
ite c t f = (c \/ f) /\ (neg c \/ t) /\ (t \/ f) -- this encoding makes (t == f) case propagate even when c is yet undecided.

-- | Negation of propositional formulas.
instance Neg (Prop s) where
    neg PTrue  = PFalse
    neg PFalse = PTrue
    neg (P p)  = P (p1neg p)

-------------------------------------------------------------------------------
-- Prop1
-------------------------------------------------------------------------------

data Prop1 s
    = P1Lit !(Lit s)
    | P1Nnd !(Set (PropA s))
    | P1And !(Set (PropA s))
  deriving (Eq, Ord)

data PropA s
    = PALit !(Lit s)
    | PANnd !(Set (PropA s))
  deriving (Eq, Ord)

instance Show (Prop1 s) where
    showsPrec d (P1Lit l)  = showsPrec d l
    showsPrec _ (P1And xs) = showNoCommaListWith shows (Set.toList xs)
    showsPrec _ (P1Nnd xs) = showChar '-' . showNoCommaListWith shows (Set.toList xs)

instance Show (PropA s) where
    showsPrec d (PALit l)  = showsPrec d l
    showsPrec _ (PANnd xs) = showChar '-' . showNoCommaListWith shows (Set.toList xs)

showNoCommaListWith :: (a -> ShowS) ->  [a] -> ShowS
showNoCommaListWith _     []     s = "[]" ++ s
showNoCommaListWith showx (x:xs) s = '[' : showx x (showl xs)
  where
    showl []     = ']' : s
    showl (y:ys) = ' ' : showx y (showl ys)

p1and :: Prop1 s -> Prop1 s -> Prop1 s
p1and p@(P1Lit x) (P1Lit y)
    | x == y    = p
    | otherwise = P1And (double (PALit x) (PALit y))
p1and p@(P1Nnd x) (P1Nnd y)
    | x == y    = p
    | otherwise = P1And (double (PANnd x) (PANnd y))
p1and (P1Lit x)  (P1Nnd y)  = P1And (double (PALit x) (PANnd y))
p1and (P1Nnd x)  (P1Lit y)  = P1And (double (PANnd x) (PALit y))
p1and (P1Lit x)  (P1And ys) = P1And (Set.insert (PALit x) ys)
p1and (P1Nnd x)  (P1And ys) = P1And (Set.insert (PANnd x) ys)
p1and (P1And xs) (P1Lit y)  = P1And (Set.insert (PALit y) xs)
p1and (P1And xs) (P1Nnd y)  = P1And (Set.insert (PANnd y) xs)
p1and (P1And xs) (P1And ys) = P1And (Set.union xs ys)

p1neg :: Prop1 s -> Prop1 s
p1neg (P1Lit l)  = P1Lit (neg l)
p1neg (P1Nnd xs) = P1And xs
p1neg (P1And xs) = P1Nnd xs

double :: Ord a => a -> a -> Set a
double x y = Set.insert x (Set.singleton y)

-------------------------------------------------------------------------------
-- Clause definitions
-------------------------------------------------------------------------------

-- | Add conjunction definition.
--
-- @addConjDefinition x ys@ asserts that @x ↔ ⋀ yᵢ@
addConjDefinition :: Lit s -> [Lit s] -> SAT s ()
addConjDefinition x zs = do
    y <- add_definition (Set.fromList zs)
    if x == y
    then return ()
    else assertEqual x y

-- | Add disjunction definition.
--
-- @addDisjDefinition x ys@ asserts that @x ↔ ⋁ yᵢ@
--
addDisjDefinition :: Lit s -> [Lit s] -> SAT s ()
addDisjDefinition x ys = addConjDefinition (neg x) (fmap neg ys)
-- Implementation: @(x ↔ ⋁ yᵢ) ↔ (¬x ↔ ⋀ ¬xyᵢ)@

-------------------------------------------------------------------------------
-- Methods
-------------------------------------------------------------------------------

-- | Assert that given 'Prop' is true.
--
-- This is equivalent to
--
-- @
-- addProp p = do
--     l <- addDefinition p
--     addClause l
-- @
--
-- but avoid creating the definition, asserting less clauses.
--
addProp :: Prop s -> SAT s ()
addProp PTrue  = return ()
addProp PFalse = SAT $ \s t _-> add_clause s [neg t]
addProp (P p)  = add_prop p

-- | Add definition of 'Prop'. The resulting literal is equivalent to the argument 'Prop'.
--
addDefinition :: Prop s -> SAT s (Lit s)
addDefinition PTrue  = trueLit
addDefinition PFalse = falseLit
addDefinition (P p)  = addDefinition1 p

-- | True literal.
trueLit :: SAT s (Lit s)
trueLit = SAT $ \_s t _ -> return t

-- | False literal
falseLit :: SAT s (Lit s)
falseLit = SAT $ \_s t _ -> return (neg t)

addDefinition1 :: Prop1 s -> SAT s (Lit s)
addDefinition1 = tseitin1

-- | Add conjuctive definition.
add_definition :: Set (Lit s) -> SAT s (Lit s)
add_definition ps
    | Set.null ps
    = trueLit

add_definition ps = SAT $ \s _ defsRef -> do
    defs <- liftST (readSTRef defsRef)
    case Map.lookup ps defs of
        Just d -> return d
        Nothing -> do
            d' <- liftST (PureSAT.newLit s)
            let d = L d'

            -- putStrLn $ "add_definition " ++ show (Set.toList ps) ++ " = " ++ show d

            -- d ∨ ¬x₁ ∨ ¬x₂ ∨ ... ∨ ¬xₙ
            add_clause s (d : map neg (Set.toList ps))

            -- ¬d ∨ x₁
            -- ¬d ∨ x₂
            --  ...
            -- ¬d ∨ xₙ
            forM_ ps $ \p -> do
                add_clause s [neg d, p]

            -- save the definition.
            liftST (writeSTRef defsRef $! Map.insert ps d defs)

            return d

-- top-level add prop: CNF
add_prop :: Prop1 s  -> SAT s ()
add_prop (P1Lit l) = addClause [l]
add_prop (P1And xs) = forM_ xs add_prop'
add_prop (P1Nnd xs) = do
    ls <- traverse tseitinA (Set.toList xs)
    addClause (map neg ls)

-- first-level: Clauses
add_prop' :: PropA s -> SAT s ()
add_prop' (PALit l) = addClause [l]
add_prop' (PANnd xs) = do
    ls <- traverse tseitinA (Set.toList xs)
    addClause (map neg ls)

tseitin1 :: Prop1 s -> SAT s (Lit s)
tseitin1 (P1Lit l) = return l
tseitin1 (P1And xs) = do
    xs' <- traverse tseitinA (Set.toList xs)
    add_definition (Set.fromList xs')
tseitin1 (P1Nnd xs) = do
    xs' <- traverse tseitinA (Set.toList xs)
    neg <$> add_definition (Set.fromList xs')

tseitinA :: PropA s -> SAT s (Lit s)
tseitinA (PALit l) = return l
tseitinA (PANnd xs) = do
    xs' <- traverse tseitinA (Set.toList xs)
    neg <$> add_definition (Set.fromList xs')

-------------------------------------------------------------------------------
-- Constraints
-------------------------------------------------------------------------------

-- | Add a clause to the solver.
addClause :: [Lit s] -> SAT s ()
addClause ls = SAT $ \s _t _r -> add_clause s ls

add_clause :: PureSAT.Solver s -> [Lit s] -> EST UnsatException s ()
add_clause s ls = do
    -- putStrLn $ "add_clause " ++ show ls
    ok <- liftST (PureSAT.addClause s (map unL ls))
    unless ok throwUnsatException

-- | At least one -constraint.
--
-- Alias to 'addClause'.
assertAtLeastOne :: [Lit s] -> SAT s ()
assertAtLeastOne = addClause

-- | At most one -constraint.
--
-- Uses 'atMostOnePairwise' for lists of length 2 to 5
-- and 'atMostOneSequential' for longer lists.
--
-- The cutoff is chosen by picking encoding with least clauses:
-- For 5 literals, 'atMostOnePairwise' needs 10 clauses and 'assertAtMostOneSequential' needs 11 (and 4 new variables).
-- For 6 literals, 'atMostOnePairwise' needs 15 clauses and 'assertAtMostOneSequential' needs 14.
--
assertAtMostOne :: [Lit s] -> SAT s ()
assertAtMostOne ls = case ls of
    []          -> return ()
    [_]         -> return ()
    [_,_]       -> assertAtMostOnePairwise ls
    [_,_,_]     -> assertAtMostOnePairwise ls
    [_,_,_,_]   -> assertAtMostOnePairwise ls
    [_,_,_,_,_] -> assertAtMostOnePairwise ls
    _           -> assertAtMostOneSequential ls

-- | At most one -constraint using pairwise encoding.
--
-- \[
-- \mathrm{AMO}(x_1, \ldots, x_n) = \bigwedge_{1 \le i < j \le n} \neg x_i \lor \neg x_j
-- \]
--
-- \(n(n-1)/2\) clauses, zero auxiliary variables.
--
assertAtMostOnePairwise :: [Lit s] -> SAT s ()
assertAtMostOnePairwise literals = mapM_ f (tails literals) where
    f :: [Lit s] -> SAT s ()
    f [] = return ()
    f (l:ls) = mapM_ (g l) ls

    g :: Lit s -> Lit s -> SAT s ()
    g l1 l2 = addClause [neg l1, neg l2]

-- | At most one -constraint using sequential counter encoding.
--
-- \[
-- \mathrm{AMO}(x_1, \ldots, x_n) =
--  (\neg x_1 \lor s_1) \land
--  (\neg x_n \lor \neg s_{n-1}) \land
--  \bigwedge_{1 < i < n} (\neg x_i \lor a_i) \land (\neg a_{i-1} \lor a_i) \land (\neg x_i \lor \neg a_{i-1})
-- \]
--
-- Sinz, C.: Towards an optimal CNF encoding of Boolean cardinality constraints, Proceedings of Principles and Practice of Constraint Programming (CP), 827–831 (2005)
--
-- \(3n-4\) clauses, \(n-1\) auxiliary variables.
--
-- We optimize the two literal case immediately ([resolution](https://en.wikipedia.org/wiki/Resolution_(logic)) on \(s_1\).
--
-- \[
-- (\neg x_1 \lor s_1) \land (\neg x_2 \lor \neg s_1) \Longrightarrow \neg x_1 \lor \neg x_2
-- \]
--
assertAtMostOneSequential :: [Lit s] -> SAT s ()
assertAtMostOneSequential []         = return ()
assertAtMostOneSequential [_]        = return ()
assertAtMostOneSequential [x1,x2]    = addClause [neg x1, neg x2]
assertAtMostOneSequential (xn:x1:xs) = do
    a1 <- newLit
    addClause [neg x1, a1]
    go a1 xs
  where
     go an1 [] = addClause [neg xn, neg an1]
     go ai1 (xi:xis) = do
        ai <- newLit
        addClause [neg xi, ai]
        addClause [neg ai1, ai]
        addClause [neg xi, neg ai1]
        go ai xis

-- | Assert that two literals are equal.
assertEqual :: Lit s -> Lit s -> SAT s ()
assertEqual l l'
    | l == l'   = return ()
    | otherwise = do
        addClause [l, neg l']
        addClause [neg l, l']

-- | Assert that all literals in the list are equal.
assertAllEqual :: [Lit s] -> SAT s ()
assertAllEqual []     = return ()
assertAllEqual (l:ls) = forM_ (Set.fromList ls) $ \l' -> assertEqual l l'

-------------------------------------------------------------------------------
-- Solving
-------------------------------------------------------------------------------

-- | Search without returning a model.
solve_ :: SAT s ()
solve_ = SAT $ \s _t _r -> do
    ok <- liftST (PureSAT.solve s)
    unless ok throwUnsatException

-- | Search and return a model.
solve :: Traversable model => model (Lit s) -> SAT s (model Bool)
solve model = SAT $ \s _t _r -> do
    ok <- liftST (PureSAT.solve s)
    unless ok throwUnsatException

    traverse (getSym s) model
  where
    getSym :: PureSAT.Solver s -> Lit s -> EST UnsatException s Bool
    getSym s (L l) = liftST (PureSAT.modelValue s l)

-------------------------------------------------------------------------------
-- Simplification
-------------------------------------------------------------------------------

-- | Removes already satisfied clauses.
simplify :: SAT s ()
simplify = SAT $ \s _t _r -> do
    ok <- liftST (PureSAT.simplify s)
    unless ok throwUnsatException

-------------------------------------------------------------------------------
-- Statistics
-------------------------------------------------------------------------------

-- | The current number of variables.
numberOfVariables :: SAT s Int
numberOfVariables = SAT $ \s _t _r -> liftST $ PureSAT.num_vars s

-- | The current number of original clauses.
numberOfClauses :: SAT s Int
numberOfClauses = SAT $ \s _t _r -> liftST $ PureSAT.num_clauses s

-- | The current number of learnt clauses.
numberOfLearnts :: SAT s Int
numberOfLearnts = SAT $ \s _t _r -> liftST $ PureSAT.num_learnts s

-- | The current number of learnt literals.
numberOfLearntLiterals :: SAT s Int
numberOfLearntLiterals = SAT $ \s _t _r -> liftST $ PureSAT.num_learnt_literals s

-- | The current number of conflicts.
numberOfConflicts :: SAT s Int
numberOfConflicts = SAT $ \s _t _r -> liftST $ PureSAT.num_conflicts s

-- | The current number of conflicts.
numberOfRestarts :: SAT s Int
numberOfRestarts = SAT $ \s _t _r -> liftST $ PureSAT.num_restarts s
