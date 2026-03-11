-- Copyright (C) 2016 Peter Selinger.
--
-- This file is free software and may be distributed under the terms
-- of the MIT license. Please see the file LICENSE for details.

-- | This module extends the SAT solver to arbitrary boolean formulas
-- (not necessarily in conjunctive normal form). This is done by the
-- standard trick of translating each boolean formula to a CNF,
-- preserving satisfiability and the number of solutions. The
-- translation is carefully done in such a way that the size of the
-- CNF is linear in the size of the formula.

module SAT.MiniSat.Formula where

import SAT.MiniSat.Variable
import SAT.MiniSat.Literals

import Control.Monad.Trans.State
import Control.Monad
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Prelude hiding (all)

-- ----------------------------------------------------------------------
-- * Boolean formulas

-- | The type of boolean formulas. It is parametric over a set of
-- variables /v/. We provide the usual boolean operators, including
-- implications and exclusive or. For convenience, we also provide the
-- list quantifiers 'All', 'Some', 'None', 'ExactlyOne', and
-- 'AtMostOne', as well as a general 'Let' operator that can be
-- used to reduce the size of repetitive formulas.

data Formula v =
  Var v                       -- ^ A variable.
  | Yes                       -- ^ The formula /true/.
  | No                        -- ^ The formula /false/.
  | Not (Formula v)           -- ^ Negation.
  | Formula v :&&: Formula v  -- ^ Conjunction.
  | Formula v :||: Formula v  -- ^ Disjunction.
  | Formula v :++: Formula v  -- ^ Exclusive or.
  | Formula v :->: Formula v  -- ^ Implication.
  | Formula v :<->: Formula v -- ^ If and only if.
  | All [Formula v]           -- ^ All are true.
  | Some [Formula v]          -- ^ At least one is true.
  | None [Formula v]          -- ^ None are true.
  | ExactlyOne [Formula v]    -- ^ Exactly one is true.
  | AtMostOne [Formula v]     -- ^ At most one is true.
  | Let (Formula v) (Formula v -> Formula v)
        -- ^ 'Let' /a/ /f/ is the formula \"let /x/=/a/ in /fx/\",
        -- which permits the subexpression /a/ to be re-used. It
        -- is logically equivalent to /fa/, but typically smaller.
  | Bound Integer             -- ^ For internal use only.

infixr 6 :&&:
infixr 5 :||:
infixr 4 :++:
infixr 2 :->:
infix  1 :<->:

instance (Eq v) => Eq (Formula v) where
  a == b = exformula 0 a == exformula 0 b

instance (Ord v) => Ord (Formula v) where
  compare a b = compare (exformula 0 a) (exformula 0 b)

instance (Show v) => Show (Formula v) where
  showsPrec d a = showsPrec d (exformula 0 a)

-- ----------------------------------------------------------------------
-- * Explicit formulas

-- | Same as 'Formula', except it uses explicit bound variables. This is
-- only used to define 'Eq', 'Ord', and 'Show' instances for
-- 'Formula'.
data ExFormula v =
  ExVar v                             -- ^ A variable.
  | ExTrue                            -- ^ The formula /true/.
  | ExFalse                           -- ^ The formula /false/.
  | ExNot (ExFormula v)               -- ^ Negation.
  | ExAnd (ExFormula v) (ExFormula v) -- ^ Conjunction.
  | ExOr (ExFormula v) (ExFormula v)  -- ^ Disjunction.
  | ExXOr (ExFormula v) (ExFormula v) -- ^ Exclusive or.
  | ExImp (ExFormula v) (ExFormula v) -- ^ Implication.
  | ExIff (ExFormula v) (ExFormula v) -- ^ If and only if.
  | ExAll [ExFormula v]               -- ^ All are true.
  | ExSome [ExFormula v]              -- ^ At least one is true.
  | ExNone [ExFormula v]              -- ^ None are true.
  | ExExactlyOne [ExFormula v]        -- ^ Exactly one is true.
  | ExAtMostOne [ExFormula v]         -- ^ At most one is true.
  | ExLet Integer (ExFormula v) (ExFormula v) -- ^ Let.
  | ExBound Integer                   -- ^ Bound variable.
  deriving (Eq, Ord)

-- | Convert a 'Formula' to an 'ExFormula'. The first argument is the
-- current nesting depth of let binders.
exformula :: Integer -> Formula v -> ExFormula v
exformula i (Var v) = ExVar v
exformula i Yes = ExTrue
exformula i No = ExFalse
exformula i (Not a) = ExNot (exformula i a)
exformula i (a :&&: b) = ExAnd (exformula i a) (exformula i b)
exformula i (a :||: b) = ExOr (exformula i a) (exformula i b)
exformula i (a :++: b) = ExXOr (exformula i a) (exformula i b)
exformula i (a :->: b) = ExImp (exformula i a) (exformula i b)
exformula i (a :<->: b) = ExIff (exformula i a) (exformula i b)
exformula i (All vs) = ExAll [exformula i v | v <- vs]
exformula i (Some vs) = ExSome [exformula i v | v <- vs]
exformula i (None vs) = ExNone [exformula i v | v <- vs]
exformula i (ExactlyOne vs) = ExExactlyOne [exformula i v | v <- vs]
exformula i (AtMostOne vs) = ExAtMostOne [exformula i v | v <- vs]
exformula i (Let a f) = ExLet i (exformula i a) (exformula (i+1) (f (Bound i)))
exformula i (Bound x)
  | 0 <= x && x < i = ExBound x
  | otherwise = error "MiniSat.Formula: 'Bound' is for internal use only"

-- | Names for bound variables.
variables :: [String]
variables = vs ++ [ v ++ show n | n <- [1..], v <- vs ]
  where
    vs = ["x", "y", "z", "u", "v", "w", "r", "s", "t"]

-- | Convert an integer to a bound variable name.
showBound :: Integer -> ShowS
showBound i = showString (variables !! (fromInteger i))

-- | The 'Show' instance of 'ExFormula' actually shows the corresponding
-- 'Formula'. We omit parentheses between associative identical
-- operators, but show some additional parentheses between different
-- operators for clarity.
instance (Show v) => Show (ExFormula v) where
  showsPrec d (ExVar v) = showParen (d > 10) $
    showString "Var " . showsPrec d v
  showsPrec d ExTrue = showString "Yes"
  showsPrec d ExFalse = showString "No"
  showsPrec d (ExNot a) = showParen (d > 10) $
    showString "Not " . showsPrec 11 a
  showsPrec d (ExAnd a b) = showParen (d > 6 || d == 4 || d == 5) $
    showsPrec 6 a . showString " :&&: " . showsPrec 6 b
  showsPrec d (ExOr a b) = showParen (d > 5 || d == 4) $
    showsPrec 5 a . showString " :||: " . showsPrec 5 b
  showsPrec d (ExXOr a b) = showParen (d > 4) $
    showsPrec 4 a . showString " :++: " . showsPrec 4 b
  showsPrec d (ExImp a b) = showParen (d > 2) $
    showsPrec 3 a . showString " :->: " . showsPrec 2 b
  showsPrec d (ExIff a b) = showParen (d > 1) $
    showsPrec 3 a . showString " :<->: " . showsPrec 3 b
  showsPrec d (ExAll as) = showParen (d > 10) $
    showString "All " . showsPrec 11 as
  showsPrec d (ExSome as) = showParen (d > 10) $
    showString "Some " . showsPrec 11 as
  showsPrec d (ExNone as) = showParen (d > 10) $
    showString "None " . showsPrec 11 as
  showsPrec d (ExExactlyOne as) = showParen (d > 10) $
    showString "ExactlyOne " . showsPrec 11 as
  showsPrec d (ExAtMostOne as) = showParen (d > 10) $
    showString "AtMostOne " . showsPrec 11 as
  showsPrec d (ExLet i a b) = showParen (d > 10) $
    showString "Let " . showsPrec 11 a . showString " (\\"
    . showBound i . showString " -> " . showsPrec 0 b . showString ")"
  showsPrec d (ExBound i) = showBound i

-- ----------------------------------------------------------------------
-- * Desugaring

-- | A version of 'Let' that binds a list of variables.
let_list :: [Formula v] -> ([Formula v] -> Formula v) -> Formula v
let_list [] f = f []
let_list (a:as) f = Let a (\x -> let_list as (\xs -> f (x:xs)))

-- | Implementation of 'All'.
all :: [Formula v] -> Formula v
all vs = foldl (:&&:) Yes vs

-- | Implementation of 'Some'.
some :: [Formula v] -> Formula v
some vs = foldl (:||:) No vs

-- | Implementation of 'None'.
none :: [Formula v] -> Formula v
none vs = Not (some vs)

-- | Implementation of 'AtMostOne'. Note that this translation is linear.
-- Without 'Let', the size of the translated formula would be /O/(/n/^2).
atMostOne :: [Formula v] -> Formula v
atMostOne [] = Yes
atMostOne [a] = Yes
atMostOne (a1:a2:as) =
  Let a1 (\x1 ->
  Let a2 (\x2 ->
    Not (x1 :&&: x2) :&&: atMostOne ((x1 :||: x2) : as)))

-- | Implementation of 'ExactlyOne'.
exactlyOne :: [Formula v] -> Formula v
exactlyOne as = let_list as (\xs -> some xs :&&: atMostOne xs)

-- ----------------------------------------------------------------------
-- * Reduced boolean formulas

-- | A reduced boolean formula doesn't contain the constants \"true\" or
-- \"false\", nor the connectives \"and\", \"implies\", or \"iff\",
-- nor list quantifiers. We use reduced formulas as an intermediate
-- representation between boolean formulas and De Morgan normal forms.
-- The primary purpose of this is to get rid of the constants \"true\"
-- and \"false\" in absorbing positions (i.e., \"/a/ or true\", \"/b/
-- and false\").
data RFormula v =
  RFVar v                           -- ^ A variable.
  | RFBound Integer                 -- ^ A bound variable.
  | RFNot (RFormula v)              -- ^ Negation.
  | RFOr (RFormula v) (RFormula v)  -- ^ Disjunction.
  | RFXOr (RFormula v) (RFormula v) -- ^ Exclusive or.
  | RFLet Integer (RFormula v) (RFormula v) -- ^ Let /x/ = /a/ in /b/.
    deriving (Show)

-- | Translate a boolean formula to reduced form, or one of the
-- constants 'True' or 'False'. The integer argument is the current
-- nesting depth of let binders, and is used to number local bound
-- variables.
rformula :: Integer -> Formula v -> Either Bool (RFormula v)
rformula i (Var v) = Right (RFVar v)
rformula i Yes = Left True
rformula i No = Left False
rformula i (Not a) =
  case rformula i a of
    Left x -> Left (not x)
    Right a'' -> Right (RFNot a'')
rformula i (a :||: b) =
  case (rformula i a, rformula i b) of
    (Left True, b'') -> Left True
    (Left False, b'') -> b''
    (a'', Left True) -> Left True
    (a'', Left False) -> a''
    (Right a'', Right b'') -> Right (RFOr a'' b'')
rformula i (a :++: b) =
  case (rformula i a, rformula i b) of
   (Left x, Left y) -> Left (x /= y)
   (Left True, Right b'') -> Right (RFNot b'')
   (Left False, Right b'') -> Right b''
   (Right a'', Left True) -> Right (RFNot a'')
   (Right a'', Left False) -> Right a''
   (Right a'', Right b'') -> Right (RFXOr a'' b'')
rformula i (a :&&: b) = rformula i (Not (Not a :||: Not b))
rformula i (a :->: b) = rformula i (Not a :||: b)
rformula i (a :<->: b) = rformula i (Not a :++: b)
rformula i (All vs) = rformula i (all vs)
rformula i (Some vs) = rformula i (some vs)
rformula i (None vs) = rformula i (none vs)
rformula i (AtMostOne vs) = rformula i (atMostOne vs)
rformula i (ExactlyOne vs) = rformula i (exactlyOne vs)
rformula i (Let a f) =
  case rformula i a of
    Left True -> rformula i (f Yes)
    Left False -> rformula i (f No)
    Right a'' ->
      case rformula (i+1) (f (Bound i)) of
        Left x -> Left x
        Right b'' -> Right (RFLet i a'' b'')
rformula i (Bound x) = Right (RFBound x)

-- ----------------------------------------------------------------------
-- * De Morgan formulas

-- | Formulas in De Morgan standard form. We omit the constant formulas
-- 'True' and 'False', as they should only occur at the top level.
data DMFormula v =
  DMPos v                                     -- ^ A positive literal.
  | DMNeg v                                   -- ^ A negative literal.
  | DMPosBound Integer                        -- ^ A positive bound variable.
  | DMNegBound Integer                        -- ^ A negative bound variable.
  | DMAnd (DMFormula v) (DMFormula v)         -- ^ Conjunction.
  | DMOr (DMFormula v) (DMFormula v)          -- ^ Disjunction.
  | DMXOr (DMFormula v) (DMFormula v)         -- ^ Exclusive or.
  | DMLet Integer (DMFormula v) (DMFormula v) -- ^ let /x/ = /a/ in /b/.
    
-- | Translate a reduced formula to De Morgan standard form.
demorgan :: RFormula v -> DMFormula v
demorgan (RFVar v) = DMPos v
demorgan (RFBound x) = DMPosBound x
demorgan (RFNot a) = demorgan_neg a
demorgan (RFOr a b) = DMOr (demorgan a) (demorgan b)
demorgan (RFXOr a b) = DMXOr (demorgan a) (demorgan b)
demorgan (RFLet x a b) = DMLet x (demorgan a) (demorgan b)

-- | Translate the negation of a reduced formula to De Morgan standard
-- form.
demorgan_neg :: RFormula v -> DMFormula v
demorgan_neg (RFVar v) = DMNeg v
demorgan_neg (RFBound x) = DMNegBound x
demorgan_neg (RFNot a) = demorgan a
demorgan_neg (RFOr a b) = DMAnd (demorgan_neg a) (demorgan_neg b)
demorgan_neg (RFXOr a b) = DMXOr (demorgan_neg a) (demorgan b)
demorgan_neg (RFLet x a b) = DMLet x (demorgan a) (demorgan_neg b)

-- ----------------------------------------------------------------------
-- * A monad for translation to CNF

-- | Literals over a set /v/, extended with countably many additional elements.
type ELit v = Lit (Either Integer v)

-- | A monad for translation to CNF. This monad keeps track of two kinds
-- of state: an integer counter to provide a supply of fresh
-- variables, and a list of definitional clauses.
data Trans v a = Trans (Integer -> (a, Integer, [[ELit v]]))

instance Monad (Trans v) where
  return a = Trans (\n -> (a, n, []))
  (Trans f) >>= g = Trans (\n ->
                            let (a1, n1, l1) = f n in
                            let Trans h = g a1 in
                            let (a2, n2, l2) = h n1 in
                            (a2, n2, l1 ++ l2))
  
instance Applicative (Trans v) where
  pure = return
  (<*>) = ap
  
instance Functor (Trans v) where
  fmap = liftM

-- | Run the 'Trans' monad.
runTrans :: Trans v a -> (a, [[ELit v]])
runTrans (Trans f) = (a, l)
  where
    (a, _, l) = f 0

-- | Return a fresh literal.
fresh_lit :: Trans v (ELit v)
fresh_lit = Trans (\n -> (Pos (Left n), n+1, []))

-- | Add some definitional clauses.
add_cnf :: [[ELit v]] -> Trans v ()
add_cnf cs = Trans (\n -> ((), n, cs))

-- ----------------------------------------------------------------------
-- * Translation from De Morgan to CNF

-- $ The simplest way to translate a formula, say
--
-- > (A ∧ B) ∨ (C ∧ (D ∨ E ∨ F)),
-- 
-- is to introduce a new variable for each intermediate result, i.e.,
-- to give clauses
--
-- > X, X ↔ Y ∨ Z, Y ↔ A ∧ B, Z ↔ C ∧ W, W ↔ D ∨ V, V ↔ E ∨ F.
--
-- (Each equivalence \"↔\" can be translated into CNF
-- straightforwardly).  Our translation is basically an optimized
-- version of this. The optimizations are: introduce additional
-- variables only when necessary; if the input formula is a CNF, leave
-- it unchanged. We give specialized translations of some formulas
-- depending on the context; for example, an exclusive or can be
-- translated in several different ways. For the above example, our
-- translation yields:
--
-- > Y ∨ Z, X ↔ A ∧ B, Z ↔ C ∧ W, W ↔ D ∨ E ∨ F.

-- | An environment is a mapping from local bound variables to
-- literals.
type Env v = Map Integer (ELit v)

-- | Translate a De Morgan formula to a SAT-equivalent CNF.  Since this
-- requires the introduction of fresh variables, the CNF is over a
-- larger variable set, and we work in the 'Trans' monad. Moreover,
-- the translation depends on an environment.
trans_cnf :: Env v -> DMFormula v -> Trans v [[ELit v]]
trans_cnf env (DMPos v) = return [[Pos (Right v)]]
trans_cnf env (DMNeg v) = return [[Neg (Right v)]]
trans_cnf env (DMPosBound n) = return [[x]]
  where
    x = env Map.! n
trans_cnf env (DMNegBound n) = return [[neg x]]
  where
    x = env Map.! n
trans_cnf env (DMAnd a b) = do
  a' <- trans_cnf env a
  b' <- trans_cnf env b
  return (a' ++ b')
trans_cnf env (DMOr a b) = do
  a' <- trans_or env a
  b' <- trans_or env b
  return [a' ++ b']
trans_cnf env (DMXOr a b) = do
  x <- trans_lit env a
  y <- trans_lit env b
  return [[x, y], [neg x, neg y]]
trans_cnf env (DMLet n a b) = do
  do_let env n a b trans_cnf

-- | Translate a De Morgan formula to a disjunction of literals.
trans_or :: Env v -> DMFormula v -> Trans v [ELit v]
trans_or env (DMXOr a b) = do
  x <- trans_lit env (DMXOr a b)
  return [x]
trans_or env (DMLet n a b) = do
  do_let env n a b trans_or
trans_or env a = do
  c <- trans_cnf env a
  or_of_cnf c

-- | Translate a De Morgan formula to a single literal.
trans_lit :: Env v -> DMFormula v -> Trans v (ELit v)
trans_lit env (DMXOr a b) = do
  x <- trans_lit env a
  y <- trans_lit env b
  z <- lit_of_xor x y
  return z
trans_lit env (DMLet n a b) = do
  do_let env n a b trans_lit
trans_lit env a = do
  c <- trans_cnf env a
  lit_of_cnf c

-- | Polymorphically translate a 'DMLet' expression. The fifth
-- argument is usually one of 'trans_cnf', 'trans_or', or 'trans_lit',
-- and is used to translate the body of the \"let\".
do_let :: Env v -> Integer -> DMFormula v -> DMFormula v -> (Env v -> DMFormula v -> Trans v a) -> Trans v a
do_let env n a b cont = do
  x <- trans_lit env a
  let env' = Map.insert n x env
  cont env' b
  
-- | Convert a CNF to a disjunction of literals.
or_of_cnf :: [[ELit v]] -> Trans v [ELit v]
or_of_cnf [d] = return d
or_of_cnf ds = do
  x <- lit_of_cnf ds
  return [x]

-- | Convert a CNF to a single literal.
lit_of_cnf :: [[ELit v]] -> Trans v (ELit v)
lit_of_cnf ds = do
  xs <- sequence (map lit_of_or ds)
  y <- lit_of_and xs
  return y

-- | Convert a conjunction of literals to a single literal.
lit_of_and :: [ELit v] -> Trans v (ELit v)
lit_of_and [l] = return l
lit_of_and cs = do
  x <- fresh_lit
  -- Define x <-> c1 ∧ ... ∧ cn
  add_cnf [[neg x, c] | c <- cs ]
  add_cnf [[x] ++ [neg c | c <- cs]]  
  return x

-- | Convert a disjunction of literals to a single literal.
lit_of_or :: [ELit v] -> Trans v (ELit v)
lit_of_or [l] = return l
lit_of_or ds = do
  x <- fresh_lit
  -- Define x <-> d1 ∨ ... ∨ dn
  add_cnf [ [x, neg d] | d <- ds ]
  add_cnf [[neg x] ++ [d | d <- ds]]
  return x

-- | Convert an exclusive or of two literals to a single literal.
lit_of_xor :: ELit v -> ELit v -> Trans v (ELit v)
lit_of_xor x y = do
  z <- fresh_lit
  -- Define z <-> x ⊕ y
  add_cnf [[z, x, neg y]]
  add_cnf [[z, neg x, y]]
  add_cnf [[neg z, x, y]]
  add_cnf [[neg z, neg x, neg y]]
  return z

-- ----------------------------------------------------------------------
-- * Top-level functions

-- | Translate a De Morgan formula to a SAT-equivalent CNF.
cnf_of_dm :: DMFormula v -> [[ELit v]]
cnf_of_dm f = l ++ a
  where
    env = Map.empty
    (a, l) = runTrans (trans_cnf env f)

-- | Translate a boolean formula to a SAT-equivalent CNF.
cnf_of_formula :: Formula v -> [[ELit v]]
cnf_of_formula f =
  case rformula 0 f of
   Left True -> []
   Left False -> [[]]
   Right rf -> cnf_of_dm (demorgan rf)    

-- ----------------------------------------------------------------------
-- * SAT solver API for boolean formulas

-- | Check whether a boolean formula is satisfiable.
satisfiable :: (Ord v) => Formula v -> Bool
satisfiable a = isJust (solve a)

-- | Return a satisfying assignment for the boolean formula, if any.
solve :: (Ord v) => Formula v -> Maybe (Map v Bool)
solve a = listToMaybe (solve_all a)

-- | Lazily enumerate all satisfying assignments for the boolean formula.
solve_all :: (Ord v) => Formula v -> [Map v Bool]
solve_all a = res
  where
    a' = cnf_of_formula a
    res' = cnf_solve_all a'
    res = map restrict res'
    restrict ans = Map.fromList [ (v, b) | (Right v, b) <- Map.toList ans ]
