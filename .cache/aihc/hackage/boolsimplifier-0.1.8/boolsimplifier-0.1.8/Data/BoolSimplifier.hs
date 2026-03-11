{- |
Module      :  Data.BoolSimplifier
Copyright   :  (c) Gershom Bazerman, Jeff Polakow 2011
License     :  BSD 3 Clause
Maintainer  :  gershomb@gmail.com
Stability   :  experimental

Machinery for representing and simplifying simple propositional formulas. Type families are used to maintain a simple normal form, taking advantage of the duality between \"And\" and \"Or\". Additional tools are provided to pull out common atoms in subformulas and otherwise iterate until a simplified fixpoint. Full and general simplification is NP-hard, but the tools here can take typical machine-generated formulas and perform most simplifications that could be spotted and done by hand by a reasonable programmer.

While there are many functions below, only 'qAtom', 'andq'(s), 'orq'(s), and 'qNot' need be used directly to build expressions. 'simplifyQueryRep' performs a basic simplification, 'simplifyIons' works on expressions with negation to handle their reduction, and 'fixSimplifyQueryRep' takes a function built out of any combination of basic simplifiers (you can write your own ones taking into account any special properties of your atoms) and runs it repeatedly until it ceases to reduce the size of your target expression.

The general notion is either that you build up an expression with these combinators directly, simplify it further, and then transform it to a target semantics, or that an expression in some AST may be converted into a normal form expression using such combinators, and then simplified and transformed back to the original AST.

Here are some simple interactions:

> Prelude Data.BoolSimplifier> (qAtom "A") `orq` (qAtom "B")
> QOp | fromList [QAtom Pos "A",QAtom Pos "B"] fromList []

> Prelude Data.BoolSimplifier> ppQueryRep $ (qAtom "A") `orq` (qAtom "B")
> "(A | B)"

> Prelude Data.BoolSimplifier> ppQueryRep $ ((qAtom "A") `orq` (qAtom "B") `andq` (qAtom "A"))
> "(A)"

> Prelude Data.BoolSimplifier> ppQueryRep $ ((qAtom "A") `orq` (qAtom "B") `andq` (qAtom "A" `orq` qAtom "C"))
> "((A | B) & (A | C))"

> Prelude Data.BoolSimplifier> ppQueryRep $ simplifyQueryRep $ ((qAtom "A") `orq` (qAtom "B") `andq` (qAtom "A" `orq` qAtom "C"))
> "((A | (B & C)))"
-}

{-# LANGUAGE
             EmptyDataDecls,
             FlexibleContexts,
             FlexibleInstances,
             FunctionalDependencies,
             GADTs,
             MultiParamTypeClasses,
             OverlappingInstances,
             PatternGuards,
             ScopedTypeVariables,
             TypeFamilies,
             TypeSynonymInstances,
             UndecidableInstances
 #-}

module Data.BoolSimplifier where

import Prelude hiding (tail, init, head, last, minimum, maximum, foldr1, foldl1, (!!), read)

import Data.List(intercalate, maximumBy)
import Data.Ord(comparing)
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Set as S
import Data.Set(Set)
import Data.Foldable (foldMap)
import qualified Data.Foldable as F

{-
-}


-- | We'll start with three types of formulas: disjunctions, conjunctions, and atoms
data QOrTyp
data QAndTyp
data QAtomTyp

instance Show QOrTyp where
    show _ = "|"
instance Show QAndTyp where
    show _ = "&"


-- | disjunction is the dual of conjunction and vice-versa
type family QFlipTyp t :: *
type instance QFlipTyp QOrTyp = QAndTyp
type instance QFlipTyp QAndTyp = QOrTyp

{-|

  A formula is either an atom (of some type, e.g. @String@).

  A non-atomic formula (which is either a disjunction or a conjunction) is
  n-ary and consists of a @Set@ of atoms and a set of non-atomic subformulas of
  dual connective, i.e. the non-atomic subformulas of a disjunction must all
  be conjunctions.  The type system enforces this since there is no @QFlipTyp@
  instance for @QAtomTyp@.

-}
data QueryRep qtyp a where
    QAtom :: (Ord a) => a -> QueryRep QAtomTyp a
    QOp   :: (Show qtyp, Ord a) => Set (QueryRep QAtomTyp a) -> Set (QueryRep (QFlipTyp qtyp) a) -> QueryRep qtyp a


extractAs :: QueryRep qtyp a -> Set (QueryRep QAtomTyp a)
extractAs (QOp as _) = as
extractAs _ = S.empty

extractCs :: QueryRep qtyp a -> Set (QueryRep (QFlipTyp qtyp) a)
extractCs (QOp _ cs) = cs
extractCs _ = S.empty

-- | convenience constructors, not particularly smart
qOr :: Ord a => Set (QueryRep QAtomTyp a) -> Set (QueryRep QAndTyp a) -> QueryRep QOrTyp a
qOr = QOp
qAnd :: Ord a => Set (QueryRep QAtomTyp a) -> Set (QueryRep QOrTyp a) -> QueryRep QAndTyp a
qAnd = QOp


instance (Eq a) => Eq (QueryRep qtyp a) where
    (QAtom x) == (QAtom y) = x == y
    (QOp as cs) == (QOp as' cs') = as == as' && cs == cs'
    _ == _ = False  -- can't happen

instance (Ord a) => Ord (QueryRep qtyp a) where
    compare (QAtom x) (QAtom y) = compare x y
    compare (QOp as cs) (QOp as' cs') = compare as as' `mappend` compare cs cs'
    compare (QAtom _) _ = GT  -- can't happen
    compare _ _ = LT  -- can't happen

instance (Show a) => Show (QueryRep qtyp a) where
    show (QAtom x) = "QAtom " ++ show x
    show (QOp as cs) = intercalate " " ["QOp", show (undefined :: qtyp), show as, show cs]

-- | pretty printer class
class PPQueryRep a where
    ppQueryRep :: a -> String

instance PPQueryRep (QueryRep qtyp String) where
    ppQueryRep (QAtom s) = s
    ppQueryRep (QOp as cs) = "(" ++
                             intercalate (" " ++ show (undefined::qtyp) ++ " ")
                                         (map ppQueryRep (S.toList as) ++ map ppQueryRep (S.toList cs)) ++
                             ")"


-- | smart constructor for @QOp@
--   does following optimization: a \/\\ (a \\\/ b) \<\-\> a, or dually: a \\\/ (a \/\\ b) \<\-\> a
qop :: (Ord a,
        Show qtyp,
        Show (QFlipTyp qtyp),
        QFlipTyp (QFlipTyp qtyp) ~ qtyp
       ) =>
       Set (QueryRep QAtomTyp a) -> Set (QueryRep (QFlipTyp qtyp) a) -> QueryRep qtyp a
qop as cs = QOp as' $ S.filter (\c -> not $ any (c `hasClause`) $ S.toList as') cs'
    where
      as' = S.unions [as, newas, neweras]

      cs' = S.unions [remainingcs, newcs]

      isUnaryOp (QOp as'' cs'') = S.size cs'' + S.size as'' == 1
      isUnaryOp _ = False

      -- | Each @unarycs@ has type @QOp (QFlipTyp qtyp) a@ and is either @QOp {a} {}@ or @QOp {} {q}@
      --   Note that @QOp {a} {}@ = @a@ and @QOp {} {q}@ = @q@
      (unarycs, remainingcs) = S.partition isUnaryOp cs

      newas = foldMap extractAs unarycs

      (newcs, neweras) = extractAtomCs unarycs


extractAtomCs :: (Ord a,
                  Show qtyp,
                  Show (QFlipTyp qtyp),
                  QFlipTyp (QFlipTyp qtyp) ~ qtyp
                 ) =>
                 Set (QueryRep qtyp a) -> (Set (QueryRep qtyp a), Set (QueryRep QAtomTyp a))
extractAtomCs cs = (opClauses, atomClauses)
    where
      cs' = foldMap extractCs cs
      atomClauses = foldMap extractAs cs'
      opClauses = foldMap extractCs cs'


{-|

QueryReps can be queried for clauses within them, and clauses within them can be extracted.

-}
class HasClause fife qtyp
    where hasClause :: QueryRep fife a -> QueryRep qtyp a -> Bool
          stripClause :: QueryRep qtyp a -> QueryRep fife a -> QueryRep fife a

instance HasClause fife QAtomTyp
    where hasClause (QOp as _) c@(QAtom _) = c `S.member` as
          hasClause _ _ = False
          stripClause c (QOp as cs) = QOp (S.delete c as) cs
          stripClause _ x = x

instance (QFlipTyp fife ~ qtyp) => HasClause fife qtyp
    where hasClause (QOp _ cs) c@(QOp _ _) = c `S.member` cs
          hasClause _ _ = False
          stripClause c (QOp as cs) = QOp as (S.delete c cs)
          stripClause _ x = x

-- | convenience functions
andqs :: Ord a => (CombineQ a qtyp QAndTyp) => [QueryRep qtyp a] -> QueryRep QAndTyp a
andqs = foldr andq (qop S.empty S.empty)

orqs :: Ord a => (CombineQ a qtyp QOrTyp) => [QueryRep qtyp a] -> QueryRep QOrTyp a
orqs = foldr orq (qop S.empty S.empty)


-- | smart constructors for @QueryRep@
class CombineQ a qtyp1 qtyp2 where
    andq :: QueryRep qtyp1 a -> QueryRep qtyp2 a -> QueryRep QAndTyp a
    orq  :: QueryRep qtyp1 a -> QueryRep qtyp2 a -> QueryRep QOrTyp  a

instance Ord a => CombineQ a QAndTyp QAndTyp where
    andq (QOp as cs) (QOp as' cs') = qop (S.union as as') (S.union cs cs')

    orq x y = qop S.empty (S.fromList [x,y])

instance Ord a => CombineQ a QAndTyp QOrTyp where
    andq (QOp as cs) y = qop as (S.insert y cs)

    orq x (QOp as cs)  = qop as (S.insert x cs)

instance Ord a => CombineQ a QAndTyp QAtomTyp where
    andq (QOp as cs) y = qop (S.insert y as) cs

    orq x y = qop (S.singleton y) (S.singleton x)


instance Ord a => CombineQ a QOrTyp QAndTyp where
    andq x y = andq y x
    orq  x y = orq  y x

instance Ord a => CombineQ a QOrTyp QOrTyp where
    andq x y = qop S.empty (S.fromList [x,y])
    orq (QOp as cs) (QOp as' cs') = qop (S.union as as') (S.union cs cs')

instance Ord a => CombineQ a QOrTyp QAtomTyp where
    andq x y = qop (S.singleton y) (S.singleton x)
    orq (QOp as cs) y = qop (S.insert y as) cs

instance Ord a => CombineQ a QAtomTyp QAndTyp where
    andq x y = andq y x
    orq  x y = orq  y x

instance Ord a => CombineQ a QAtomTyp QOrTyp where
    andq x y = andq y x
    orq  x y = orq  y x

instance Ord a => CombineQ a QAtomTyp QAtomTyp where
    andq x y = qop (S.fromList [x,y]) S.empty
    orq  x y = qop (S.fromList [x,y]) S.empty


-- | (a \/\\ b) \\\/ (a \/\\ c) \\\/ d \<\-\> (a \/\\ (b \\\/ c)) \\\/ d
-- (and also the dual)
simplifyQueryRep :: (Ord a, Show (QFlipTyp qtyp), Show (QFlipTyp (QFlipTyp qtyp)), QFlipTyp (QFlipTyp qtyp) ~ qtyp) =>
                    QueryRep qtyp a -> QueryRep qtyp a
simplifyQueryRep (QOp as cs')
        | Just (comVal, comCs, restCs) <- getCommonClauseAs cs = simplifyQueryRep $
                  qop as (S.insert (qop (S.singleton comVal) (S.singleton $ qop S.empty comCs)) restCs)

        | Just (comVal, comCs, restCs) <- getCommonClauseCs cs = simplifyQueryRep $
                  qop as (S.insert (qop S.empty $ S.fromList [comVal, qop S.empty comCs]) restCs)

        | otherwise = QOp as cs
      where
        cs = S.map simplifyQueryRep cs'

simplifyQueryRep x = x

-- | Given a set of QueryReps, extracts a common clause if possible, returning the clause, the terms from which the clause has been extracted, and the rest.
getCommonClauseAs :: Ord a => Set (QueryRep fife a) -> Maybe (QueryRep QAtomTyp a,
                                                              Set (QueryRep fife a),
                                                              Set (QueryRep fife a))
getCommonClauseAs cs
    | M.size mp > 0 && countMax > (1::Int) = Just $ (maxClause, S.map (stripClause maxClause) com, rest)
    | otherwise = Nothing
  where
    (com, rest) = S.partition (`hasClause` maxClause) cs
    mp = mkClauseMap cs
    (maxClause, countMax) =  maximumByNote "getCommonClause" (comparing snd) $ M.toList mp
    mkClauseMap = foldr go M.empty . F.concatMap (S.toList . extractAs)
      where go c x = M.insertWith (+) c 1 x

getCommonClauseCs :: Ord a => Set (QueryRep fife a) -> Maybe (QueryRep (QFlipTyp fife) a,
                                                              Set (QueryRep fife a),
                                                              Set (QueryRep fife a))
getCommonClauseCs cs
    | M.size mp > 0 && countMax > (1::Int) = Just $ (maxClause, S.map (stripClauseLocal maxClause) com, rest)
    | otherwise = Nothing
  where
    (com, rest) = S.partition (`hasClauseLocal` maxClause) cs
    mp = mkClauseMap cs
    (maxClause, countMax) =  maximumByNote "getCommonClause" (comparing snd) $ M.toList mp
    mkClauseMap = foldr go M.empty . F.concatMap (S.toList . extractCs)

    go c x = M.insertWith (+) c 1 x

    hasClauseLocal (QOp _ css) c@(QOp _ _) = c `S.member` css
    hasClauseLocal _ _ = False

    stripClauseLocal c (QOp as css) = QOp as (S.delete c css)
    stripClauseLocal _ x = x

-- | Takes any given simplifier and repeatedly applies it until it ceases to reduce the size of the query reprepresentation.
fixSimplifyQueryRep  :: (QueryRep qtyp a -> QueryRep qtyp a) -> QueryRep qtyp a -> QueryRep qtyp a
fixSimplifyQueryRep simplify x
    | initl <= endl = x
    | otherwise = fixSimplifyQueryRep simplify res
  where
    res = simplify x
    initl = qSize x
    endl  = qSize res

    qSize :: QueryRep qtyp a -> Int
    qSize (QOp as cs) = sum (map qSize $ S.toList as) +
                        sum (map qSize $ S.toList cs)
    qSize (QAtom _) = 1


-- | We can wrap any underying atom dype in an Ion to give it a "polarity" and add handling of "not" to our simplification tools.
data Ion a = Neg a | Pos a deriving (Eq, Ord, Show)

qAtom :: Ord a => a -> QueryRep QAtomTyp (Ion a)
qAtom = QAtom . Pos

isEmptyQR, isConstQR :: QueryRep qtyp a -> Bool
isEmptyQR (QOp as cs) = S.null as && S.null cs
isEmptyQR _ = False

isConstQR (QOp as cs) | S.null as && S.size cs == 1 = isEmptyQR (S.findMin cs)
isConstQR _ = False

instance PPQueryRep (QueryRep QAndTyp (Ion String)) where
--    ppQueryRep (QAtom (Pos s)) = s
--    ppQueryRep (QAtom (Neg s)) = "~" ++ s
    ppQueryRep q@(QOp as cs)
        | isEmptyQR q || isConstQR q = ppConstQR q
        | otherwise = "(" ++
                      intercalate (" " ++ show (undefined::QAndTyp) ++ " ")
                                  (map ppQueryRep (S.toList as) ++ map ppQueryRep (S.toList cs)) ++
                      ")"

instance PPQueryRep (QueryRep QOrTyp (Ion String)) where
--    ppQueryRep (QAtom (Pos s)) = s
--    ppQueryRep (QAtom (Neg s)) = "~" ++ s
    ppQueryRep q@(QOp as cs)
        | isEmptyQR q || isConstQR q = ppConstQR q
        | otherwise = "(" ++
                      intercalate (" " ++ show (undefined::QOrTyp) ++ " ")
                                  (map ppQueryRep (S.toList as) ++ map ppQueryRep (S.toList cs)) ++
                      ")"

instance PPQueryRep (QueryRep QAtomTyp (Ion String)) where
    ppQueryRep (QAtom (Pos s)) = s
    ppQueryRep (QAtom (Neg s)) = "~" ++ s
    ppQueryRep (QOp _ _) = error "the type system does not work"

class PPConstQR qtyp where
    ppConstQR :: QueryRep qtyp a -> String
instance PPConstQR QAndTyp where
    ppConstQR q | isEmptyQR q = "False"
                | otherwise = "True"
instance PPConstQR QOrTyp where
    ppConstQR q | isEmptyQR q = "True"
                | otherwise = "False"
instance PPConstQR a where
    ppConstQR _ = error "impossible PPConstQR"


class QNot qtyp where
    type QNeg qtyp
    qNot :: QueryRep qtyp (Ion a) -> QueryRep (QNeg qtyp) (Ion a)

instance QNot QAtomTyp where
    type QNeg QAtomTyp = QAtomTyp
    qNot (QAtom (Neg a)) = QAtom (Pos a)
    qNot (QAtom (Pos a)) = QAtom (Neg a)
    qNot _ = error "qNot"

instance QNot QOrTyp where
    type QNeg QOrTyp = QAndTyp
    qNot (QOp as cs) = QOp (S.map qNot as) (S.map qNot cs)

instance QNot QAndTyp where
    type QNeg QAndTyp = QOrTyp
    qNot (QOp as cs) = QOp (S.map qNot as) (S.map qNot cs)

-- |
-- >  a  /\  (b \/ ~b)  /\  (c \/ d)   <->   a /\ (c \/ d)
-- >  a  /\  ~a         /\  (b \/ c)   <->   False
-- >         (a \/ ~a)  /\  (b \/ ~b)  <->   True  (*)
--
-- and duals
--
-- > N.B. 0-ary \/ is False and 0-ary /\ is True
--
simplifyIons :: (Ord a, Show (QFlipTyp qtyp), QFlipTyp (QFlipTyp qtyp) ~ qtyp) => QueryRep qtyp (Ion a) -> QueryRep qtyp (Ion a)
simplifyIons (QOp as cs)
    | nullified = QOp S.empty S.empty
    | S.null as && S.null cs' = QOp S.empty (S.singleton $ QOp S.empty S.empty)  -- for (*) above
    | otherwise = qop as cs'
  where
    cs' = S.filter (not . isEmptyQR) $ S.map simplifyIons cs  -- simplify sub formulas

    go acc (a:as') | qNot a `S.member` acc = True        -- check for opposite polarity atoms in this formula
                  | otherwise = go (S.insert a acc) as'
    go _ [] = False

    nullified = go S.empty (S.toList as) || any isConstQR (S.toList cs')  -- isConstQR detects whether a formula is 0-ary
simplifyIons x = x


--simpleTest = orq (qAtom "a") (qAtom "b") `andq` orq (qAtom "a") (qAtom "c")
--simpleTest1 = orq (qNot $ qAtom "a") (qAtom "b") `andq` orq (qAtom "a") (qAtom "c")

maximumByNote :: String -> (a -> a -> Ordering) -> [a] -> a
maximumByNote err _ [] = error $ "maximumByNote: " ++ err
maximumByNote _   f xs = maximumBy f xs
