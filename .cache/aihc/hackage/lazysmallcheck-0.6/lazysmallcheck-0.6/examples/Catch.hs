module Catch where

-- A property of Catch by Neil Mitchell

import Data.List
import Data.Maybe


-- Property

data Prop a = Or [Prop a] | And [Prop a] | Lit a

andP = And
orP = Or
lit = Lit
true = And []


-- Constraints

data Sat a = Sat a Constraint

substP ::  Eq alpha => [(alpha,beta)] -> Prop (Sat alpha) -> Prop (Sat beta)
substP xs (Lit (Sat i k)) = Lit $ Sat (fromJust $ lookup i xs) k
substP xs (And p) = And $ map (substP xs) p
substP xs (Or p) = Or $ map (substP xs) p


-- MP constraints

type Constraint  =  [Val]
data Val         =  [Pattern] :* [Pattern] |  Any deriving (Show,Eq)
data Pattern     =  Pattern CtorName [Val] deriving (Show,Eq)


(<|) :: CtorName -> Constraint -> Prop (Sat Int)
c <| vs = orP (map f vs)
    where
    (rec,non) = partition (isRec . (,) c) [0..arity c-1]

    f Any = true
    f (ms_1 :* ms_2) = orP  [ andP $ map lit $ g vs_1
                            | Pattern c_1 vs_1 <- ms_1, c_1 == c]
        where g vs =  zipWith Sat non (map (:[]) vs) ++
                      map (`Sat` [ms_2 :* ms_2]) rec

mergeVal :: Val -> Val -> Val
(a_1 :* b_1)  `mergeVal`  (a_2 :* b_2)  = merge a_1 a_2 :* merge b_1 b_2
x             `mergeVal`  y             = if x == Any then y else x

merge :: [Pattern] -> [Pattern] -> [Pattern]
merge  ms_1 ms_2 = [Pattern c_1 (zipWith mergeVal vs_1 vs_2) |
       Pattern c_1 vs_1 <- ms_1, Pattern c_2 vs_2 <- ms_2, c_1 == c_2]

validConstraint = all validVal
validVal Any = True
validVal (ms1 :* ms2) = validPatterns ms1 && validPatterns ms2
validPatterns = all validPattern
validPattern (Pattern c xs) = (fields c == length xs) && all validVal xs


-- Evaluator

data Value  =  Value CtorName [Value]
            |  Bottom
               deriving (Eq,Show)

sat :: Sat Value -> Bool
sat (Sat Bottom        k) = True
sat (Sat (Value c xs)  k) = sat' $ substP (zip [0..] xs) (c <| k)

sat' :: Prop (Sat Value) -> Bool
sat' (And xs) = all sat' xs
sat' (Or xs) = any sat' xs
sat' (Lit x) = sat x


-- Core language

data CtorName = Ctor | CtorN | CtorR | CtorNR
                deriving (Show,Eq)

arity Ctor = 0
arity CtorN = 1
arity CtorR = 1
arity CtorNR = 2

fields Ctor = 0
fields CtorN = 1
fields CtorR = 0
fields CtorNR = 1

isRec (CtorR,  0) = True
isRec (CtorNR, 1) = True
isRec _ = False

validValue :: Value -> Bool
validValue Bottom = True
validValue (Value c xs) = (arity c == length xs) && all validValue xs


-- Properties

infixr 0 -->
False --> _ = True
True --> x = x

prop :: (Value, [Pattern], [Pattern]) -> Bool
prop (v,ms1,ms2) = (validValue v && validPatterns ms1 && validPatterns ms2 &&
                   sat (Sat v [ms :* ms])) --> sat (Sat v [ms1 :* ms2])
    where
        ms = merge ms1 ms2
