{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main where

import Data.Set (Set)
import qualified Data.Set as S
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Bimap (Bimap)
import qualified Data.Bimap as B

import Data.String
import Text.RawString.QQ     (r)

import Data.List (sortOn, subsequences)
import Data.Monoid
import Data.Foldable
import Data.Maybe
import Data.Ord
import Data.Function
import Data.Traversable

import Prettyprinter hiding ((<>), group)

import Prelude hiding (foldl, foldl1, maximum, mapM_, minimum, sum, concat)

data Sign = Minus | Plus
  deriving (Eq, Ord, Show)

data Form = Form String (Set (Sign, String))
  deriving (Eq, Ord, Show)

type Factor = Int
type Expr   = IntSet
data Forms = Forms (Bimap String Int) (IntMap Expr)
  deriving (Eq, Show)

instance IsString Forms where
  fromString = foldl addForm (Forms B.empty mempty) . map parseForm . filter (not . null) . lines

parseForm :: String -> Form
parseForm xs = case words xs of
  (lhs : "=" : ys) -> Form lhs (f ys) where
    f [] = mempty
    f ("+":z:zs) = S.singleton (Plus, z) <> f zs
    f ("-":z:zs) = S.singleton (Minus, z) <> f zs
    f (z:zs) = S.singleton (Plus, z) <> f zs
  as -> error (show as)

addForm :: Forms -> Form -> Forms
addForm (Forms ids exprs) (Form lhs rhs) =
  Forms (B.insert lhs lhsId ids') (IM.insert lhsId rhsExpr exprs) where
    (ids', rhsExpr) = foldl appendVar (ids, mempty) rhs
    lhsId = nextId ids'

appendVar :: (Bimap String Int, Expr) -> (Sign, String) -> (Bimap String Int, Expr)
appendVar (ids, expr) (sign, str) = (ids', expr') where
  maybeId = B.lookup str ids
  (ids', newId) = case maybeId of
    Nothing -> (B.insert str t ids, t) where
      t = nextId ids
    Just i  -> (ids, i)
  expr' = IS.insert (if sign==Plus then newId else negate newId) expr

nextId :: (Ord a, Num b, Enum b) => Bimap a b -> b
nextId bm
  | B.null bm = 1
  | otherwise = succ . fst . B.findMaxR $ bm

instance Pretty Forms where
  pretty (Forms ids exprs) = vsep (IM.elems $ IM.mapWithKey prettyExpr exprs) where
    toStr i = fromJust $ B.lookupR i ids
    prettyExpr lhsId expr = pretty (toStr lhsId) <+> equals <+> hsep (map prettyVar (IS.toDescList expr))
    prettyVar n = pretty (if n < 0 then '-' else '+') <+> pretty (toStr (abs n))


flipSign :: Expr -> Expr
flipSign = IS.map negate

nonTrivial :: [a] -> Bool
nonTrivial []  = False
nonTrivial [_] = False
nonTrivial _   = True

-- | Input must be in ascending order
absExpr :: (Num a, Ord a) => [a] -> [a]
absExpr [] = []
absExpr [x] = [x]
absExpr xxs@(x : xs) = if negate x < last xs then xxs else map negate xxs

nub :: Ord a => [a] -> [a]
nub = toList . S.fromList

suspicious :: Forms -> [IntSet]
suspicious fs@(Forms _ exprs) = take 8 $ sortOn (substitutionWeight fs) subexprs
  where
    subexprs :: [Expr]
    subexprs = toList $ fold $ snd $ mapAccumL doExpr mempty (toList exprs)

    doExpr :: [IntSet] -> Expr -> ([IntSet], Set Expr)
    doExpr acc expr = (expr : acc
      , S.fromList
      $ map (IS.fromList . absExpr)
      $ filter nonTrivial
      $ concatMap (subsequences . IS.toList)
      $ filter ((> 1) . IS.size)
      $ map (IS.intersection expr) acc ++ map (IS.intersection (flipSign expr)) acc
      )

substitutionWeight:: Forms -> Expr -> Int
substitutionWeight (Forms _ exprs) newExpr = size * count where
  newExprNeg = flipSign newExpr
  size = IS.size newExpr - 1
  count = 1 - IM.size (IM.filter (\expr -> newExpr    `IS.isSubsetOf` expr || newExprNeg `IS.isSubsetOf` expr) exprs)

substitute :: Char -> Forms -> Expr -> Forms
substitute ch (Forms ids exprs) newExpr = Forms ids' exprs' where
  lhsId  = nextId ids
  lhs    = ch : show lhsId
  ids'   = B.insert lhs lhsId ids
  exprs' = IM.insert lhsId newExpr (IM.map eliminate exprs)

  newExprNeg = flipSign newExpr
  eliminate expr
    | newExpr    `IS.isSubsetOf` expr = IS.insert lhsId (expr `IS.difference` newExpr)
    | newExprNeg `IS.isSubsetOf` expr = IS.insert (-lhsId) (expr `IS.difference` newExprNeg)
    | otherwise                       = expr

weight :: Forms -> Int
weight (Forms _ exprs) = getSum $ foldMap (Sum . pred . IS.size) exprs

optimize :: Char -> Forms -> Forms
optimize ch fs = case suspicious fs of
  []   -> fs
  susp -> minimumBy (comparing weight) . map (optimize ch . substitute ch fs) $ susp

main :: IO ()
main = do
  let before =
        [ ('t', makarov1)
        , ('u', makarov2)
        , ('v', makarov3)
        , ('t', laderman1)
        , ('u', laderman2)
        , ('v', laderman3)
        ]
  let weightBefore = map (weight . snd) before
  let after = map (uncurry optimize) before
  let weightAfter = map weight after
  mapM_ (print . pretty) after
  putStrLn $ show weightBefore ++ " = " ++ show (sum weightBefore)
  putStrLn $ show weightAfter  ++ " = " ++ show (sum weightAfter)

makarov1 :: Forms
makarov1 = [r|
l1  = a3 + c1 - c2
l2  = a2 + b1 + b2
l3  = a2 + b1 + b3
l4  = a3 - c2 - c3
l5  = a1 - c1 + c2
l6  = a1 + b1 + b2
l7  = a1 + b1 + b3 + c2 + c3
l8  = a2
l9  = a3
l10 = b1
l11 = c2
l12 = c1 - c2
l13 = b1 + b2
l14 = a2 + b1
l15 = b2
l16 = a3 - c2
l17 = c2
l18 = b3 - c2 - c3
l19 = c1 + c3 - b1 - b3
l20 = b1 + b3
l21 = c2 + c3
l22 = c2 + c3 - b1 - b3
|]


makarov2 :: Forms
makarov2 = [r|
r1 =  k1 + k7 - k8 + k9
r2 =  k2 - k4 + k5 - k6
r3 =  k3 - k4 + k5 - k6
r4 =  k3 - k7 + k8 - k9
r5 =  k1
r6 =  k2
r7 =  k3
r8 =  k1 + k4 - k5 + k6
r9 =  k2 + k7 - k8 + k9
r10 =  k4
r11 =  k7
r12 =  k1 + k7
r13 =  k4 - k2
r14 =  k4 - k5 + k6
r15 =  k6
r16 =  k7 - k8 + k9
r17 =  k8
r18 =  k6
r19 =  k8
r20 =  k4 - k3 + k6 + k8
r21 =  k3 + k6 - k7 + k8
r22 =  k6 + k8
|]

makarov3 :: Forms
makarov3 = [r|
c11 = m5 + m10 + m11 + m12
c12 = m8 + m10 - m14 + m17 - m18 + m19 - m22
c13 = m1 - m11 - m12 - m16 + m17 - m18 + m19 - m22
c21 = m6 - m10 + m11 + m13
c22 = m2 - m10 + m13 + m14 + m15 + m17
r23 = m9 - m11 + m15 - m16 + m17
c31 = m7 - m10 - m11 + m20 - m21 + m22
c32 = m3 - m10 + m14 - m17 + m18 + m20 + m22
c33 = m4 + m11 + m16 - m17 + m18 + m21
|]

laderman1 :: Forms
laderman1 = [r|
l1 = a11 + a12 + a13 - a21 - a22 - a32 - a33
l2 = a11 - a21
l3 = a22
l4 = - a11 + a21 + a22
l5 = a21 + a22
l6 = a11
l7 = - a11 + a31 + a32
l8 = - a11 + a31
l9 = a31 + a32
l10 = a11 + a12 + a13 - a22 - a23 - a31 - a32
l11 = a32
l12 = - a13 + a32 + a33
l13 = a13 - a33
l14 = a13
l15 = a32 + a33
l16 = - a13 + a22 + a23
l17 = a13 - a23
l18 = a22 + a23
l19 = a12
l20 = a23
l21 = a21
l22 = a31
l23 = a33
|]

laderman2 :: Forms
laderman2 = [r|
r1 = b22
r2 = - b12 + b22
r3 = - b11 + b12 + b21 - b22 - b23 - b31 + b33
r4 = b11 - b12 + b22
r5 = - b11 + b12
r6 = b11
r7 = b11 - b13 + b23
r8 = b13 - b23
r9 = - b11 + b13
r10 = b23
r11 = - b11 + b13 + b21 - b22 - b23 - b31 + b32
r12 = b22 + b31 - b32
r13 = b22 - b32
r14 = b31
r15 = - b31 + b32
r16 = b23 + b31 - b33
r17 = b23 - b33
r18 = - b31 + b33
r19 = b21
r20 = b32
r21 = b13
r22 = b12
r23 = b33
|]

laderman3 :: Forms
laderman3 = [r|
c11 = m6 + m14 + m19
c12 = m1 + m4 + m5 + m6 + m12 + m14 + m15
c13 = m6 + m7 + m9 + m10 + m14 + m16 + m18
c21 = m2 + m3 + m4 + m6 + m14 + m16 + m17
c22 = m2 + m4 + m5 + m6 + m20
c23 = m14 + m16 + m17 + m18 + m21
c31 = m6 + m7 + m8 + m11 + m12 + m13 + m14
c32 = m12 + m13 + m14 + m15 + m22
c33 = m6 + m7 + m8 + m9 + m23
|]




