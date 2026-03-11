{-# LANGUAGE
    BangPatterns,
    GADTs,
    ScopedTypeVariables,
    TypeOperators #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Pretty printing of function representations.
--
-- === Warning
--
-- This is an internal module: it is not subject to any versioning policy,
-- breaking changes can happen at any time.
-- It is made available only for debugging.
-- Otherwise, use "Test.Fun".
--
-- If something here seems useful, please open an issue to export it from an
-- external module.

module Test.Fun.Internal.Pretty where

import Data.Bits (setBit)
import Control.Applicative (liftA2)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Semigroup (Semigroup((<>)))

import Test.Fun.Internal.Types
  ((:->)(..), Branches(..), Fields(..), Bin(..), ConName, FunName, TypeName, Concrete(..), ShowsPrec)

-- * Interface

-- | Prettify function representation.
showsPrecFun :: forall a r. ShowsPrec r -> ShowsPrec (a :-> r)
showsPrecFun showsPrec_ _ h = s 0 where
  s = sparens 0 ("\\" ~% sVar defVar % " -> " ~% unExpr_ (tFun (tShow_ showsPrec_) h defCtx))

-- | Break up lines after braces and indent.
--
-- === __Example__
--
-- Input:
--
-- > \x -> case x :: Either _ _ of { Left x1 -> case x1 of { Left x2 -> () ; Right x2 -> case x2 of {} } ; Right x1 -> () }
--
-- Output:
--
-- > \x -> case x :: Either _ _ of {
-- >   Left x1 -> case x1 of {
-- >     Left x2 -> () ;
-- >     Right x2 -> case x2 of {} } ;
-- >   Right x1 -> () }
indent :: String -> String
indent = go 0 where
  go !n ('{' : '}' : xs) = '{' : '}' : go n xs
  go n ('{' : xs) = '{' : '\n' : replicate (n + 2) ' ' ++ go (n + 2) (dropWhile (== ' ') xs)
  go n (';' : xs) = ';' : '\n' : replicate n ' ' ++ go n (dropWhile (== ' ') xs)
  go n ('}' : xs) = '}' : go (n - 2) xs
  go n (c : xs) = c : go n xs
  go _ [] = []

prettyFun :: forall a r. (r -> C Expr) -> (a :-> r) -> String
prettyFun prettyR h = unExpr_ (tFun prettyR h defCtx) ""

-- * Implementation

-- ** Strings

type DString = String -> String
type PrecDString = Int -> DString

sid :: DString
sid = id

sstring :: String -> DString
sstring = (++)

(%) :: DString -> DString -> DString
(%) = (.)

(~%) :: String -> DString -> DString
(~%) = (%) . sstring

infixr 1 %, ~%

sparens :: Int -> DString -> PrecDString
sparens d0 s d = if d > d0 then "(" ~% s % ")" ~% sid else s

-- ** Pretty-printed expressions

newtype Expr = Expr { unExpr :: PrecDString }

type Pattern = Expr

unExpr_ :: Expr -> DString
unExpr_ e = unExpr e 0

data Var = Var String !Int

-- Context mapping variables to expressions.
data Ctx = (Var, Expr) :. Ctx

infixr 1 :.

defVar :: Var
defVar = Var "a" 0

defCtx :: Ctx
defCtx = addVar [defVar] badCtx

badCtx :: Ctx
badCtx = addVar [Var "unknown" 0] badCtx

-- | Type of values under some context
type C a = Ctx -> a

-- ** Basic expression constructors

-- Naming convention:
-- - s for strings,
-- - e for expressions (Expr),
-- - t for terms (C Expr, expression under some context).

eWild :: Pattern
eWild = Expr (\_ -> sstring "_")

eConst :: String -> Expr
eConst s = Expr (\_ -> sstring s)

tConst :: String -> C Expr
tConst s _ = eConst s

eInt :: Integer -> Expr
eInt n = Expr (\_ -> show n ~% sid)

eApp :: Expr -> Expr -> Expr
eApp f x = Expr (sparens 10 (unExpr f 10 % " " ~% unExpr x 11))

tShow :: Show a => a -> C Expr
tShow = tShow_ showsPrec

tShow_ :: ShowsPrec a -> a -> C Expr
tShow_ showsPrec_ a _ = Expr (flip showsPrec_ a)

sVar :: Var -> DString
sVar (Var s i) = s ~% show i ~% sid

eVar :: Var -> Expr
eVar v = Expr (\_ -> sVar v)

addVar :: [Var] -> Ctx -> Ctx
addVar [] vs = vs
addVar (v : ps) vs = (v, eVar v) :. addVar ps vs

-- ** Main implementation

-- | Pretty-print a function representation.
tFun :: forall a r. (r -> C Expr) -> (a :-> r) -> C Expr
tFun prettyR = go where
  go :: forall b. (b :-> r) -> C Expr
  go (Absurd _) = tAbsurd
  go (Const r) = \(_ :. vs) -> prettyR r vs
  go (CoApply c a _ h) = tCoApply c a (tFun go h)
  go (Apply fn _ h) = tApply fn (go h)
  go (Case tn _ b r) = tCase tn (appendIf (partialBranches b) (tBranches prettyR b) (bWild (prettyR r)))
  go (CaseInteger tn _ b r) = tCase tn (tBin prettyR b <> bEllipsis b <> bWild (prettyR r))
  go (ToShrink _) = tConst "[...]"

tApply :: FunName -> C Expr -> C Expr
tApply f y ((v, t) :. vs) =
  y ((v, eApp (eConst f) t) :. vs)

tCoApply :: Concrete w -> w -> C Expr -> C Expr
tCoApply c a y ((v, t) :. vs) =
  y ((nextVar v, eApp t (eConst (showsPrecC c 11 a ""))) :. (v, t) :. vs)

tAbsurd :: C Expr
tAbsurd ((_, t) :. _) = Expr (\_ -> "case " ~% unExpr_ t % " of {}" ~% sid)

appendIf :: Semigroup m => Bool -> m -> m -> m
appendIf True = (<>)
appendIf False = const

-- | @True@ if there is a @Fail@ branch.
partialBranches :: Branches x r -> Bool
partialBranches Fail = True
partialBranches (Pat _ _) = False
partialBranches (Alt b1 b2) = partialBranches b1 || partialBranches b2

-- The patterns are parameterized by a fresh variable.
type CBranches = Var -> C [EBranch]

data EBranch
  = EBranch Pattern Expr
  | EBranchEllipsis

bEllipsis :: Bin r -> CBranches
bEllipsis b _ _
  | ellidedBin b = [EBranchEllipsis]
  | otherwise = []

bWild :: C Expr -> CBranches
bWild e _ vs = [EBranch eWild (e vs)]

tCase :: TypeName -> CBranches -> C Expr
tCase tn bs ((v, t) :. vs) = Expr (\_ ->
    "case " ~% unExpr_ t % " :: " ~% tn ~% " of { "
      ~% sBranches (bs v vs)
      % " }" ~% sid)
  where
    renderBranch (EBranch p e) = unExpr_ p % " -> " ~% unExpr_ e
    renderBranch EBranchEllipsis = "[...]" ~% sid
    sBranches [] = sid
    sBranches (b0 : bs_) =
      renderBranch b0 %
      foldr (\b ebs -> " ; " ~% renderBranch b % ebs) sid bs_

tBranches :: forall x r. (r -> C Expr) -> Branches x r -> CBranches
tBranches prettyR = go where
  go :: forall y. Branches y r -> CBranches
  go Fail = \_ _ -> []
  go (Alt b1 b2) = (liftA2 . liftA2) (++) (go b1) (go b2)
  go (Pat cn d) = tFields prettyR d cn []

tFields :: forall x r. (r -> C Expr) -> Fields x r -> ConName -> [Var] -> CBranches
tFields prettyR = go where
  go :: forall y. Fields y r -> ConName -> [Var] -> CBranches
  go (NoField h) cn ps _ vs = [EBranch (mkPattern cn ps) (prettyR h (addVar ps vs))]
  go (Field d) cn ps v vs = tFields (tFun prettyR) d cn (v' : ps) v' vs where
    v' = nextVar v

nextVar :: Var -> Var
nextVar (Var s i) = Var s (i + 1)

mkPattern :: ConName -> [Var] -> Pattern
mkPattern cn vs = Expr (\_ -> cn ~% foldr (\v s -> " " ~% sVar v % s) sid vs)

tBin :: (r -> C Expr) -> Bin r -> CBranches
tBin prettyR b _ vs =
  fmap (\(n, e) -> EBranch (eInt n) e)
    (sortBy (comparing fst) (tBin' prettyR b vs))

data Sign = Pos | Neg

resign :: Sign -> Integer -> Integer
resign Pos x = x
resign Neg x = - x

tBin' :: (r -> C Expr) -> Bin r -> C [(Integer, Expr)]
tBin' prettyR b vs = go_ b where
  go_ (BinToShrink _) = []
  go_ BinEmpty = []
  go_ (BinAlt r_ b0 b1) = tr ++ tb01 where
    tr = case r_ of
      Nothing -> []
      Just r -> [(0, prettyR r vs)]
    tb01 = (go Pos 0 0 b0 . go Neg 0 0 b1) []

  go _ !_ !_ (BinToShrink _) k = k
  go _ _ _ BinEmpty k = k
  go z i n (BinAlt r_ b0 b1) k = tr ++ tb01 where
    i' = i + 1
    n' = setBit n i
    tr = case r_ of
      Nothing -> []
      Just r -> [(resign z n', prettyR r vs)]
    tb01 = (go z i' n b0 . go z i' n' b1) k

ellidedBin :: Bin r -> Bool
ellidedBin (BinToShrink _) = True
ellidedBin BinEmpty = False
ellidedBin (BinAlt _ b0 b1) = ellidedBin b0 && ellidedBin b1
