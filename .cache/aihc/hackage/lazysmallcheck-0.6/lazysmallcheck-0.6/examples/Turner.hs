module Turner where

-- Turner's abstraction algorithm as defined by Simon PJ
-- (with properties added)

infixl 9 :@

data Var = V0 | V1
  deriving (Show, Eq)

data Exp = Exp :@ Exp | L Var Exp | V Var | F Comb
  deriving (Show, Eq)

data Comb = I | K | B | C | S | C' | B' | S'
  deriving (Show, Eq)

compile (f :@ x) = compile f :@ compile x
compile (L v e) = abstr v (compile e)
compile e = e

abstr v (f :@ x) = opt (F S :@ abstr v f :@ abstr v x)
abstr v (V w) | v == w = F I
abstr v e = F K :@ e

opt (F S :@ (F K :@ p) :@ (F K :@ q)) = F K :@ (p :@ q)
opt (F S :@ (F K :@ p) :@ F I) = p
opt (F S :@ (F K :@ p) :@ (F B :@ q :@ r)) = F B' :@ p :@ q :@ r
opt (F S :@ (F K :@ p) :@ q) = F B :@ p :@ q
opt (F S :@ (F B :@ p :@ q) :@ (F K :@ r)) = F C' :@ p :@ q :@ r
opt (F S :@ p :@ (F K :@ q)) = F C :@ p :@ q
opt (F S :@ (F B :@ p :@ q) :@ r) = F S' :@ p :@ q :@ r
opt e = e

-- Combinator reduction

simp (F I :@ a) = Just a
simp (F K :@ a :@ b) = Just a
simp (F S :@ f :@ g :@ x) = Just $ f :@ x :@ (g :@ x)
simp (F B :@ f :@ g :@ x) = Just $ f :@ (g :@ x)
simp (F C :@ f :@ g :@ x) = Just $ f :@ x :@ g
simp (F B' :@ k :@ f :@ g :@ x) = Just $ k :@ (f :@ (g :@ x))
simp (F C' :@ k :@ f :@ g :@ x) = Just $ k :@ (f :@ x) :@ g
simp (F S' :@ k :@ f :@ g :@ x) = Just $ k :@ (f :@ x) :@ (g :@ x)
simp e = Nothing

simplify e =
  case simp e of
    Nothing -> case e of
                 f :@ g -> simplify f :@ simplify g
                 _ -> e
    Just e' -> simplify e'

-- Properties

infixr 0 -->
False --> _ = True
True --> x = x

prop_abstr (v, e) = simplify (abstr v e :@ V v) == e
