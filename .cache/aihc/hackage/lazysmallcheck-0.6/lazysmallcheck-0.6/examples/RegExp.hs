module RegExp where

(<==>) :: Bool -> Bool -> Bool
a <==> b = a == b

-- ---------------------

data Nat = Zer
         | Suc Nat
  deriving (Eq, Show)

sub :: Nat -> Nat -> Nat
sub x y =
 case y of
  Zer -> x
  Suc y' -> case x of
   Zer -> Zer
   Suc x' -> sub x' y'

data Sym = N0
         | N1 Sym
 deriving (Eq, Show)

data RE = Sym Sym
        | Or RE RE
        | Seq RE RE
        | And RE RE
        | Star RE
        | Empty
  deriving (Eq, Show)

accepts :: RE -> [Sym] -> Bool
accepts re ss =
 case re of
  Sym n -> case ss of
   [] -> False
   (n':ss') -> n == n' && null ss'
  Or re1 re2 -> accepts re1 ss || accepts re2 ss
  Seq re1 re2 -> seqSplit re1 re2 [] ss
  And re1 re2 -> accepts re1 ss && accepts re2 ss
  Star re' -> case ss of
   [] -> True
   (s:ss') -> seqSplit re' re (s:[]) ss'
    -- accepts Empty ss || accepts (Seq re' re) ss
  Empty -> null ss

seqSplit :: RE -> RE -> [Sym] -> [Sym] -> Bool
seqSplit re1 re2 ss2 ss =
 seqSplit'' re1 re2 ss2 ss || seqSplit' re1 re2 ss2 ss

seqSplit'' :: RE -> RE -> [Sym] -> [Sym] -> Bool
seqSplit'' re1 re2 ss2 ss = accepts re1 ss2 && accepts re2 ss

seqSplit' :: RE -> RE -> [Sym] -> [Sym] -> Bool
seqSplit' re1 re2 ss2 ss =
 case ss of
  [] -> False
  (n:ss') ->
   seqSplit re1 re2 (ss2 ++ [n]) ss'

rep :: Nat -> RE -> RE
rep n re =
 case n of
  Zer -> Empty
  Suc n' -> Seq re (rep n' re)

repMax :: Nat -> RE -> RE
repMax n re =
 case n of
  Zer -> Empty
  Suc n' -> Or (rep n re) (repMax n' re)

repInt' :: Nat -> Nat -> RE -> RE
repInt' n k re =
 case k of
  Zer -> rep n re
  Suc k' -> Or (rep n re) (repInt' (Suc n) k' re)

repInt :: Nat -> Nat -> RE -> RE
repInt n k re = repInt' n (sub k n) re

-- Properties

prop_regex :: (Nat, Nat, RE, RE, [Sym]) -> Bool
prop_regex (n, k, p, q, s) = r
  where
    r = (accepts (repInt n k (And p q)) s)
          <==> (accepts (And (repInt n k p) (repInt n k q)) s)
  --(accepts (And (repInt n k p) (repInt n k q)) s) <==> (accepts (repInt n k (And p q)) s)^M

a_sol = (Zer, Suc (Suc Zer), Sym N0, Seq (Sym N0) (Sym N0), [N0, N0])
