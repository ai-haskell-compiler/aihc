{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Use ==" #-}
{-# HLINT ignore "Use /=" #-}
{-# HLINT ignore "Use max" #-}
{-# HLINT ignore "Use min" #-}

module GHC.Classes
  ( CTuple0,
    CTuple2,
    CTuple3,
    CTuple4,
    CTuple5,
    CTuple6,
    CTuple7,
    CTuple8,
    CTuple9,
    CTuple10,
    CTuple11,
    CTuple12,
    CTuple13,
    CTuple14,
    CTuple15,
    CTuple16,
    CTuple17,
    CTuple18,
    CTuple19,
    CTuple20,
    CTuple21,
    CTuple22,
    CTuple23,
    CTuple24,
    CTuple25,
    CTuple26,
    CTuple27,
    CTuple28,
    CTuple29,
    CTuple30,
    CTuple31,
    CTuple32,
    CTuple33,
    CTuple34,
    CTuple35,
    CTuple36,
    CTuple37,
    CTuple38,
    CTuple39,
    CTuple40,
    CTuple41,
    CTuple42,
    CTuple43,
    CTuple44,
    CTuple45,
    CTuple46,
    CTuple47,
    CTuple48,
    CTuple49,
    CTuple50,
    CTuple51,
    CTuple52,
    CTuple53,
    CTuple54,
    CTuple55,
    CTuple56,
    CTuple57,
    CTuple58,
    CTuple59,
    CTuple60,
    CTuple61,
    CTuple62,
    CTuple63,
    CTuple64,
    Eq (..),
    Ord (..),
    (&&),
    (||),
    not,
  )
where

import GHC.Types (Bool (..), Constraint, Ordering (..))

-- | The empty constraint tuple: what @()@ means at kind @Constraint@,
-- as opposed to the unit type @()@ at kind @Type@. A type family whose
-- result kind is @Constraint@ reduces to it when it has nothing to
-- demand, as @Assert 'True _@ does. GHC wires this class in; here it is
-- an ordinary class with no methods and one instance, so that solving it
-- and giving it an empty dictionary need no special cases.
class CTuple0

instance CTuple0

-- | The constraint tuples of two to 64 components: what @(c1, c2)@
-- means at kind @Constraint@, as the right-hand side of a constraint
-- synonym. GHC gives each the components as superclasses and one
-- instance. The type checker here splits a tuple into its components
-- wherever it becomes a predicate, so these classes need neither, and
-- no constraint on them is ever solved.
{- ORMOLU_DISABLE -}
type CTuple2 :: Constraint -> Constraint -> Constraint
class CTuple2 c1 c2

type CTuple3 :: Constraint -> Constraint -> Constraint -> Constraint
class CTuple3 c1 c2 c3

type CTuple4 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple4 c1 c2 c3 c4

type CTuple5 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple5 c1 c2 c3 c4 c5

type CTuple6 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple6 c1 c2 c3 c4 c5 c6

type CTuple7 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple7 c1 c2 c3 c4 c5 c6 c7

type CTuple8 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple8 c1 c2 c3 c4 c5 c6 c7 c8

type CTuple9 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple9 c1 c2 c3 c4 c5 c6 c7 c8 c9

type CTuple10 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple10 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10

type CTuple11 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple11 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11

type CTuple12 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple12 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12

type CTuple13 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple13 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13

type CTuple14 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple14 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14

type CTuple15 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple15 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15

type CTuple16 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple16 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16

type CTuple17 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple17 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17

type CTuple18 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple18 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18

type CTuple19 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple19 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19

type CTuple20 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple20 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20

type CTuple21 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple21 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21

type CTuple22 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple22 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22

type CTuple23 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple23 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23

type CTuple24 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple24 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24

type CTuple25 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple25 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25

type CTuple26 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple26 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26

type CTuple27 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple27 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27

type CTuple28 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple28 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28

type CTuple29 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple29 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29

type CTuple30 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple30 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30

type CTuple31 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple31 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31

type CTuple32 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple32 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32

type CTuple33 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple33 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33

type CTuple34 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple34 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34

type CTuple35 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple35 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35

type CTuple36 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple36 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36

type CTuple37 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple37 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37

type CTuple38 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple38 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38

type CTuple39 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple39 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39

type CTuple40 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple40 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40

type CTuple41 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple41 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41

type CTuple42 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple42 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42

type CTuple43 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple43 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43

type CTuple44 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple44 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44

type CTuple45 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple45 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45

type CTuple46 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple46 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46

type CTuple47 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple47 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47

type CTuple48 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple48 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48

type CTuple49 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple49 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49

type CTuple50 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple50 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50

type CTuple51 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple51 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51

type CTuple52 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple52 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52

type CTuple53 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple53 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53

type CTuple54 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple54 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54

type CTuple55 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple55 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54 c55

type CTuple56 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple56 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54 c55 c56

type CTuple57 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple57 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54 c55 c56 c57

type CTuple58 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple58 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54 c55 c56 c57 c58

type CTuple59 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple59 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54 c55 c56 c57 c58 c59

type CTuple60 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple60 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54 c55 c56 c57 c58 c59 c60

type CTuple61 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple61 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54 c55 c56 c57 c58 c59 c60 c61

type CTuple62 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple62 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54 c55 c56 c57 c58 c59 c60 c61 c62

type CTuple63 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple63 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54 c55 c56 c57 c58 c59 c60 c61 c62 c63

type CTuple64 :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
class CTuple64 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54 c55 c56 c57 c58 c59 c60 c61 c62 c63 c64

{- ORMOLU_ENABLE -}

class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  left == right = not (left /= right)
  left /= right = not (left == right)

infix 4 ==, /=

class (Eq a) => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
  compare left right =
    if left == right
      then EQ
      else
        if left <= right
          then LT
          else GT
  left < right =
    case compare left right of
      LT -> True
      _ -> False
  left <= right =
    case compare left right of
      GT -> False
      _ -> True
  left > right =
    case compare left right of
      GT -> True
      _ -> False
  left >= right =
    case compare left right of
      LT -> False
      _ -> True
  max left right = if left <= right then right else left
  min left right = if left <= right then left else right

infix 4 <, <=, >, >=

infixr 3 &&

-- | Boolean conjunction, lazy in its second argument.
(&&) :: Bool -> Bool -> Bool
False && _ = False
True && right = right

infixr 2 ||

-- | Boolean disjunction, lazy in its second argument.
(||) :: Bool -> Bool -> Bool
False || right = right
True || _ = True

-- | Boolean negation.
not :: Bool -> Bool
not False = True
not True = False
