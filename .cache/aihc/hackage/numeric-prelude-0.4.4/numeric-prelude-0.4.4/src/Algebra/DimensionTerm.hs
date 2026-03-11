{- |
We already have the dynamically checked physical units
provided by "Number.Physical"
and the statically checked ones of the @dimensional@ package of Buckwalter,
which require multi-parameter type classes with functional dependencies.

Here we provide a poor man's approach:
The units are presented by type terms.
There is no canonical form and thus the type checker
can not automatically check for equal units.
However, if two unit terms represent the same unit,
then you can tell the type checker to rewrite one into the other.

You can add more dimensions by introducing more types of class 'C'.

This approach is not entirely safe
because you can write your own flawed rewrite rules.
It is however more safe than with no units at all.
-}

module Algebra.DimensionTerm where

import Prelude hiding (recip)


{- Haddock does not like 'where' clauses before empty declarations -}
class Show a => C a -- where


noValue :: C a => a
noValue =
   let x = error ("there is no value of type " ++ show x)
   in  x

{- * Type constructors -}

data Scalar  = Scalar
data Mul a b = Mul
data Recip a = Recip
type Sqr   a = Mul a a

appPrec :: Int
appPrec = 10

instance Show Scalar where
   show _ = "scalar"

instance (Show a, Show b) => Show (Mul a b) where
   showsPrec p x =
      let disect :: Mul a b -> (a,b)
          disect _ = undefined
          (y,z) = disect x
      in  showParen (p >= appPrec)
            (showString "mul " . showsPrec appPrec y .
             showString " " . showsPrec appPrec z)

instance (Show a) => Show (Recip a) where
   showsPrec p x =
      let disect :: Recip a -> a
          disect _ = undefined
      in  showParen (p >= appPrec)
            (showString "recip " . showsPrec appPrec (disect x))


instance C Scalar -- where

instance (C a, C b) => C (Mul a b) -- where

instance (C a) => C (Recip a) -- where


scalar :: Scalar
scalar = noValue

mul :: (C a, C b) => a -> b -> Mul a b
mul _ _ = noValue

recip :: (C a) => a -> Recip a
recip _ = noValue


infixl 7 %*%
infixl 7 %/%

(%*%) :: (C a, C b) => a -> b -> Mul a b
(%*%) = mul

(%/%) :: (C a, C b) => a -> b -> Mul a (Recip b)
(%/%) x y = mul x (recip y)


{- * Rewrites -}

applyLeftMul :: (C u0, C u1, C v) => (u0 -> u1) -> Mul u0 v -> Mul u1 v
applyLeftMul _ _ = noValue
applyRightMul :: (C u0, C u1, C v) => (u0 -> u1) -> Mul v u0 -> Mul v u1
applyRightMul _ _ = noValue
applyRecip :: (C u0, C u1) => (u0 -> u1) -> Recip u0 -> Recip u1
applyRecip _ _ = noValue

commute :: (C u0, C u1) => Mul u0 u1 -> Mul u1 u0
commute _ = noValue
associateLeft :: (C u0, C u1, C u2) => Mul u0 (Mul u1 u2) -> Mul (Mul u0 u1) u2
associateLeft _ = noValue
associateRight :: (C u0, C u1, C u2) => Mul (Mul u0 u1) u2 -> Mul u0 (Mul u1 u2)
associateRight _ = noValue
recipMul :: (C u0, C u1) => Recip (Mul u0 u1) -> Mul (Recip u0) (Recip u1)
recipMul _ = noValue
mulRecip :: (C u0, C u1) => Mul (Recip u0) (Recip u1) -> Recip (Mul u0 u1)
mulRecip _ = noValue

identityLeft :: C u => Mul Scalar u -> u
identityLeft _ = noValue
identityRight :: C u => Mul u Scalar -> u
identityRight _ = noValue
cancelLeft :: C u => Mul (Recip u) u -> Scalar
cancelLeft _ = noValue
cancelRight :: C u => Mul u (Recip u) -> Scalar
cancelRight _ = noValue
invertRecip :: C u => Recip (Recip u) -> u
invertRecip _ = noValue
doubleRecip :: C u => u -> Recip (Recip u)
doubleRecip _ = noValue
recipScalar :: Recip Scalar -> Scalar
recipScalar _ = noValue


{- * Example dimensions -}

{- ** Scalar -}

{- |
This class allows defining instances that are exclusively for 'Scalar' dimension.
You won't want to define instances by yourself.
-}
class C dim => IsScalar dim where
   toScalar :: dim -> Scalar
   fromScalar :: Scalar -> dim

instance IsScalar Scalar where
   toScalar = id
   fromScalar = id


{- ** Basis dimensions -}

data Length      = Length
data Time        = Time
data Mass        = Mass
data Charge      = Charge
data Angle       = Angle
data Temperature = Temperature
data Information = Information

length :: Length
length = noValue

time :: Time
time = noValue

mass :: Mass
mass = noValue

charge :: Charge
charge = noValue

angle :: Angle
angle = noValue

temperature :: Temperature
temperature = noValue

information :: Information
information = noValue


instance Show Length      where show _ = "length"
instance Show Time        where show _ = "time"
instance Show Mass        where show _ = "mass"
instance Show Charge      where show _ = "charge"
instance Show Angle       where show _ = "angle"
instance Show Temperature where show _ = "temperature"
instance Show Information where show _ = "information"

instance C Length      -- where
instance C Time        -- where
instance C Mass        -- where
instance C Charge      -- where
instance C Angle       -- where
instance C Temperature -- where
instance C Information -- where

{- ** Derived dimensions -}

type Frequency = Recip Time

frequency :: Frequency
frequency = noValue


data Voltage = Voltage

type VoltageAnalytical =
        Mul (Mul (Sqr Length) Mass) (Recip (Mul (Sqr Time) Charge))

voltage :: Voltage
voltage = noValue

instance Show Voltage where show _ = "voltage"

instance C Voltage -- where

unpackVoltage :: Voltage -> VoltageAnalytical
unpackVoltage _ = noValue

packVoltage :: VoltageAnalytical -> Voltage
packVoltage _ = noValue
