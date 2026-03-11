{-# LANGUAGE Trustworthy #-}
-- | Defunctorization core primitives.
module DeFun.Core where

import Data.Kind (Type)

-- $setup
-- >>> import Prelude (Show)
-- >>> import Data.SOP.NP (NP (..))
-- >>> import DeFun

-------------------------------------------------------------------------------
-- * FunKind
-------------------------------------------------------------------------------

-- | A kind for type-level functions. 
type FunKind :: Type -> Type -> Type
data FunKind :: Type -> Type -> Type

-------------------------------------------------------------------------------
-- * Fun
-------------------------------------------------------------------------------

-- | Something of kind @'Fun' a b@ (or @a '~>' b@) is a defunctionalized type function.
--
-- Normal type arrows @(->)@ can be converted into defunctionalized arrows @('~>')@ by use of 'Con1', 'Con2' ... family of types.
--
type Fun a b = FunKind a b -> Type

-- implementation note: defunctionalized symbols would be nicer
-- if defined as constructors of open data kind.
-- But GHC doesn't have such functionality yet:
-- https://gitlab.haskell.org/ghc/ghc/-/issues/11080

-- | An infix synonmy for 'Fun'.
type (~>) a b = Fun a b
infixr 0 ~>

-------------------------------------------------------------------------------
-- * App
-------------------------------------------------------------------------------

-- | Type level function application.
type family App (f :: a ~> b) (x :: a) :: b

-- | An infix synonym for 'App'.
--
-- Note: there is a term version which is a synonym to 'appLam'.
type (@@) a b = App a b
infixl 9 @@

-------------------------------------------------------------------------------
-- * Lam
-------------------------------------------------------------------------------

-- | A term-level representation of defunctionalized type function.
--
-- If the @a@ and @b@ type arguments are singletons,
-- then @'Lam' a b@ itself will be a singleton of the defunctionalized type function,
-- but in general it may not be.
-- (c.f @'Data.SOP.NP.NP' Sing@ is a list singleton, but @NP@ is more general).
--
type Lam :: (a -> Type) -> (b -> Type) -> (a ~> b) -> Type
newtype Lam a b f = Lam { appLam :: LamRep a b f }

-- | An infix synonym for 'Lam'
type a :~> b = Lam a b
infixr 0 :~>

-- | A constructor of 'Lam'
mkLam :: LamRep a b f -> Lam a b f
mkLam = Lam

-- | Unwrapped representation of defunctionalized type function.
type LamRep :: (a -> Type) -> (b -> Type) -> (a ~> b) -> Type
type LamRep a b fun = forall x. a x -> b (fun @@ x)

-- | An infix synonym for 'appLam'.
--
-- Note: there is a type version which is a synonym to 'App'.
(@@) :: Lam a b f -> a x -> b (f @@ x)
f @@ x = appLam f x

-- | A term-level representation of binary defunctionalized type function.
type Lam2 a b c fun = Lam a (b :~> c) fun

-- | A term-level representation of ternary defunctionalized type function.
type Lam3 a b c d fun = Lam a (b :~> c :~> d) fun

-- | Unwrapped representation of binary defunctionalized type function.
type LamRep2  :: (a -> Type) -> (b -> Type) -> (c -> Type) -> (a ~> b ~> c) -> Type
type LamRep2 a b c fun = forall x y. a x -> b y -> c (fun @@ x @@ y)

-- | Unwrapped representation of ternary defunctionalized type function.
type LamRep3  :: (a -> Type) -> (b -> Type) -> (c -> Type) -> (d -> Type) -> (a ~> b ~> c ~> d) -> Type
type LamRep3 a b c d fun = forall x y z. a x -> b y -> c z -> d (fun @@ x @@ y @@ z)

-- | 'Lam2' explicitly bidirectional pattern synonyms for binary defunctionalized type function.
pattern Lam2 :: LamRep2 a b c fun -> Lam2 a b c fun
pattern Lam2 f <- (appLam2 -> f)
  where Lam2 f = mkLam2 f

-- | 'Lam3' explicitly bidirectional pattern synonyms for ternary defunctionalized type function.
pattern Lam3 :: LamRep3 a b c d fun -> Lam3 a b c d fun
pattern Lam3 f <- (appLam3 -> f)
  where Lam3 f = mkLam3 f

-- | Constructor of 'Lam2'
mkLam2 :: LamRep2 a b c fun -> Lam2 a b c fun
mkLam2 f = Lam (\x -> Lam (f x))

-- | Destructor of 'Lam2'
appLam2 :: Lam2 a b c fun -> LamRep2 a b c fun
appLam2 f x y = f @@ x @@ y

-- | Constructor of 'Lam3'
mkLam3 :: LamRep3 a b c d fun -> Lam3 a b c d fun
mkLam3 f = Lam (\x -> mkLam2 (f x))

-- | Destructor of 'Lam3'
appLam3 :: Lam3 a b c d fun -> LamRep3 a b c d fun
appLam3 f x y z = f @@ x @@ y @@ z

-------------------------------------------------------------------------------
-- * Con
-------------------------------------------------------------------------------

-- | Wrapper for converting the normal type-level arrow into a '~>'. For example, given
--
-- >>> data Nat = Z | S Nat
--
-- we can write
--
-- >>> :kind! Map (Con1 S) '[Z, S Z]
-- Map (Con1 S) '[Z, S Z] :: [Nat]
-- = [S Z, S (S Z)]
--
type Con1 :: (a -> b) -> a ~> b
data Con1 con arg
type instance App (Con1 f) x = f x

-- | Similar to 'Con1', but for two-parameter type constructors.
type Con2 :: (a -> b -> c) -> a ~> b ~> c
data Con2 con arg
type instance App (Con2 f) arg = Con1 (f arg)

-- | Similar to 'Con2', but for three-parameter type constructors.
type Con3 :: (a -> b -> c -> d) -> a ~> b ~> c ~> d
data Con3 con arg
type instance App (Con3 f) arg = Con2 (f arg)

-- | A term-level constructor for 'Lam' of 'Con1'. For example, given
--
-- >>> data Nat = Z | S Nat
-- >>> data SNat (n :: Nat) where { SZ :: SNat Z; SS :: SNat n -> SNat (S n) }
-- >>> deriving instance Show (SNat n) 
-- 
-- we can define 
--
-- >>> let conS = con1 SS -- taking a singleton(-like) constructor.
-- >>> :type conS
-- conS :: Lam SNat SNat (Con1 S)
--
-- and use it with term level functions
--
-- >>> map conS (SZ :* SS SZ :* SS (SS SZ) :* Nil)
-- SS SZ :* SS (SS SZ) :* SS (SS (SS SZ)) :* Nil
--
con1 :: LamRep a b (Con1 con) -> Lam a b (Con1 con)
con1 = mkLam

-- | A term-level constructor for 'Lam' of 'Con2'
con2 :: LamRep2 a b c (Con2 con) -> Lam2 a b c (Con2 con)
con2 = mkLam2

-- | A term-level constructor for 'Lam' of 'Con2'
con3 :: LamRep3 a b c d (Con3 con) -> Lam3 a b c d (Con3 con)
con3 = mkLam3
