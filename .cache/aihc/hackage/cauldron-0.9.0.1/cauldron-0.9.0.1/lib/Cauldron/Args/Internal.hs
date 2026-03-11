{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Cauldron.Args.Internal where

import Cauldron.Beans (Beans, SomeMonoidTypeRep (..), fromDynList)
import Cauldron.Beans qualified
import Control.Exception (Exception, throw)
import Data.Dynamic
import Data.Foldable qualified
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Kind
import Data.Sequence (Seq)
import Data.Sequence qualified
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable
import Type.Reflection (SomeTypeRep (..))
import Type.Reflection qualified

-- | An 'Applicative' that knows how to construct values by searching in a
-- 'Beans' map, and keeps track of the types that will be searched in the
-- 'Beans' map.
data Args a = Args
  { _argReps :: Set SomeTypeRep,
    _regReps :: Set SomeMonoidTypeRep,
    _runArgs :: (forall t. (Typeable t) => Maybe t) -> a
  }
  deriving stock (Functor)

-- | Look for a type in the 'Beans' map and return its corresponding value.
--
-- >>> :{
-- fun1 :: Bool -> Int
-- fun1 _ = 5
-- w1 :: Args Int
-- w1 = fun1 <$> arg
-- fun2 :: String -> Bool -> Int
-- fun2 _ _ = 5
-- w2 :: Args Int
-- w2 = fun2 <$> arg <*> arg
-- :}
arg :: forall a. (Typeable a) => Args a
arg =
  let tr = typeRep (Proxy @a)
   in Args
        { _argReps = Set.singleton tr,
          _regReps = Set.empty,
          _runArgs = \f ->
            case f @a of
              Just v -> v
              Nothing -> throw (LazilyReadBeanMissing tr)
        }

-- | Here the 'Beans' map is not passed /directly/, instead, we pass a
-- function-like value that, given a type, will return a value of that type or
-- 'Nothing'. Such function is usually constructed using 'taste' on some 'Beans'
-- map.
--
-- >>> :{
-- let beans = fromDynList [toDyn @Int 5]
--  in runArgs (taste beans) (arg @Int)
-- :}
-- 5
--
-- See also 'LazilyReadBeanMissing'.
runArgs :: (forall b. (Typeable b) => Maybe b) -> Args a -> a
runArgs f (Args _ _ _runArgs) =
  -- https://www.reddit.com/r/haskell/comments/16diti/comment/c7vc9ky/
  _runArgs f

-- | Inspect ahead of time what types will be searched in the 'Beans' map.
--
-- >>> :{
-- let beans = fromDynList [toDyn @Int 5, toDyn False]
--     args = (,) <$> arg @Int <*> arg @Bool
--  in (getArgsReps args, runArgs (taste beans) args)
-- :}
-- (fromList [Int,Bool],(5,False))
getArgsReps :: Args a -> Set TypeRep
getArgsReps (Args {_argReps}) = _argReps

-- | Tweak the look-by-type function that is eventually passed to 'runArgs'.
--
-- Unlikely to be commonly useful.
--
-- >>> :{
-- let tweak :: forall t. Typeable t => Maybe t -> Maybe t
--     tweak _ = case Type.Reflection.typeRep @t
--                    `Type.Reflection.eqTypeRep`
--                    Type.Reflection.typeRep @Int of
--                  Just HRefl -> Just 5
--                  Nothing -> Nothing
--  in runArgs (taste Cauldron.Beans.empty) $ contramapArgs tweak $ arg @Int
-- :}
-- 5
contramapArgs :: (forall t. (Typeable t) => Maybe t -> Maybe t) -> Args a -> Args a
contramapArgs tweak args@Args {_runArgs} = args {_runArgs = \f -> _runArgs (tweak f)}

-- | Inspect ahead of time the types of registrations that might be contained in
-- the result value of an 'Args'.
--
-- >>> :{
-- let args = foretellReg @(Sum Int) *> pure ()
--  in getRegsReps args
-- :}
-- fromList [Sum Int]
getRegsReps :: Args a -> Set SomeMonoidTypeRep
getRegsReps (Args {_regReps}) = _regReps

-- | This function is used in an 'Args' context to create a tell-like function
-- that can later be used to register a value into a 'Regs'.
--
-- The type of the future registration must be an instance of 'Monoid'.
--
-- There are no other ways of registering values into 'Regs'.
foretellReg :: forall a. (Typeable a, Monoid a) => Args (a -> Regs ())
foretellReg =
  let tr = SomeMonoidTypeRep (Type.Reflection.typeRep @a)
   in Args
        { _argReps = Set.empty,
          _regReps = Set.singleton tr,
          _runArgs = \_ a -> Regs (Data.Sequence.singleton (toDyn a)) ()
        }

instance Applicative Args where
  pure a =
    Args
      { _argReps = Set.empty,
        _regReps = Set.empty,
        _runArgs = \_ -> a
      }
  Args
    { _argReps = _argReps1,
      _regReps = _regReps1,
      _runArgs = f
    }
    <*> Args
      { _argReps = _argReps2,
        _regReps = _regReps2,
        _runArgs = a
      } =
      Args
        { _argReps = _argReps1 `Set.union` _argReps2,
          _regReps = _regReps1 `Set.union` _regReps2,
          _runArgs = \beans -> (f beans) (a beans)
        }

someMonoidTypeRepToSomeTypeRep :: SomeMonoidTypeRep -> SomeTypeRep
someMonoidTypeRepToSomeTypeRep (SomeMonoidTypeRep tr) = SomeTypeRep tr

-- | A writer-like monad for collecting the values of registrations.
data Regs a = Regs (Seq Dynamic) a
  deriving stock (Functor)

-- | Extract the 'Beans' map of registrations, along with the main result value.
--
-- The 'Set' of 'SomeMonoidTypeRep's will typically come from 'getRegsReps'.
--
-- Only values for 'TypeRep's present in the set will be returned. There will be
-- values for all 'TypeRep's present in the set (some of them might be the
-- 'mempty' for that type).
runRegs :: Set SomeMonoidTypeRep -> Regs a -> (Beans, a)
runRegs monoidReps (Regs dyns a) =
  -- https://www.reddit.com/r/haskell/comments/16diti/comment/c7vc9ky/
  let onlyStaticlyKnown =
        ( manyMemptys monoidReps : do
            dyn <- Data.Foldable.toList dyns
            -- This bit is subtle. I mistakenly used Cauldron.Beans.singleton here
            -- and ended up with the Dynamic type as the *key*. It was hell to debug.
            [fromDynList [dyn]]
        )
          & do foldl (Cauldron.Beans.unionBeansMonoidally monoidReps) (mempty @Beans)
          & do flip Cauldron.Beans.restrictKeys (Set.map someMonoidTypeRepToSomeTypeRep monoidReps)
   in (onlyStaticlyKnown, a)

instance Applicative Regs where
  pure a = Regs Data.Sequence.empty a
  Regs w1 f <*> Regs w2 a2 =
    Regs (w1 Data.Sequence.>< w2) (f a2)

instance Monad Regs where
  (Regs w1 a) >>= k =
    let Regs w2 r = k a
     in Regs (w1 Data.Sequence.>< w2) r

manyMemptys :: Set SomeMonoidTypeRep -> Beans
manyMemptys reps =
  reps
    & Data.Foldable.toList
    <&> Cauldron.Beans.someMonoidTypeRepMempty
    & fromDynList

-- | Imprecise exception that might lie hidden in the result of 'runArgs', if
-- the 'Beans' map lacks a value for some type demanded by the 'Args'.
--
-- Why not make 'runArgs' return a 'Maybe' instead of throwing an imprecise
-- exception? The answer is that, for my purposes, using 'Maybe' or 'Either'
-- caused undesirable strictness when doing weird things like reading values
-- \"from the future\".
--
-- >>> :{
-- try @LazilyReadBeanMissing $ evaluate $ runArgs (taste Cauldron.Beans.empty) (arg @Int)
-- :}
-- Left (LazilyReadBeanMissing Int)
--
-- If more safety is needed, one can perform additional preliminary checks with
-- the help of 'getArgsReps'.
newtype LazilyReadBeanMissing = LazilyReadBeanMissing TypeRep
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Convenience typeclass for wiring all the arguments of a curried function in one go.
class Wireable curried tip | curried -> tip where
  -- | Takes a curried function and reads all of its arguments by type using
  -- 'arg', returning an 'Args' for the final result value of the function.
  --
  -- This function assumes that the @tip@ is not a function, in order to know
  -- when to stop collecting arguments. If your intended @tip@ is a function,
  -- you might need to wrap it in a newtype in order to disambiguate.
  --
  -- >>> :{
  -- fun0 :: Int
  -- fun0 = 5
  -- w0 :: Args Int
  -- w0 = wire fun0
  -- fun1 :: Bool -> Int
  -- fun1 _ = 5
  -- w1 :: Args Int
  -- w1 = wire fun1
  -- fun2 :: String -> Bool -> Int
  -- fun2 _ _ = 5
  -- w2 :: Args Int
  -- w2 = wire fun2
  -- :}
  wire :: curried -> Args tip

instance (Wireable_ (IsFunction curried) curried tip) => Wireable curried tip where
  wire curried = wire_ (Proxy @(IsFunction curried)) do pure curried

class Wireable_ (where_ :: Where) curried tip | where_ curried -> tip where
  wire_ :: Proxy where_ -> Args curried -> Args tip

instance Wireable_ AtTheTip a a where
  wire_ _ r = r

instance (Typeable b, Wireable_ (IsFunction rest) rest tip) => Wireable_ NotYetThere (b -> rest) tip where
  wire_ _ af = wire_ (Proxy @(IsFunction rest)) do af <*> arg @b

type IsFunction :: Type -> Where
type family IsFunction f :: Where where
  IsFunction (_ -> _) = 'NotYetThere
  IsFunction _ = 'AtTheTip

data Where
  = NotYetThere
  | AtTheTip

data WhereNested
  = Tup2
  | Tup3
  | Tup4
  | Innermost

type IsReg :: Type -> WhereNested
type family IsReg f :: WhereNested where
  IsReg (_, _) = 'Tup2
  IsReg (_, _, _) = 'Tup3
  IsReg (_, _, _, _) = 'Tup4
  IsReg _ = 'Innermost

-- | Convenience typeclass for automatically extracting registrations from a value.
-- Counterpart of 'Wireable' for registrations.
class Registrable nested tip | nested -> tip where
  -- | We look for (potentially nested) tuples in the value. All tuple
  -- components except the rightmost-innermost must have 'Monoid' instances, and
  -- are put into a 'Regs'.
  --
  -- >>> :{
  -- args :: Args (Identity (Sum Int, All, String))
  -- args = pure (Identity (Sum 5, All False, "foo"))
  -- registeredArgs :: Args (Identity (Regs String))
  -- registeredArgs = register args
  -- :}
  --
  -- >>> :{
  -- let reps = getRegsReps registeredArgs
  --  in ( reps == Data.Set.fromList [ SomeMonoidTypeRep $ Type.Reflection.typeRep @(Sum Int)
  --                                 , SomeMonoidTypeRep $ Type.Reflection.typeRep @All]
  --     , registeredArgs & runArgs (taste Cauldron.Beans.empty)
  --                      & runIdentity
  --                      & runRegs reps
  --                      & \(beans,_) -> (taste @(Sum Int) beans, taste @All beans)
  --     )
  -- :}
  -- (True,(Just (Sum {getSum = 5}),Just (All {getAll = False})))
  --
  -- Tuples can be nested:
  --
  -- >>> :{
  -- args :: Args (Identity (Sum Int, (All, String)))
  -- args = pure (Identity (Sum 5, (All False, "foo")))
  -- registeredArgs :: Args (Identity (Regs String))
  -- registeredArgs = register args
  -- :}
  --
  -- If there are no tuples in the result type, no values are put into 'Regs'.
  --
  -- >>> :{
  -- args :: Args (Identity String)
  -- args = pure (Identity "foo")
  -- registeredArgs :: Args (Identity (Regs String))
  -- registeredArgs = register args
  -- :}
  register :: forall m. (Functor m) => Args (m nested) -> Args (m (Regs tip))

instance (Registrable_ (IsReg nested) nested tip) => Registrable nested tip where
  register amnested = register_ (Proxy @(IsReg nested)) do fmap (fmap pure) amnested

class Registrable_ (where_ :: WhereNested) nested tip | where_ nested -> tip where
  register_ :: forall m. (Functor m) => Proxy where_ -> Args (m (Regs nested)) -> Args (m (Regs tip))

instance Registrable_ Innermost a a where
  register_ _ = id

instance (Typeable b, Monoid b, Registrable_ (IsReg rest) rest tip) => Registrable_ Tup2 (b, rest) tip where
  register_ _ af =
    register_ (Proxy @(IsReg rest)) do
      tell1 <- foretellReg @b
      action <- af
      pure (action <&> \regs -> regs >>= \(b, rest) -> tell1 b *> pure rest)

instance (Typeable b, Monoid b, Typeable c, Monoid c, Registrable_ (IsReg rest) rest tip) => Registrable_ Tup3 (b, c, rest) tip where
  register_ _ af =
    register_ (Proxy @(IsReg rest)) do
      tell1 <- foretellReg @b
      tell2 <- foretellReg @c
      action <- af
      pure (action <&> \regs -> regs >>= \(b, c, rest) -> tell1 b *> tell2 c *> pure rest)

instance (Typeable b, Monoid b, Typeable c, Monoid c, Typeable d, Monoid d, Registrable_ (IsReg rest) rest tip) => Registrable_ Tup3 (b, c, d, rest) tip where
  register_ _ af =
    register_ (Proxy @(IsReg rest)) do
      tell1 <- foretellReg @b
      tell2 <- foretellReg @c
      tell3 <- foretellReg @d
      action <- af
      pure (action <&> \regs -> regs >>= \(b, c, d, rest) -> tell1 b *> tell2 c *> tell3 d *> pure rest)

-- $registrations
--
-- The 'Args' applicative has an additional feature: it lets you \"register\"
-- ahead of time the types of some values that /might/ be included in the result
-- of the 'Args', but without being reflected in the result type. It's not
-- mandatory that these values must be ultimately produced, however.
--
-- Here's an example. We have an 'Args' value that returns a 'Regs'. While
-- constructing the 'Args' value, we register the @Sum Int@ and @All@ types
-- using 'foretellReg', which also gives us the means of later writing into the
-- 'Regs'. By using 'getRegsReps', we can inspect the 'TypeRep's of the types we
-- registered without having to run the 'Args',
--
-- >>> :{
-- fun2 :: String -> Bool -> Int
-- fun2 _ _ = 5
-- args :: Args (Regs Int)
-- args = do -- Using ApplicativeDo
--   r <- fun2 <$> arg <*> arg -- could also have used 'wire'
--   tell1 <- foretellReg @(Sum Int)
--   tell2 <- foretellReg @All
--   pure $ do
--      tell1 (Sum 11)
--      tell2 (All False)
--      pure r
-- :}
--
-- >>> :{
-- let reps = getRegsReps args
--  in ( reps == Data.Set.fromList [ SomeMonoidTypeRep $ Type.Reflection.typeRep @(Sum Int)
--                                 , SomeMonoidTypeRep $ Type.Reflection.typeRep @All]
--     , args & runArgs (taste $ fromDynList [toDyn @String "foo", toDyn False])
--            & runRegs reps
--            & \(beans,_) -> (taste @(Sum Int) beans, taste @All beans)
--     )
-- :}
-- (True,(Just (Sum {getSum = 11}),Just (All {getAll = False})))

-- $setup
-- >>> :set -XBlockArguments
-- >>> :set -XOverloadedLists
-- >>> :set -XApplicativeDo
-- >>> :set -XGADTs
-- >>> :set -Wno-incomplete-uni-patterns
-- >>> import Data.Functor.Identity
-- >>> import Data.Function ((&))
-- >>> import Data.Monoid
-- >>> import Cauldron.Beans (taste)
-- >>> import System.IO
-- >>> import Control.Exception
