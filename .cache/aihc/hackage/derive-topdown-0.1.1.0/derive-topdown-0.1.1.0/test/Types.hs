{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE StarIsType #-}   
#endif
{-# LANGUAGE DeriveGeneric #-}            
{-# LANGUAGE GeneralizedNewtypeDeriving #-}            
{-# LANGUAGE DerivingStrategies #-}            
{-# LANGUAGE DeriveFunctor #-}            
{-# LANGUAGE RankNTypes #-}            
{-# LANGUAGE StandaloneDeriving #-}            
{-# LANGUAGE DataKinds #-}            
{-# LANGUAGE TypeOperators #-}            
{-# LANGUAGE GADTs #-}            
{-# LANGUAGE KindSignatures #-}            
{-# LANGUAGE TypeFamilies #-}            
{-# LANGUAGE FlexibleContexts #-}            
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PackageImports #-}


module Types where
import           Arity
import           Control.Monad.IO.Class
import           Data.Binary
import           Data.Derive.Superclass
import           Data.Derive.TopDown
import           Data.Derive.TopDown.Breaks
import           Data.Proxy
import           Data.Ratio
import           GHC.Generics            hiding ( C
                                                , D
                                                )
import qualified "haskell-src"  Language.Haskell.Syntax as H

-- types for testing

-- ^ simple cases
type Age = Int
data Person = Person
  { pname :: String
  , age   :: Age
  }
data Department = Department
  { head   :: Person
  , staffs :: [Person]
  }
data Company = Company
  { manager     :: Person
  , departments :: [Department]
  }

$(deriving_superclasses ''Ord ''Person)
$(deriving_ ''Show ''Company)
$(deriving_ ''Generic ''Company)
$(instance_ ''Binary ''Company)
$(deriving_th_with (''TypeArity, makeTypeArity) ''Company [int, char])

-- for enumeration
data E0 = E01 | E02 | E03 | E04

$(deriving_superclasses ''Ord ''E0)
$(deriving_ ''Show ''E0)
$(deriving_ ''Generic ''E0)
$(deriving_th (''TypeArity, makeTypeArity) ''E0)
data I a = I a
data B a b = B a b
data T a b c = T a b c

$(deriving_superclasses ''Ord ''I)
$(deriving_superclasses ''Ord ''B)
$(deriving_superclasses ''Ord ''T)

$(deriving_ ''Show ''I)
$(deriving_ ''Show ''B)
$(deriving_ ''Show ''T)

$(deriving_ ''Generic ''I)
$(deriving_ ''Generic ''B)
$(deriving_ ''Generic ''T)

$(instance_ ''Binary ''I)
$(instance_ ''Binary ''B)
$(instance_ ''Binary ''T)

$(deriving_th (''TypeArity, makeTypeArity) ''I)
$(deriving_th (''TypeArity, makeTypeArity) ''B)
$(deriving_th (''TypeArity, makeTypeArity) ''T)

-- regular constructor
data T1 = T11 Int Bool Char
        | T12 String
        | T13 [Int]
        | T14 (Int, Bool, Char)

$(deriving_superclasses ''Ord ''T1)
$(deriving_ ''Show ''T1)
$(deriving_ ''Generic ''T1)
$(instance_ ''Binary ''T1)
$(deriving_th (''TypeArity, makeTypeArity) ''T1)

data T2 a b = T21 a [b]
            | T22 b (Bool) (Either a b)
            | T23 Int ((a, b)) (Maybe a)
            | T24 (T3 Maybe a b)


data T3 (k :: * -> *) (a :: *) b = T31 (k a) [b]
                                 | T32 b Bool (a) (Maybe a)
                                 | T33 Int (a, k b)
                                 | T34 (T2 a b)
-- 2 mutual recursive declaration
$(deriving_ ''Show ''T2)
$(deriving_ ''Show ''T3)

-- $(deriving_superclasses ''Ord ''T2)
-- $(deriving_superclasses ''Ord ''T3)
fmap concat (sequence [(deriving_superclasses ''Ord ''T2), (deriving_superclasses ''Ord ''T3)])
$(deriving_ ''Generic ''T2)
$(deriving_ ''Generic ''T3)
$(instance_ ''Binary ''T2)
$(instance_ ''Binary ''T3)

$(deriving_th (''TypeArity, makeTypeArity) ''T2)

-- mutual recursive declaration tests
data R0 a = R01 (R1 a)
data R1 x = R11 (R2 x) x
data R2 f = R21 (R3 f)
data R3 v = R31 (R4 v)
data R4 v = R41 (R5 v)
data R5 b = R51 (R6 b)
data R6 b = R61 (R1 b)
$(deriving_ ''Show ''R0)
$(deriving_ ''Generic ''R0)
$(deriving_ ''Generic1 ''R0)
$(instance_ ''Binary ''R0)
$(deriving_th (''TypeArity, makeTypeArity) ''R0)

data Rx0 a b c = Rx01 a (Rx1 Int a c) (Rx2 b c)
data Rx1 x y z = Rx11 z (Rx2 z x) | Rx12 (Rx0 Bool Char y)
data Rx2 f g = Rx21 (Rx3 Int g) (Rx1 f g f)
data Rx3 v b = Rx31 v | Rx32 (Rx1 Int Char b) b
$(deriving_th (''TypeArity, makeTypeArity) ''Rx0)

$(deriving_ ''Show ''Rx0)
$(deriving_ ''Generic ''Rx0)
$(deriving_with ''Generic1 ''Rx0 Nothing [''Rx1, ''Rx2] genInferredContext)
$(deriving_with ''Generic1 ''Rx3 Nothing [''Rx1] genInferredContext)
$(instance_ ''Binary ''Rx0)


-- forall type
-- deriving for this is unsupported by GHC
data T4 a = T41 (forall b . Show b => Either a b)
$(deriving_th_with (''TypeArity, makeTypeArity) ''T4 [''Either])

-- Type Operator
data (:=:) a b = E11 a b | E12 (a,b) | E13 [b] Int
$(deriving_ ''Show ''(:=:))
$(deriving_ ''Generic ''(:=:))


data T5 a b = T5 (a :=: b)

$(deriving_ ''Show ''T5)
$(deriving_ ''Generic ''T5)
$(deriving_ ''Generic1 ''T5)
$(deriving_th (''TypeArity, makeTypeArity) ''T5)
-- data type record

-- GADTs
data Exp a where
    Add :: Int  -> Int -> Exp Int
    And :: Bool -> Bool -> Exp Bool
    Le  :: Int  -> Int -> Exp Bool

$(deriving_ ''Show ''Exp)
$(deriving_th (''TypeArity, makeTypeArity) ''Exp)

-- Cannot derive Generic for GADT
-- must be a vanilla data constructor to derive
-- $(deriving_ ''Generic ''Exp)

data Aggregator = SUM | AVG | MIN | MAX | CNT | CNTD | ATTR

data Granularity = G0 | G1

data DataQL a where
  DimRaw :: String -> DataQL G0
  MsrRaw :: String -> DataQL G0
  Agg :: Aggregator -> DataQL G0 -> DataQL G1
  Include :: [DataQL G0] -> DataQL G1 -> DataQL G0
  Exclude :: [DataQL G0] -> DataQL G1 -> DataQL G0
  Fixed   :: [DataQL G0] -> DataQL G1 -> DataQL G0

$(deriving_ ''Show ''DataQL)
$(deriving_th (''TypeArity, makeTypeArity) ''DataQL)
-- TODO: Record GADTs

-- ^ phantom
-- TODO: test a is not in the context
data P1 a = P1C1 (P2 a)
data P2 b = P2C1 Int

$(deriving_ ''Show ''P1)
$(deriving_ ''Generic ''P1)
$(deriving_th (''TypeArity, makeTypeArity) ''P1)

-- ^ type family
type family F a b

type instance F Int Char = Bool
type instance F String Char = String
type instance F a Int = Maybe a

type family G x

-- TODO: test G b is not in the context
data T6 a b c = T61 (Maybe (F a c)) a (Proxy (G b))
$(deriving_ ''Show ''T6)
$(deriving_ ''Generic ''T6)
-- TODO: template instance generation need to handle type family
-- $(deriving_th (''TypeArity, makeTypeArity) ''T6)

-- ^ recursive definition
data List a = Nil | Cons a (List a)
$(deriving_ ''Show ''List)
$(deriving_ ''Generic ''List)
$(deriving_ ''Generic1 ''List)
$(deriving_th (''TypeArity, makeTypeArity) ''List)

-- ^ recursive defintion
data Stream k a b c = S (k a) | S2 a (Stream k b c Int)
    -- deriving Eq
{-
deriving Eq in GHC for Stream will not put (k Int) in the context
-}
-- context is (k b),(k b), a, b, c should be 
$(deriving_ ''Show ''Stream)
$(deriving_ ''Generic ''Stream)
$(deriving_th (''TypeArity, makeTypeArity) ''Stream)

-- ^ composite 
-- TODO: test b is not in context
data P a b = P (E a (Px b))
data E x y = L x | R y
data Px z = Px
$(deriving_ ''Show ''P)
$(deriving_ ''Generic ''P)
$(deriving_ ''Functor ''P)
$(deriving_ ''Generic1 ''P)
$(deriving_th (''TypeArity, makeTypeArity) ''P)
#if __GLASGOW_HASKELL__ >= 804
$(deriving_with ''Binary ''E Nothing [] genHoleContext)
#else
$(deriving_with ''Binary ''E Nothing [] genInferredContext)
#endif

-- $(instance_with ''Binary ''E  [] Nothing genHoleContext)
-- GHC deriving does not work on ForallT 
data C b = C (forall a. Show a => a) b | C2 Int b (Maybe Bool)

-- C must not have exotic unlifted or polymorphic argument
-- $(deriving_ ''Generic ''C)
$(deriving_th (''TypeArity, makeTypeArity) ''C)

-- ^ ForallC
data D a = forall b. Show b => D b | D2 a
$(deriving_ ''Show ''D)
$(deriving_th (''TypeArity, makeTypeArity) ''D)

-- ^ for superclass MonadIO 
newtype IO_ a = IO_ (IO a)
$(strategy_deriving_superclasses NewtypeStrategy ''MonadIO ''IO_)
$(deriving_th (''TypeArity, makeTypeArity) ''IO_)

-- ^ for superclass RealFloat
newtype F32 = F32 Float
$(strategy_deriving_superclasses NewtypeStrategy ''RealFloat ''F32)
$(deriving_th (''TypeArity, makeTypeArity) ''F32)

-- deriving GHC syntax tree in haskell-src
$(deriving_ ''Ord ''H.HsModule)
$(deriving_with ''Generic ''H.HsModule Nothing [''Ratio] genInferredContext)
$(instance_ ''Binary ''H.HsModule)
$(deriving_th (''TypeArity, makeTypeArity) ''H.HsModule)

