{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-
  This module shows how to use the registry with hedgehog generators
-}
module Test.Data.Registry.GenSpec where

import Data.List (partition)
import Data.Registry
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Protolude as P
import Test.Tasty.Extensions hiding (forAll)
import Test.Tasty.Extensions qualified as Tasty

-- * DATA MODEL

newtype Company = Company {departments :: [Department]}
  deriving (Eq, Show)

newtype Department = Department {employees :: [Employee]}
  deriving (Eq, Show)

data Employee = Employee
  { name :: Name,
    age :: Age,
    salary :: Salary
  }
  deriving (Eq, Show)

newtype Name = Name Text deriving (Eq, Show)

newtype Age = Age Int deriving (Eq, Show, Ord, Num)

data Salary
  = Fixed Int
  | Variable Int Double
  deriving (Eq, Show)

isFixed (Fixed _) = True
isFixed _ = False

-- * GENERATORS

genText :: Gen Text
genText = Gen.text (Range.linear 2 10) Gen.ascii

genList :: forall a. (Typeable a) => Gen a -> Gen [a]
genList = Gen.list (Range.linear 0 3)

genInt :: Gen Int
genInt = Gen.int (Range.linear 1 100)

genDouble :: Gen Double
genDouble = Gen.double (Range.linearFrac 1 100)

setDepartmentWithOneEmployee :: Monad m => RegistryProperty m ()
setDepartmentWithOneEmployee = do
  e <- forAll @Employee
  tweakGen @[Employee] (const $ pure [e])

setCompanyWithOneDepartment :: Monad m => RegistryProperty m ()
setCompanyWithOneDepartment = do
  d <- forAll @Department
  tweakGen @[Department] (const (pure [d]))

setMinimalCompany :: Monad m => RegistryProperty m ()
setMinimalCompany =
  -- be careful, this is NOT commutative!
  -- if you set a company with one department first you may end up
  -- with a department with no employees, generated once and forall
  setDepartmentWithOneEmployee
    >> setCompanyWithOneDepartment

-- | Create a registry for all generators
registry =
  funTo @Gen Company
    <+ fun (genList @Department)
    <+ funTo @Gen Department
    <+ fun (genList @Employee)
    <+ funTo @Gen Employee
    <+ funTo @Gen Age
    <+ funTo @Gen Fixed
    <+ funTo @Gen Name
    <+ fun genInt
    <+ fun genText
    <+ fun genDouble

test_company_with_one_employee = noShrink $
  prop "generate just one employee" $
    runR $ do
      setMinimalCompany
      company <- forAll @Company
      let allEmployees = company & departments >>= (& employees)
      length allEmployees === 1

-- * WITH VARIANTS

registry' =
  fun (sequence . replicate @(Gen Salary) 100)
    <: fun salaryGen
    <: funTo @Gen (tag @"Fixed" Fixed)
    <: funTo @Gen (tag @"Variable" Variable)
    <: registry

salaryGen :: Gen (Tag "Fixed" Salary) -> Gen (Tag "Variable" Salary) -> Gen Salary
salaryGen fixed variable = choice [unTag <$> fixed, unTag <$> variable]

test_with_different_salaries = noShrink $
  prop "generate both fixed and variable salaries" $
    runWith registry' $ do
      salaries <- forAll @[Salary]
      let (fixed, variables) = partition isFixed salaries

      annotate "the choice operator allows us to generate both fixed and variable salaries"
      not (null fixed) === True
      not (null variables) === True

-- * HELPERS

type RegistryProperty m a = forall ins out. StateT (Registry ins out) (PropertyT m) a

forAll :: forall a m. (HasCallStack, Typeable a, Show a, Monad m) => RegistryProperty m a
forAll = withFrozenCallStack $ get >>= P.lift . Tasty.forAll . make @(Gen a)

tweakGen :: forall a m. (Typeable a, Monad m) => (Gen a -> Gen a) -> RegistryProperty m ()
tweakGen f = modify $ tweak @(Gen a) f

runR :: Monad m => RegistryProperty m a -> PropertyT m a
runR = runWith registry

runWith :: Monad m => Registry ins out -> RegistryProperty m a -> PropertyT m a
runWith r rp = evalStateT rp r
