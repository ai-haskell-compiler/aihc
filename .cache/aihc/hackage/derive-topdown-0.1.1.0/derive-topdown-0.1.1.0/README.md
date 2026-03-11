# derive-topdown
[![Hackage version](https://img.shields.io/hackage/v/derive-topdown.svg?label=Hackage)](https://hackage.haskell.org/package/derive-topdown) [![Stackage version](https://www.stackage.org/package/derive-topdown/badge/lts?label=Stackage)](https://www.stackage.org/package/derive-topdown)

This is a Haskell project which will derive type class instances from top for a composite data type.

## 1. Standalone deriving
There are functions named `deriving_`, `derivings`, `derivingss`. Please see the API for their types.

``` haskell
{-# LANGUAGE StandaloneDeriving,
    ConstraintKinds,
    UndecidableInstances,
    GADTs,
    TemplateHaskell,
    DeriveGeneric #-}
{-# OPTIONS_GHC -ddump-splices #-}

import Data.Derive.TopDown
import GHC.Generics
import Data.Binary
import Data.Aeson
import Data.Aeson.TH

data Gender = Male | Female
type Age = Int
data Person a = P {name :: String , age :: Int, gender :: Gender}
data Department a = D {dname :: String , 
                        head :: Person a, 
                        staff :: [Person a]}
data Company a = C {cname :: String, 
                    departments :: [Department a]}

derivings [''Eq, ''Ord, ''Generic] ''Company
```
You will get:
```haskell
derivings [''Eq, ''Ord, ''Generic] ''Company
======>
deriving instance Eq Gender
deriving instance Eq (Person a_acKV)
deriving instance Eq a_acKU => Eq (Department a_acKU)
deriving instance Eq a_acKT => Eq (Company a_acKT)
deriving instance Ord Gender
deriving instance Ord (Person a_acKV)
deriving instance Ord a_acKU => Ord (Department a_acKU)
deriving instance Ord a_acKT => Ord (Company a_acKT)
deriving instance Generic Gender
deriving instance Generic (Person a_acKV)
deriving instance Generic (Department a_acKU)
deriving instance Generic (Company a_acKT)
```
## 2. Empty Instances generation

For empty class instances deriving, `instance_`, `instances`, `instancess` are provided. We can use it in this way.

```haskell
instances [''Binary] ''Company
======>
instance Binary Gender
instance Binary (Person a_af50)
instance Binary a_af4Z => Binary (Department a_af4Z)
instance Binary a_af4Y => Binary (Company a_af4Y)
```
## 3. Usage with Template Haskell
For generating instances with a template Haskell function, `deriving_th`, `deriving_ths` and `deriving_thss` can be used:
```haskell
deriving_ths
    [(''ToJSON, deriveToJSON defaultOptions),
    (''FromJSON, deriveFromJSON defaultOptions)]
    ''Company
======>
instance ToJSON Gender where
    toJSON
    = \ value_amQG
        -> case value_amQG of {
                Male -> String (text-1.2.2.2:Data.Text.pack "Male")
                Female -> String (text-1.2.2.2:Data.Text.pack "Female") }
    toEncoding
    = \ value_amQH
        -> case value_amQH of {
                Male
                -> Data.Aeson.Encoding.Internal.text
                    (text-1.2.2.2:Data.Text.pack "Male")
                Female
                -> Data.Aeson.Encoding.Internal.text
                    (text-1.2.2.2:Data.Text.pack "Female") }
instance ToJSON a_amqg => ToJSON (Person a_amqg) where
    toJSON
    = \ value_amQy
    ...
    ...
```
You can use this this function with [`derive`](http://hackage.haskell.org/package/derive) package. However, it seems that this package is broken with GHC >= 802.

## 4. Deriving the superclasses
`Data.Derive.Superclass` provides `deriving_superclasses`, `strategy_deriving_superclasses` and newtype_deriving_superclasses, gnds can be used to derive class instance and its superclass instances. 

For example:
```haskell
data A = A
deriving_superclasses ''Ord ''A
```
You wil get:
```haskell
deriving_superclasses ''Ord ''A
======>
deriving instance Ord A
deriving instance Eq A
```

Mutual recursive type decalrations need to derive superclass together as the following:
```haskell
fmap concat (sequence [(deriving_superclasses ''Ord ''T2), (deriving_superclasses ''Ord ''T3)])
```
In the code above, T2 and T3 are mutual recursive data types.

## 5. Deriving with strategies in GHC 8.2
If you want to specify the strategy for deriving mechanism then `strategy_deriving`, `strategy_derivings` and `strategy_derivingss` can be used.
The 3 strategies for deriving `StockStrategy`,`AnyclassStrategy`,`NewtypeStrategy` are exposed when you import `TopDown`. They can be written as `stock`, `anyclass` as the default grammar. For `newtype`, you can write it as `newtype_` since there is a clison with `newtype` for data declaration. Please see [DerivingStrategies](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/DerivingStrategies)

## Note
### Derive with breaks
The deriving process might be interrupted by a given list of type names. The generation process will stop on those give names. Please see the test case.

### **NOTE**:  About deriving instances of Typeable
There is a bug with `isInstance` function when working with Typeable class. See [`ticket #11251`](https://ghc.haskell.org/trac/ghc/ticket/11251). So there might be problems if you really want to derive `Typeable` class. However, this bug should not affect you too much here since GHC now has `AutoDeriveTypeable` extension, which means you should never derive `Typeable` manually.

### **NOTE**: You cannot derive a type synonym.
`derive-topdown` will not work with `-XTypeSynonymInstances` language extension. The top node in the data declaration tree has to be a data or newtype.

More discussion about `derive-topdown`, please see [`ticket #10607`](https://ghc.haskell.org/trac/ghc/ticket/10607)
