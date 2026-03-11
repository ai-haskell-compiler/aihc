{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Numeric.CollectErrors.Type where

import Control.CollectErrors
  ( CanTakeErrors,
    CanTestErrorsCertain (..),
    CanTestErrorsPresent,
    CollectErrors (CollectErrors),
    lift1TCE,
    lift2CE,
    liftCE,
    liftT1CE,
    noValue,
    prependErrors,
    removeValue,
    unCollectErrors,
  )
import Control.DeepSeq
import qualified Data.List as List
import qualified Data.Set as Set
import GHC.Generics

type CN = CollectErrors NumErrors

cn :: v -> CN v
cn = pure

{-| Unsafe way to get a value out of the CN wrapper. -}
unCN :: CN p -> p
unCN = unCollectErrors

{-| Unsafe way to get the result of a function out of the CN wrapper. -}
unCNfn1 :: (a -> CN p) -> a -> p
unCNfn1 fn a = unCollectErrors (fn a)

{-| Unsafe way to get the result of a binary function out of the CN wrapper. -}
unCNfn2 :: (a -> b -> CN p) -> a -> b -> p
unCNfn2 fn a b = unCollectErrors (fn a b)

newtype NumErrors = NumErrors (Set.Set NumErrorLevel)
  deriving (Eq, Semigroup, Monoid, CanTestErrorsCertain, CanTestErrorsPresent, Generic, NFData)

type NumErrorLevel = (NumError, ErrorCertaintyLevel)

instance Show NumErrors where
  show (NumErrors set) =
    "{" <> (List.intercalate "; " $ map showEL $ Set.toList set) <> "}"
    where
      showEL (e, l) =
        show l <> ": " <> show e

data NumError
  = DivByZero
  | OutOfDomain String
  | NumError String
  deriving (Eq, Ord, Generic)

instance NFData NumError

instance Show NumError where
  show DivByZero = "division by 0"
  show (OutOfDomain s) = "out of domain: " ++ s
  show (NumError s) = "numeric error: " ++ s

data ErrorCertaintyLevel
  = ErrorCertain
  | ErrorPotential
  deriving (Eq, Ord, Generic)

instance NFData ErrorCertaintyLevel

instance Show ErrorCertaintyLevel where
  show ErrorCertain = "ERROR"
  show ErrorPotential = "POTENTIAL ERROR"

instance CanTestErrorsCertain NumErrorLevel where
  hasCertainError = (== ErrorCertain) . snd

-- | Construct an empty wrapper indicating that given error has certainly occurred.
noValueNumErrorCertain :: NumError -> CN v
noValueNumErrorCertain e = noValue $ NumErrors $ Set.singleton (e, ErrorCertain)

-- | Construct an empty wrapper indicating that given error may have occurred.
noValueNumErrorPotential :: NumError -> CN v
noValueNumErrorPotential e = noValue $ NumErrors $ Set.singleton (e, ErrorPotential)

removeValueErrorCertain :: CN t -> NumError -> CN t
removeValueErrorCertain v e =
  removeValue v $ NumErrors $ Set.singleton (e, ErrorCertain)

removeValueErrorPotential :: CN t -> NumError -> CN t
removeValueErrorPotential v e =
  removeValue v $ NumErrors $ Set.singleton (e, ErrorPotential)

prependErrorCertain :: NumError -> CN t -> CN t
prependErrorCertain e = prependErrors $ NumErrors $ Set.singleton (e, ErrorCertain)

prependErrorPotential :: NumError -> CN t -> CN t
prependErrorPotential e = prependErrors $ NumErrors $ Set.singleton (e, ErrorPotential)

class CanClearPotentialErrors cnt where
  -- |
  --    If there is a value, remove any potential errors that are associated with it.
  clearPotentialErrors :: cnt -> cnt

instance CanClearPotentialErrors (CN t) where
  clearPotentialErrors (CollectErrors (Just v) (NumErrors es)) =
    CollectErrors (Just v) (NumErrors $ Set.filter notPotential es)
    where
      notPotential (_, ErrorPotential) = False
      notPotential _ = True
  clearPotentialErrors ce = ce

instance (CanClearPotentialErrors t1, CanClearPotentialErrors t2) => CanClearPotentialErrors (t1, t2) where
  clearPotentialErrors (v1, v2) = (clearPotentialErrors v1, clearPotentialErrors v2)

instance (CanClearPotentialErrors t) => CanClearPotentialErrors [t] where
  clearPotentialErrors = map clearPotentialErrors

liftCN :: (a -> (CN c)) -> (CN a) -> (CN c)
liftCN = liftCE

lift2CN :: (a -> b -> (CN c)) -> (CN a) -> (CN b) -> (CN c)
lift2CN = lift2CE

lift1TCN :: (a -> b -> (CN c)) -> (CN a) -> b -> (CN c)
lift1TCN = lift1TCE

liftT1CN :: (a -> b -> (CN c)) -> a -> (CN b) -> (CN c)
liftT1CN = liftT1CE

type CanTakeCNErrors = CanTakeErrors NumErrors
