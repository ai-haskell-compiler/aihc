{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Validator
    ( -- * Core monad and runners
      ValidationM, ValidationT
    , ValidationRule, ValidationRuleT
    , TransValidationRule, TransValidationRuleT
    , runValidator, runValidatorT
      -- * Combinators
    , (>=>), (<=<)
      -- * Checks
    , minLength, maxLength, lengthBetween, notEmpty
    , largerThan, smallerThan, valueBetween
    , matchesRegex
    , conformsPred, conformsPredM
      -- * Transforming checks
    , requiredValue, nonEmptyList
    , conformsPredTrans, conformsPredTransM
      -- * Helper classes and types
    , HasLength(..), ConvertibleStrings(..)
    , Int64
      -- * Regular expression helpers
    , re, mkRegexQQ, Regex
    )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Data.Int
import Data.String.Conversions
import Text.Regex.PCRE.Heavy
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

-- | The validation monad
type ValidationM e = ValidationT e Identity

-- | The validation monad transformer
newtype ValidationT e m a
    = ValidationT { unValidationT :: ExceptT e m a }
      deriving (Monad, Functor, Applicative, Alternative, MonadPlus, MonadTrans)

-- | Run a validation on a type 'a'
runValidator :: TransValidationRule e a b -> a -> Either e b
runValidator a b = runIdentity $ runValidatorT a b
{-# INLINE runValidator #-}

-- | Run a validation on a type 'a'
runValidatorT :: Monad m => TransValidationRuleT e m a b -> a -> m (Either e b)
runValidatorT validationSteps input =
    runExceptT $ unValidationT (validationSteps input)
{-# INLINE runValidatorT #-}

-- | A validation rule. Combine using @('>=>')@ or @('<=<')@
type ValidationRule e a = ValidationRuleT e Identity a

-- | A validation rule. Combine using @('>=>')@ or @('<=<')@
type ValidationRuleT e m a = TransValidationRuleT e m a a

-- | A transforming validation rule. Combine using @('>=>')@ or @('<=<')@
type TransValidationRule e a b = TransValidationRuleT e Identity a b

-- | A transforming validation rule. Combine using @('>=>')@ or @('<=<')@
type TransValidationRuleT e m a b = a -> ValidationT e m b

-- | All types that have a length, eg. 'String', '[a]', 'Vector a', etc.
class HasLength a where
    getLength :: a -> Int64

instance HasLength [a] where
    getLength = fromIntegral . length
    {-# INLINE getLength #-}

instance HasLength T.Text where
    getLength = fromIntegral . T.length
    {-# INLINE getLength #-}

instance HasLength TL.Text where
    getLength = TL.length
    {-# INLINE getLength #-}

instance HasLength BS.ByteString where
    getLength = fromIntegral . BS.length
    {-# INLINE getLength #-}

instance HasLength BSL.ByteString where
    getLength = BSL.length
    {-# INLINE getLength #-}

-- | Mark a custom check as failed
checkFailed :: Monad m => e -> ValidationT e m a
checkFailed = ValidationT . throwE
{-# INLINE checkFailed #-}

-- | Check that the value is at least N elements long
minLength :: (Monad m, HasLength a) => Int64 -> e -> ValidationRuleT e m a
minLength lowerBound e obj = largerThan lowerBound e (getLength obj) >> return obj
{-# INLINE minLength #-}

-- | Check that the value is at maxium N elements long
maxLength :: (Monad m, HasLength a) => Int64 -> e -> ValidationRuleT e m a
maxLength upperBound e obj =
    smallerThan upperBound e (getLength obj) >> return obj
{-# INLINE maxLength #-}

-- | Check that the value's length is between N and M
lengthBetween :: (Monad m, HasLength a) => Int64 -> Int64 -> e -> ValidationRuleT e m a
lengthBetween lowerBound upperBound e obj = valueBetween lowerBound upperBound e (getLength obj) >> return obj
{-# INLINE lengthBetween #-}

-- | Specialized minLength with N = 1
notEmpty :: (Monad m, HasLength a) => e -> ValidationRuleT e m a
notEmpty = minLength 1
{-# INLINE notEmpty #-}

-- | Check that a value is larger than N
largerThan :: (Monad m, Ord a) => a -> e -> ValidationRuleT e m a
largerThan lowerBound = conformsPred (>= lowerBound)
{-# INLINE largerThan #-}

-- | Check that a value is smaller than N
smallerThan :: (Monad m, Ord a) => a -> e -> ValidationRuleT e m a
smallerThan upperBound = conformsPred (<= upperBound)
{-# INLINE smallerThan #-}

-- | Check that a value is between M and N
valueBetween :: (Monad m, Ord a) => a -> a -> e -> ValidationRuleT e m a
valueBetween lowerBound upperBound e = largerThan lowerBound e >=> smallerThan upperBound e
{-# INLINE valueBetween #-}

-- | Checks that a value matches a regular expression
matchesRegex :: (ConvertibleStrings SBS a, ConvertibleStrings a SBS, Monad m) => Regex -> e -> ValidationRuleT e m a
matchesRegex r = conformsPred (=~ r)
{-# INLINE matchesRegex #-}

-- | Check that a value conforms a predicate
conformsPred :: Monad m => (a -> Bool) -> e -> ValidationRuleT e m a
conformsPred predicate e obj = unless (predicate obj) (checkFailed e) >> return obj
{-# INLINE conformsPred #-}

-- | Check that a value conforms a predicate
conformsPredM :: Monad m => (a -> m Bool) -> e -> ValidationRuleT e m a
conformsPredM predicate e obj =
    do res <- lift $ predicate obj
       unless res (checkFailed e) >> return obj
{-# INLINE conformsPredM #-}

-- | Check that an optional value is actually set to 'Just a'
requiredValue :: Monad m => e -> TransValidationRuleT e m (Maybe a) a
requiredValue = conformsPredTrans id
{-# INLINE requiredValue #-}

-- | Check that a list is not empty
nonEmptyList :: Monad m => e -> TransValidationRuleT e m [a] (NEL.NonEmpty a)
nonEmptyList = conformsPredTrans NEL.nonEmpty
{-# INLINE nonEmptyList #-}

-- | Do some check returning 'Nothing' if the value is invalid and 'Just a' otherwise.
conformsPredTrans :: Monad m => (a -> Maybe b) -> e -> TransValidationRuleT e m a b
conformsPredTrans f e obj =
    case f obj of
      Nothing -> checkFailed e
      Just val -> return val
{-# INLINE conformsPredTrans #-}

-- | Do some check returning 'Nothing' if the value is invalid and 'Just a' otherwise.
conformsPredTransM :: Monad m => (a -> m (Maybe b)) -> e -> TransValidationRuleT e m a b
conformsPredTransM f e obj =
    do res <- lift $ f obj
       case res of
         Nothing -> checkFailed e
         Just val -> return val
{-# INLINE conformsPredTransM #-}
