{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module : System.Prompt
-- License : BSD-3-Clause
module System.Prompt.Requirement
  ( Requirement (..),
    SRequirement (..),
  )
where

-- | Whether an answer to a question is needed
--
-- This is a [singleton](https://hackage.haskell.org/package/singletons), deliberately named
-- in reverse compared to normal to simplify readability for library users unfamiliar with
-- singletons.
data Requirement (requirement :: SRequirement) where
  -- | An answer is required
  Required :: Requirement 'SRequired
  -- | An answer is not required
  Optional :: Requirement 'SOptional

-- | Whether an answer to a question is needed
data SRequirement
  = -- | An answer is required
    SRequired
  | -- | An answer is not required
    SOptional
