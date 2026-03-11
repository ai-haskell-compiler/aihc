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

module Cauldron.Args
  ( -- * Arguments
    Args,
    arg,
    runArgs,
    getArgsReps,
    contramapArgs,

    -- ** Reducing 'arg' boilerplate with 'wire'
    Wireable (wire),

    -- ** When a bean is missing
    LazilyReadBeanMissing (..),

    -- * Registrations
    -- $registrations
    Regs,
    foretellReg,
    runRegs,
    getRegsReps,

    -- ** Reducing 'foretellReg' boilerplate with 'register'
    Registrable (register),

    -- * Re-exports
    Beans,
    taste,
    fromDynList,
    SomeMonoidTypeRep,
  )
where

import Cauldron.Args.Internal
import Cauldron.Beans (Beans, SomeMonoidTypeRep (..), fromDynList, taste)
