{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Data.Registry.THSpec where

import Data.Generics.Product.Typed
import Data.Registry.TH
import Protolude

-- * Example of TemplateHaskell usage to create the typeclass corresponding to a component

-- | We define 2 low-level components: Logger and Tracer
--   We make the corresponding typeclasses
data Logger m = Logger
  { -- a function taking 2 arguments
    _info :: Text -> Text -> m (),
    -- a function having a constraint
    _error :: forall a. (Monoid a) => a -> Text -> m ()
  }

makeTypeclass ''Logger

newtype Tracer m = Tracer
  { _traceIt :: Text -> m ()
  }

makeTypeclass ''Tracer

-- | Now we make a component which is going to use both components
newtype Service m = Service
  { doIt :: Int -> Text -> m ()
  }

newService :: Monad m => Logger m -> Tracer m -> Service m
newService logger tracer =
  Service
    { doIt = \n t -> run (implementService n t)
    }
  where
    -- the typeclass instances can be resolved by using a Reader containing
    --  a tuple with all the components
    run = flip runReaderT (logger, tracer)

-- | Implement the service using typeclasses
implementService ::
  Monad m =>
  WithLogger m =>
  WithTracer m =>
  Int ->
  Text ->
  m ()
implementService n t = do
  info "doing it" t
  traceIt (show n)
