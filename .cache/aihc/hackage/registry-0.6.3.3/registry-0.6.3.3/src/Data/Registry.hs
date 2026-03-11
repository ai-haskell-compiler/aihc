-- |
--
-- Import this module if you want to access all the functionalities of the
-- Registry API
module Data.Registry
  ( module M,
    Typed (..),
  )
where

import Data.Registry.Dot as M -- Produce a graph out of a registry
import Data.Registry.Lift as M -- Lift functions into a monadic context
import Data.Registry.Make as M -- Various "make" functions to create components from a registry
import Data.Registry.Rio as M -- ResourceT + cache monad for effectful instantiation and singletons
import Data.Registry.Registry as M -- The Registry data structure
import Data.Registry.Solver as M -- Type-level constraints to check if we can make a component from a registry
import Data.Registry.Statistics as M -- Provide statistics about the execution of a registry
import Data.Registry.Internal.Types -- Export the Typed data type because it appears in Registry's API
