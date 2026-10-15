{-# LANGUAGE NoImplicitPrelude #-}

-- | The small Prelude of The Lazy Reader.
--
-- A module of the reader imports this module without an import line, as
-- it imports the real Prelude in GHC. The module gathers the classes,
-- types and functions of the guide in one place.
module Prelude
  ( module GHC.Classes,
    module GHC.Num,
    module GHC.Prim.Base,
    Bool (..),
    Char,
    Int,
    Ordering (..),
    IO,
  )
where

import GHC.Classes
import GHC.Num
import GHC.Prim.Base
import GHC.Types (Bool (..), Char, IO, Int, Ordering (..))
