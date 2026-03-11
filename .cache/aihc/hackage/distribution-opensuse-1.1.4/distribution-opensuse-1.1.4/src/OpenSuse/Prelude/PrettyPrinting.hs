-- | This is simply a re-export of the standard library @pretty@ with some
-- orphan instances added for convenience.

module OpenSuse.Prelude.PrettyPrinting
  ( module Text.PrettyPrint.HughesPJClass
  )
  where

import OpenSuse.Prelude.PrettyPrinting.Orphans ( )

import Text.PrettyPrint.HughesPJClass
