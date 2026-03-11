{-# LANGUAGE DeriveGeneric #-}

module Numeric.EMD.Internal (
    EMDOpts(..)
  , BoundaryHandler(..)
  , Sifter(..)
  , SM
  , SingleSift(..)
  ) where

import           Control.Monad.Trans.Reader
import           Data.Conduino
import           Data.Void
import           GHC.Generics
import           Numeric.EMD.Internal.Spline
import qualified Data.Binary                 as Bi
import qualified Data.Vector.Generic.Sized   as SVG

-- | Options for EMD composition.
data EMDOpts v n a = EO
    { eoSifter          :: Sifter v n a           -- ^ stop condition for sifting
    , eoSplineEnd       :: SplineEnd a            -- ^ end conditions for envelope splines
    , eoBoundaryHandler :: Maybe BoundaryHandler  -- ^ process for handling boundary
    }
  deriving (Generic)

-- | Boundary conditions for splines.
data BoundaryHandler
    -- | Clamp envelope at end points (Matlab implementation)
    = BHClamp
    -- | Extend boundaries symmetrically
    | BHSymmetric
  deriving (Show, Eq, Ord, Generic)

-- | @since 0.1.3.0
instance Bi.Binary BoundaryHandler

-- | Result of a single sift
data SingleSift v n a = SingleSift
    { ssResult :: !(SVG.Vector v n a)
    , ssMinEnv :: !(SVG.Vector v n a)
    , ssMaxEnv :: !(SVG.Vector v n a)
    }

-- | Monad where 'Sifter' actions live.  The reader parameter is the
-- "original vector".
type SM v n a = Reader (SVG.Vector v n a)

-- | A sift stopping condition.
--
-- It is a 'Pipe' consumer that takes single sift step results upstream and
-- terminates with '()' as soon as it is satisfied with the latest sift
-- step.
--
-- Use combinators like 'siftOr' and 'siftAnd' to combine sifters, and the
-- various sifters in "Numeric.EMD.Sift" to create sifters from commonly
-- established ones or new ones from scratch.
--
-- @since 0.2.0.0
newtype Sifter v n a = Sifter { sPipe :: Pipe (SingleSift v n a) Void Void (SM v n a) () }

