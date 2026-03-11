-- | `Data.Flag.Phantom` is for strict typing of Flag.
--
--   When you want to distinguish two Flag by its origination, use this module instead of `Data.Flag`(`Data.Flag.Simple`).
module Data.Flag.Phantom (
    module Data.Flag.Phantom
  , module Data.Flag.Internal
  ) where


import Data.Foldable -- For base-4.7.0.*

import Data.Flag.Internal
import qualified Data.Flag.Simple as SF


newtype PhantomFlag t = PhFlag Flag deriving (Show, Eq, Ord)

-- | Take `Flag` from `PhantomFlag`
getFlag (PhFlag f) = f

encodeFlag :: (Foldable f, Bounded e, Enum e) => f e -> PhantomFlag t
encodeFlag = PhFlag . SF.encodeFlag

decodeFlag :: Enum e => PhantomFlag t -> [e]
decodeFlag = SF.decodeFlag . getFlag

showFlag :: PhantomFlag t -> String
showFlag (PhFlag f) = SF.showFlag f

showFlagFit :: (Bounded e, Enum e) => e -> PhantomFlag t -> String
showFlagFit a (PhFlag f) = SF.showFlagFit a f

showFlagBy :: Int -> PhantomFlag t -> String
showFlagBy l (PhFlag f) = SF.showFlagBy l f

readFlag :: String -> PhantomFlag t
readFlag = PhFlag . SF.readFlag

readEnum :: (Enum e) => String -> [e]
readEnum = SF.readEnum

include :: PhantomFlag t -> PhantomFlag t -> Bool
include (PhFlag f1) (PhFlag f2) = SF.include f1 f2
exclude :: PhantomFlag t -> PhantomFlag t -> Bool
exclude (PhFlag f1) (PhFlag f2) = SF.exclude f1 f2

about :: (Flag -> Flag -> b) -> PhantomFlag t -> PhantomFlag t -> PhantomFlag t -> b
about f (PhFlag fb) (PhFlag f1) (PhFlag f2)  = SF.about f fb f1 f2

eqAbout = about (==)

includeAbout = about SF.include
-- Should be tested that this really works properly!
excludeAbout = about SF.exclude

anyReq (PhFlag obj) (PhFlag req) = SF.anyReq obj req

allReq (PhFlag obj) (PhFlag req) = SF.allReq obj req
