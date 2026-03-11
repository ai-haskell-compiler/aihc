module Data.Text.ENIG.Data where


import           Data.Text               (Text)
import qualified Data.Text               as T


-- TODO: Optimize order of PPPI by Hangul statistical data
-- | Category of PostPosition Particle in Hangul
data PPPCategory
  = EN  -- ^ __은/는__
  | EL  -- ^ __을/를__
  | IG  -- ^ __이/가__
  | WG  -- ^ __와/과__
  | AY  -- ^ __아/야__
  | IX  -- ^ __이/null__
  | EuX -- ^ __으/null__
  deriving (Eq,Ord,Enum,Bounded,Show,Read)

-- | Identity of PostPosition Particle in Hangul
data PPPIdentity
  = Eun  -- ^ __/은/__
  | Neun -- ^ __/는/__
  | Eul  -- ^ __/을/__
  | Leul -- ^ __/를/__
  | I    -- ^ __/이/__
  | Ga   -- ^ __/가/__
  | Wa   -- ^ __/와/__
  | Gwa  -- ^ __/과/__
  | A    -- ^ __/아/__
  | Ya   -- ^ __/야/__
  | Ix   -- ^ Continuous __/이/__
  | Eux  -- ^ __/으/__
  | X    -- ^ __/null/__
  deriving (Eq,Ord,Enum,Bounded,Show,Read)

type Code = Int
