module Singleraeh.Bool where

import Data.Kind ( Type )
import Singleraeh.Demote

-- | Singleton 'Bool'.
type SBool :: Bool -> Type
data SBool b where
    STrue  :: SBool True
    SFalse :: SBool False

demoteSBool :: SBool b -> Bool
demoteSBool = \case STrue -> True; SFalse -> False

instance Demotable SBool where
    type Demote SBool = Bool
    demote = demoteSBool

class SingBool (b :: Bool) where singBool :: SBool b
instance SingBool True  where singBool = STrue
instance SingBool False where singBool = SFalse
