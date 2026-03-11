module Singleraeh.Maybe where

import Data.Kind ( Type, Constraint )
import Singleraeh.Demote

-- | Singleton 'Maybe'.
type SMaybe :: (a -> Type) -> Maybe a -> Type
data SMaybe sa ma where
    SJust    :: sa a -> SMaybe sa (Just a)
    SNothing ::         SMaybe sa Nothing

demoteSMaybe
    :: forall da sa ma
    .  (forall a. sa a -> da)
    -> SMaybe sa ma
    -> Maybe da
demoteSMaybe demoteSA = \case
  SJust sa -> Just $ demoteSA sa
  SNothing -> Nothing

instance Demotable sa => Demotable (SMaybe sa) where
    type Demote (SMaybe sa) = Maybe (Demote sa)
    demote = demoteSMaybe demote

class SingMaybe (ca :: ak -> Constraint) (sa :: ak -> Type) (ma :: Maybe ak) where
    singMaybe'
        :: (forall a. ca a => sa a)
        -> SMaybe sa ma

singMaybe
    :: forall ca sa ma. SingMaybe ca sa ma
    => (forall a. ca a => sa a)
    -> SMaybe sa ma
singMaybe = singMaybe' @_ @ca

instance ca a => SingMaybe ca sa (Just a) where
    singMaybe' sa = SJust sa

instance SingMaybe ca sa Nothing where
    singMaybe' _  = SNothing
