-- | Description: Internal
module Exon.Class.Newtype where

import Exon.Data.Segment (Segment)
import Exon.Generic (IsNewtype, OverNt)
import Unsafe.Coerce (unsafeCoerce)

-- | Internal auxiliary class that applies a function to the value inside of a nested chain of 'Generic' newtypes.
class OverNewtype (current :: Type) (wrapped :: Maybe Type) (inner :: Type) | current -> inner where
  overNewtype :: (NonEmpty (Segment inner) -> inner) -> NonEmpty (Segment current) -> current

instance (
    OverNt current wrapped,
    IsNewtype wrapped next,
    OverNewtype wrapped next inner
  ) => OverNewtype current ('Just wrapped) inner where
    overNewtype f segments =
      unsafeCoerce (overNewtype @wrapped @next @inner f (unsafeCoerce segments))
    {-# inline overNewtype #-}

instance OverNewtype current 'Nothing current where
  overNewtype = id
  {-# inline overNewtype #-}

-- | Internal auxiliary class that applies a function to the value inside of a nested chain of 'Generic' newtypes.
--
-- The method only passes its arguments to 'overNewtypes', but the class hides the intermediate parameter.
class OverNewtypes (result :: Type) (inner :: Type) | result -> inner where
  overNewtypes :: (NonEmpty (Segment inner) -> inner) -> NonEmpty (Segment result) -> result

instance (
    IsNewtype result wrapped,
    OverNewtype result wrapped inner
  ) => OverNewtypes result inner where
    overNewtypes = overNewtype @result @wrapped
    {-# inline overNewtypes #-}
