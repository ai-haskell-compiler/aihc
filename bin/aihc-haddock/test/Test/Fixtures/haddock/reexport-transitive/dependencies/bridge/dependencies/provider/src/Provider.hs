module Provider (Box (..)) where

-- | A box with two alternatives.
data Box a
  = -- | A left box.
    LeftBox a
  | -- | A right box.
    RightBox a
