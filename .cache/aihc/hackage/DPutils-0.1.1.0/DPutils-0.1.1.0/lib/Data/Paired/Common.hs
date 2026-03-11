
module Data.Paired.Common where



-- | Shall we combine elements on the main diagonal as well?
--
-- If we choose @NoDiag@, we deal with upper triangular matrices that are
-- effectively one element smaller.

data OnDiag = OnDiag | NoDiag
  deriving (Eq)

-- | Select only a subset of the possible enumerations.

data Enumerate
  -- | Enumerate all elements
  = All
  -- | Enumerate from a value and at most @N@ elements
  | FromN Int Int
  deriving (Eq)

-- | If the size of the input is known before-hand or not.

data SizeHint
  = UnknownSize
  | KnownSize Int
  deriving (Eq)

