module NLP.Minimorph.Number where

-- | Singular and Plural.
data SingPlu a = SP
    { sg :: a
    , pl :: a
    }
  deriving (Show, Eq)

data Number = Singular | Plural
  deriving (Eq, Show)

fromSP :: Number -> SingPlu a -> a
fromSP Singular = sg
fromSP Plural   = pl
