module Data.License.Type where

data License =
    MIT
  | ISC

  | BSD2
  | BSD3
  | BSD4

  | GPLv2
  | GPLv3

  | LGPLv2_1
  | LGPLv3

  | AGPLv3

  | MPL_2_0
  | Apache_2_0
  deriving (Eq, Show, Bounded, Enum)
