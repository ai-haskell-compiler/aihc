module Provider (Describe (..), Token (..)) where

import Definitions

instance Describe Token where
  describe Token = Token
