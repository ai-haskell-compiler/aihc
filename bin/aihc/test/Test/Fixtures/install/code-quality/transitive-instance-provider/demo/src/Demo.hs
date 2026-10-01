-- | The instance of 'Describe' for 'Token' lives in @provider@, which this
-- package does not depend on. The instance reaches this module through
-- @Middle@, which imports it.
module Demo (described) where

import Middle (Token, describe, token)

described :: Token
described = describe token
