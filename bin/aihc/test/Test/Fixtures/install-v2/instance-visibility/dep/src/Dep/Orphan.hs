module Dep.Orphan where

import Dep.Types

instance Mark Token where
  mark value = value
