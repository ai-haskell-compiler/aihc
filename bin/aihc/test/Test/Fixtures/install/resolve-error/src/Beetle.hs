{-# LANGUAGE TypeApplications #-}

-- Imports a module that failed to type check: still checked, and its
-- own error is reported.
module Beetle where

import Aardvark ()

beetle = () @()
