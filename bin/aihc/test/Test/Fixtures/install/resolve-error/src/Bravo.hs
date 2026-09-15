-- Imports a module whose names did not resolve: skipped, and silent.
module Bravo where

import Alpha

unbox :: AlphaBox -> ()
unbox (AlphaBox _) = ()
