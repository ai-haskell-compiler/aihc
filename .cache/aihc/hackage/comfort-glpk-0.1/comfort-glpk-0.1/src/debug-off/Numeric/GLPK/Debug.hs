module Numeric.GLPK.Debug where

import qualified Math.Programming.Glpk.Header as FFI
import Control.Monad (void)


initLog :: IO ()
initLog = void $ FFI.glp_term_out FFI.glpkOff
