{- ORACLE_TEST pass -}
module S5ImportAsHiding where
import Data.Maybe as M hiding (fromMaybe)
x = M.isNothing Nothing