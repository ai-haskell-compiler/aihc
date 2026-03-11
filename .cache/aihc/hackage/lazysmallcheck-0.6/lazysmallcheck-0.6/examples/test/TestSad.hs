import Test.LazySmallCheck
import Sad
import System

main = do [d] <- getArgs ; depthCheck (read d) prop_binSad
