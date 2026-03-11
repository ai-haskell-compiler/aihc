import Test.LazySmallCheck
import Mux
import System

main = do [d] <- getArgs ; depthCheck (read d) prop_encode
