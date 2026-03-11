import Test.LazySmallCheck
import ListSet
import System

main = do [d] <- getArgs ; depthCheck (read d) prop_insertSet
