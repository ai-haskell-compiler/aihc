import Test.LazySmallCheck
import SumPuz
import System

main = do [d] <- getArgs ; depthCheck (read d) prop_Sound
