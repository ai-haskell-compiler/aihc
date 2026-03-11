import Test.LazySmallCheck
import Countdown
import System

main = do [d] <- getArgs ; depthCheck (read d) prop_lemma3
