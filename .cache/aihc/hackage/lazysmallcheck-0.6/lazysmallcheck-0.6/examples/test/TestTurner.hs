import Test.LazySmallCheck
import Turner
import System

instance Serial Var where
  series = cons0 V0 \/ cons0 V1

instance Serial Exp where
  series = cons2 (:@) \/ cons2 L \/ (cons1 V . (+1))

main = do [d] <- getArgs ; depthCheck (read d) prop_abstr
