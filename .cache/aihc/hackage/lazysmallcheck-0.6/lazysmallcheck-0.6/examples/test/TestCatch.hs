import Test.LazySmallCheck
import Catch
import System

instance Serial Value where
  series = cons0 Bottom \/ cons2 Value

instance Serial CtorName where
  series = cons0 Ctor \/ cons0 CtorN \/ cons0 CtorR \/ cons0 CtorNR

instance Serial Val where
  series = cons2 (:*) \/ cons0 Any

instance Serial Pattern where
  series = cons2 Pattern

main = do [d] <- getArgs ; depthCheck (read d) prop
