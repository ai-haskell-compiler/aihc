import Test.LazySmallCheck
import RegExp
import System

instance Serial Nat where
  series = cons0 Zer \/ cons1 Suc

instance Serial Sym where
  series = cons0 N0 \/ cons1 N1

instance Serial RE where
  series = cons1 Sym
        \/ cons2 Or
        \/ cons2 Seq
        \/ cons2 And
        \/ cons1 Star
        \/ cons0 Empty

main = do [d] <- getArgs ; depthCheck (read d) prop_regex
