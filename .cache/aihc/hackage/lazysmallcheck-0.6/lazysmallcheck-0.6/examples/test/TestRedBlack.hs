import Test.LazySmallCheck
import RedBlack
import System

instance Serial Colour where
  series = cons0 R \/ cons0 B

instance Serial a => Serial (Tree a) where
  series = cons0 E \/ cons4 T

main = do [d] <- getArgs ; depthCheck (read d) prop_insertRB
