import Test.LazySmallCheck
import Huffman
import System

instance Serial a => Serial (BTree a) where
  series = cons1 Leaf \/ cons2 Fork

main = do [d] <- getArgs ; depthCheck (read d) prop_decEnc
