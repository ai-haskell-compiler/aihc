import Test.LazySmallCheck
import Mate
import System

instance Serial Kind where
  series = cons0 King
      \/ cons0 Queen
      \/ cons0 Rook
      \/ cons0 Bishop
      \/ cons0 Knight
      \/ cons0 Pawn

instance Serial Colour where
  series = cons0 Black \/ cons0 White

instance Serial Board where
  series = cons2 Board

main = do [d] <- getArgs ; depthCheck (read d) prop_checkmate
