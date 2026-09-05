module Message where

import Cycle

message :: String
message = cycleMessage

-- | A polymorphic head in front of the application operator: the checked
-- condition of the if expression must be Bool, not the head's type variable.
keep :: Bool -> a -> a
keep _ x = x

messageText :: String
messageText =
  if keep True $ (1208925819614629174706176 :: Integer) == 1208925819614629174706176
    then "build-exe works"
    else "large integer failed"
