module Message where

import Cycle

message :: String
message = cycleMessage

messageText :: String
messageText =
  if (1208925819614629174706176 :: Integer) == 1208925819614629174706176
    then "build-exe works"
    else "large integer failed"
