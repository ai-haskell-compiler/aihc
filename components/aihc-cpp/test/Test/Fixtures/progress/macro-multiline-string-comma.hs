#define DEBUG_TRACE(s) {- nothing -}

happyDoAction i tk st =
  DEBUG_TRACE("state: " ++ show st ++
              ", token: " ++ show i ++
              ", action: ")
  case action of
    Done -> done
