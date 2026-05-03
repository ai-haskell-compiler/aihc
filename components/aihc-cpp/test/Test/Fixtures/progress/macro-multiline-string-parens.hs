#define DEBUG_TRACE(s) {- nothing -}

happySimulateReduce r st sts =
  DEBUG_TRACE("simulate reduction of rule " ++ show r ++ ", (nested)")
  let result = reduce r st sts
  in result
