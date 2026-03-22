module ExprDoLetFunctionBindingNested where

memoIO f = do
  v <- newMVar M.empty
  let f' x = do
        m <- readMVar v
        case M.lookup x m of
          Nothing -> do let { r = f x }; modifyMVar_ v (return . M.insert x r); return r
          Just r -> return r
  return f'
