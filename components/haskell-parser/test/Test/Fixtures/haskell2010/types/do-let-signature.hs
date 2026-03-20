module T12 where

timeItT :: IO a -> IO (Double, a)
timeItT ioa = do
  a <- ioa
  let t :: Double
      t = 0
  pure (t, a)
