-- Tests succeed if module compiles
module Test.Format.Compile
  (
  )
where

-- broken on older GHCs or time
--    • Couldn't match representation of type: proxy t
--                               with that of: proxy (Wrapped t)
