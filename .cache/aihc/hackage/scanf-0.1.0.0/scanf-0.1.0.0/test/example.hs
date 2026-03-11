{-# LANGUAGE QuasiQuotes #-}

import Text.Scanf

main :: IO ()
main = do
  scanf [fmt|%c|] "34"
    =? Just ('3' :+ ())

  scanf [fmt|%d|] "10"
    =? Just (10 :+ ())

  scanf [fmt|%f|] "4.3"
    =? Just (4.3 :+ ())

  scanf [fmt| %l|]
        "    1000000000000000000000000000000000"
    =? Just (1000000000000000000000000000000000 :+ ())

  scanf [fmt|%s %s|] " a b"
    =? Just ("" :+ "a" :+ ())

  scanf [fmt|%%|] "%"
    =? Just ()

  scanf [fmt|a a|] "a  a b"
    =? Just ()

  let s0 = "10-11-12   Fri  4.3C"
      f0 = [fmt|%d-%d-%l %s %f %c|]
      f0' = fmt_ (int . "-" % int . "-" % integer . " " % string . " " % double . " " % char)
      r0 = Just
        ((10 :: Int) :+ (11 :: Int) :+ (12 :: Integer)
          :+ "Fri" :+ (4.3 :: Double) :+ 'C' :+ ())
  scanf f0 s0 =? r0
  scanf f0' s0 =? r0

  let s1 = "3 lazy functions and 4 strict fields"
      f1 = [fmt|%d lazy %s and %d strict %s|]
      f1' = fmt_ (int . " lazy " % string . " and " % int . " strict " % string)
      r1 = Just ((3 :: Int) :+ "functions" :+ (4 :: Int) :+ "fields" :+ ())
      p1 = (2 :: Int) :+ "dogs" :+ (3 :: Int) :+ "cats" :+ ()
      q1 = "2 lazy dogs and 3 strict cats"
  scanf f1  s1 =? r1
  scanf f1' s1 =? r1

  printf f1  p1 =? q1
  printf f1' p1 =? q1

assertEq, (=?) :: (Eq a, Show a) => a -> a -> IO ()
assertEq a b = do
  if a == b then
    return ()
  else
    fail $ show a ++ " /= " ++ show b

(=?) = assertEq

