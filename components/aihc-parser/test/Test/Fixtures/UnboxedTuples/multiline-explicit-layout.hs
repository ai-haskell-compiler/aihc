{-# LANGUAGE UnboxedTuples #-}
module MultilineExplicitLayout where {

x :: Int -> (# Int, Int #);
x n =
  (#
    n
  , 2
  #);

f :: (# Int, Int #) -> Int;
f
  (# a
   , _
   #) = a;

g :: (# Int
      , Int #) -> (# Int
                  , Int #);
g t = t

}
