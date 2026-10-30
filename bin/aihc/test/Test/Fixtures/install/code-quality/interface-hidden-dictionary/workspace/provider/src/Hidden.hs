module Hidden where
data Flag = Yes | No
class Pick a where
  pick :: a -> Flag
instance Pick Flag where
  pick x = x
