module Data.Tuple.Tools (
	curry3,
	uncurry3,
	rotate
) where

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x, y, z)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

rotate :: Floating a => a -> (a, a) -> (a, a)
rotate rad (x, y) = (x * cos rad - y * sin rad, x * sin rad + y * cos rad)
