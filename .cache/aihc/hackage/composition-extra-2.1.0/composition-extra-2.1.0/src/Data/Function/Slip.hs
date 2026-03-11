-- | Bit-shift style argument re-arrangement

module Data.Function.Slip where


slipr :: (a -> b -> c -> d) -> b -> c -> a -> d
slipr f b c a = f a b c

(<~>>) = slipr

infixl 8 <~>>


slipl :: (a -> b -> c -> d) -> c -> a -> b -> d
slipl f c a b = f a b c

(<<~>) = slipl

infixl 8 <<~>


slipr4 :: (a -> b -> c -> d -> e) -> b -> c -> d -> a -> e
slipr4 f b c d a = f a b c d

(<~~>>) = slipr4

infixl 8 <~~>>


slipl4 :: (a -> b -> c -> d -> e) -> d -> a -> b -> c -> e
slipl4 f d a b c = f a b c d

(<<~~>) = slipl4

infixl 8 <<~~>


slipr5  :: (a -> b -> c -> d -> e -> f) -> b -> c -> d -> e -> a -> f
slipr5 f b c d e a = f a b c d e

(<~~~>>) = slipr5

infixl 8 <~~~>>


slipl5  :: (a -> b -> c -> d -> e -> f) -> e -> a -> b -> c -> d -> f
slipl5 f e a b c d = f a b c d e

(<<~~~>) = slipl5

infixl 8 <<~~~>
