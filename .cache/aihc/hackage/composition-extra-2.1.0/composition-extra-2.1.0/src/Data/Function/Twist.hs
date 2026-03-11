module Data.Function.Twist where


twist :: (a -> b -> c -> d -> e) -> c -> d -> a -> b -> e
twist fun c d a b = fun a b c d

(<<~~>>) = twist

infixl 8 <<~~>>


twist5r :: (a -> b -> c -> d -> e -> f) -> c -> d -> e -> a -> b -> f
twist5r fun c d e a b = fun a b c d e

(<<~~>>>) = twist5r

infixl 8 <<~~>>>


twist5l :: (a -> b -> c -> d -> e -> f) -> d -> e -> a -> b -> c -> f
twist5l fun d e a b c = fun a b c d e

(<<<~~>>) = twist5l

infixl 8 <<<~~>>


twist5 :: (a -> b -> c -> d -> e -> f) -> d -> e -> c -> a -> b -> f
twist5 fun d e c a b = fun a b c d e

(<<<~~>>>) = twist5

infixl 8 <<<~~>>>


twist6 :: (a -> b -> c -> d -> e -> f -> g) -> d -> e -> f -> a -> b -> c -> g
twist6 fun d e f a b c = fun a b c d e f

(<<~~~>>) = twist6

infixl 8 <<~~~>>


twist7r :: (a -> b -> c -> d -> e -> f -> g -> h) -> d -> e -> f -> g -> a -> b -> c -> h
twist7r fun d e f g a b c = fun a b c d e f g

(<<~~~>>>) = twist7r

infixl 8 <<~~~>>>


twist7l :: (a -> b -> c -> d -> e -> f -> g -> h) -> e -> f -> g -> a -> b -> c -> d -> h
twist7l fun e f g a b c d = fun a b c d e f g

(<<<~~~>>) = twist7l

infixl 8 <<<~~~>>


twist7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> e -> f -> g -> d -> a -> b -> c -> h
twist7 fun e f g d a b c = fun a b c d e f g

(<<<~~~>>>) = twist7

infixl 8 <<<~~~>>>
