module Control.Monad.Syntax.Six where


(======<<) :: Monad m =>
              (a -> b -> c -> d -> e -> f -> m g)
           -> m a
           -> b -> c -> d -> e -> f -> m g
(======<<) mf x b c d e f = x >>= (\a -> mf a b c d e f)

infixr 1 ======<<

(=.====<<) :: Monad m =>
              (a -> b -> c -> d -> e -> f -> m g)
           -> m b
           -> a -> c -> d -> e -> f -> m g
(=.====<<) mf x a c d e f = x >>= (\b -> mf a b c d e f)

infixr 1 =.====<<

(==.===<<) :: Monad m =>
              (a -> b -> c -> d -> e -> f -> m g)
           -> m c
           -> a -> b -> d -> e -> f -> m g
(==.===<<) mf x a b d e f = x >>= (\c -> mf a b c d e f)

infixr 1 ==.===<<

(===.==<<) :: Monad m =>
              (a -> b -> c -> d -> e -> f -> m g)
           -> m d
           -> a -> b -> c -> e -> f -> m g
(===.==<<) mf x a b c e f = x >>= (\d -> mf a b c d e f)

infixr 1 ===.==<<

(====.=<<) :: Monad m =>
              (a -> b -> c -> d -> e -> f -> m g)
           -> m e
           -> a -> b -> c -> d -> f -> m g
(====.=<<) mf x a b c d f = x >>= (\e -> mf a b c d e f)

infixr 1 ====.=<<

(=====.<<) :: Monad m =>
              (a -> b -> c -> d -> e -> f -> m g)
           -> m f
           -> a -> b -> c -> d -> e -> m g
(=====.<<) mf x a b c d e = x >>= mf a b c d e

infixr 1 =====.<<
