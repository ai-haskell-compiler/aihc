module Control.Monad.Syntax.Four where


(====<<) :: Monad m =>
            (a -> b -> c -> d -> m e)
         -> m a
         -> b -> c -> d -> m e
(====<<) mf x b c d = x >>= (\a -> mf a b c d)

infixr 1 ====<<

(=.==<<) :: Monad m =>
            (a -> b -> c -> d -> m e)
         -> m b
         -> a -> c -> d -> m e
(=.==<<) mf x a c d = x >>= (\b -> mf a b c d)

infixr 1 =.==<<

(==.=<<) :: Monad m =>
            (a -> b -> c -> d -> m e)
         -> m c
         -> a -> b -> d -> m e
(==.=<<) mf x a b d = x >>= (\c -> mf a b c d)

infixr 1 ==.=<<

(===.<<) :: Monad m =>
            (a -> b -> c -> d -> m e)
         -> m d
         -> a -> b -> c -> m e
(===.<<) mf x a b c = x >>= mf a b c

infixr 1 ===.<<
