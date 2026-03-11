module Data.Functor.Syntax where

import Data.Function.Syntax


-- * Nested Mapping

(<$$>) :: (Functor f0, Functor f1) =>
          (a -> b)
       -> f1 (f0 a)
       -> f1 (f0 b)
(<$$>) = fmap . fmap

infixr 8 <$$>

(<$$$>) :: (Functor f0, Functor f1, Functor f2) =>
          (a -> b)
       -> f2 (f1 (f0 a))
       -> f2 (f1 (f0 b))
(<$$$>) = fmap . fmap . fmap

infixr 8 <$$$>

(<$$$$>) :: (Functor f0, Functor f1, Functor f2, Functor f3) =>
          (a -> b)
       -> f3 (f2 (f1 (f0 a)))
       -> f3 (f2 (f1 (f0 b)))
(<$$$$>) = fmap . fmap . fmap . fmap

infixr 8 <$$$$>

(<$$$$$>) :: (Functor f0, Functor f1, Functor f2, Functor f3, Functor f4) =>
          (a -> b)
       -> f4 (f3 (f2 (f1 (f0 a))))
       -> f4 (f3 (f2 (f1 (f0 b))))
(<$$$$$>) = fmap . fmap . fmap . fmap . fmap

infixr 8 <$$$$$>

(<&&>) :: (Functor f0, Functor f1) =>
          f1 (f0 a)
       -> (a -> b)
       -> f1 (f0 b)
(<&&>) = flip (fmap . fmap)

infixl 1 <&&>

(<&&&>) :: (Functor f0, Functor f1, Functor f2) =>
          f2 (f1 (f0 a))
       -> (a -> b)
       -> f2 (f1 (f0 b))
(<&&&>) = flip (fmap . fmap . fmap)

infixl 1 <&&&>

(<&&&&>) :: (Functor f0, Functor f1, Functor f2, Functor f3) =>
          f3 (f2 (f1 (f0 a)))
       -> (a -> b)
       -> f3 (f2 (f1 (f0 b)))
(<&&&&>) = flip (fmap . fmap . fmap . fmap)

infixl 1 <&&&&>

(<&&&&&>) :: (Functor f0, Functor f1, Functor f2, Functor f3, Functor f4) =>
          f4 (f3 (f2 (f1 (f0 a))))
       -> (a -> b)
       -> f4 (f3 (f2 (f1 (f0 b))))
(<&&&&&>) = flip (fmap . fmap . fmap . fmap . fmap)

infixl 1 <&&&&&>

-- * Nested Application

(<$~>) :: Functor f0 =>
          f0 (a -> b)
       -> a -> f0 b
(<$~>) f x = fmap ($ x) f

infixl 8 <$~>

(<~$>) :: Functor f0 =>
          (a -> b -> c)
       -> f0 b
       -> f0 (a -> c)
(<~$>) f = fmap (f -$)

infixr 8 <~$>

(<$~~>) :: Functor f0 =>
          f0 (a -> b -> c)
       -> b -> f0 (a -> c)
(<$~~>) f x = fmap (-$ x) f

infixl 8 <$~~>

(<~~$>) :: Functor f0 =>
           (a -> b -> c -> d)
        -> f0 c
        -> f0 (a -> b -> d)
(<~~$>) f = fmap (f --$)

infixr 8 <~~$>

(<$~~~>) :: Functor f0 =>
          f0 (a -> b -> c -> d)
       -> c -> f0 (a -> b -> d)
(<$~~~>) f x = fmap (--$ x) f

infixl 8 <$~~~>

(<~~~$>) :: Functor f0 =>
            (a -> b -> c -> d -> e)
         -> f0 d
         -> f0 (a -> b -> c -> e)
(<~~~$>) f = fmap (f ---$)

infixr 8 <~~~$>

(<$$~>) :: (Functor f0, Functor f1) =>
           f1 (f0 (a -> b))
        -> a -> f1 (f0 b)
(<$$~>) f x = fmap (<$~> x) f

infixl 8 <$$~>

(<~$$>) :: (Functor f0, Functor f1) =>
           (a -> b -> c)
        -> f1 (f0 b)
        -> f1 (f0 (a -> c))
(<~$$>) f = fmap (f <~$>)

infixr 8 <~$$>

(<$$~~>) :: (Functor f0, Functor f1) =>
            f1 (f0 (a -> b -> c))
         -> b -> f1 (f0 (a -> c))
(<$$~~>) f x = fmap (<$~~> x) f

infixl 8 <$$~~>

(<~~$$>) :: (Functor f0, Functor f1) =>
            (a -> b -> c -> d)
         -> f1 (f0 c)
         -> f1 (f0 (a -> b -> d))
(<~~$$>) f = fmap (f <~~$>)

infixr 8 <~~$$>

(<$$~~~>) :: (Functor f0, Functor f1) =>
             f1 (f0 (a -> b -> c -> d))
          -> c -> f1 (f0 (a -> b -> d))
(<$$~~~>) f x = fmap (<$~~~> x) f

infixl 8 <$$~~~>

(<~~~$$>) :: (Functor f0, Functor f1) =>
             (a -> b -> c -> d -> e)
          -> f1 (f0 d)
          -> f1 (f0 (a -> b -> c -> e))
(<~~~$$>) f = fmap (f <~~~$>)

infixr 8 <~~~$$>


(<$$$~>) :: (Functor f0, Functor f1, Functor f2) =>
            f2 (f1 (f0 (a -> b)))
         -> a -> f2 (f1 (f0 b))
(<$$$~>) f x = fmap (<$$~> x) f

infixl 8 <$$$~>

(<~$$$>) :: (Functor f0, Functor f1, Functor f2) =>
            (a -> b -> c)
         -> f2 (f1 (f0 b))
         -> f2 (f1 (f0 (a -> c)))
(<~$$$>) f = fmap (f <~$$>)

infixr 8 <~$$$>

(<$$$~~>) :: (Functor f0, Functor f1, Functor f2) =>
            f2 (f1 (f0 (a -> b -> c)))
         -> b -> f2 (f1 (f0 (a -> c)))
(<$$$~~>) f x = fmap (<$$~~> x) f

infixl 8 <$$$~~>

(<~~$$$>) :: (Functor f0, Functor f1, Functor f2) =>
             (a -> b -> c -> d)
          -> f2 (f1 (f0 c))
          -> f2 (f1 (f0 (a -> b -> d)))
(<~~$$$>) f = fmap (f <~~$$>)

infixr 8 <~~$$$>

(<$$$~~~>) :: (Functor f0, Functor f1, Functor f2) =>
             f2 (f1 (f0 (a -> b -> c -> d)))
          -> c -> f2 (f1 (f0 (a -> b -> d)))
(<$$$~~~>) f x = fmap (<$$~~~> x) f

infixl 8 <$$$~~~>

(<~~~$$$>) :: (Functor f0, Functor f1, Functor f2) =>
              (a -> b -> c -> d -> e)
           -> f2 (f1 (f0 d))
           -> f2 (f1 (f0 (a -> b -> c -> e)))
(<~~~$$$>) f = fmap (f <~~~$$>)

infixr 8 <~~~$$$>


(<$$$$~>) :: (Functor f0, Functor f1, Functor f2, Functor f3) =>
             f3 (f2 (f1 (f0 (a -> b))))
          -> a -> f3 (f2 (f1 (f0 b)))
(<$$$$~>) f x = fmap (<$$$~> x) f

infixl 8 <$$$$~>

(<~$$$$>) :: (Functor f0, Functor f1, Functor f2, Functor f3) =>
             (a -> b -> c)
          -> f3 (f2 (f1 (f0 b)))
          -> f3 (f2 (f1 (f0 (a -> c))))
(<~$$$$>) f = fmap (f <~$$$>)

infixr 8 <~$$$$>

(<$$$$~~>) :: (Functor f0, Functor f1, Functor f2, Functor f3) =>
            f3 (f2 (f1 (f0 (a -> b -> c))))
         -> b -> f3 (f2 (f1 (f0 (a -> c))))
(<$$$$~~>) f x = fmap (<$$$~~> x) f

infixl 8 <$$$$~~>

(<~~$$$$>) :: (Functor f0, Functor f1, Functor f2, Functor f3) =>
              (a -> b -> c -> d)
           -> f3 (f2 (f1 (f0 c)))
           -> f3 (f2 (f1 (f0 (a -> b -> d))))
(<~~$$$$>) f = fmap (f <~~$$$>)

infixr 8 <~~$$$$>

(<$$$$~~~>) :: (Functor f0, Functor f1, Functor f2, Functor f3) =>
             f3 (f2 (f1 (f0 (a -> b -> c -> d))))
          -> c -> f3 (f2 (f1 (f0 (a -> b -> d))))
(<$$$$~~~>) f x = fmap (<$$$~~~> x) f

infixl 8 <$$$$~~~>

(<~~~$$$$>) :: (Functor f0, Functor f1, Functor f2, Functor f3) =>
               (a -> b -> c -> d -> e)
            -> f3 (f2 (f1 (f0 d)))
            -> f3 (f2 (f1 (f0 (a -> b -> c -> e))))
(<~~~$$$$>) f = fmap (f <~~~$$$>)

infixr 8 <~~~$$$$>


(<$$$$$~>) :: (Functor f0, Functor f1, Functor f2, Functor f3, Functor f4) =>
             f4 (f3 (f2 (f1 (f0 (a -> b)))))
          -> a -> f4 (f3 (f2 (f1 (f0 b))))
(<$$$$$~>) f x = fmap (<$$$$~> x) f

infixl 8 <$$$$$~>

(<~$$$$$>) :: (Functor f0, Functor f1, Functor f2, Functor f3, Functor f4) =>
              (a -> b -> c)
           -> f4 (f3 (f2 (f1 (f0 b))))
           -> f4 (f3 (f2 (f1 (f0 (a -> c)))))
(<~$$$$$>) f = fmap (f <~$$$$>)

infixr 8 <~$$$$$>

(<$$$$$~~>) :: (Functor f0, Functor f1, Functor f2, Functor f3, Functor f4) =>
            f4 (f3 (f2 (f1 (f0 (a -> b -> c)))))
         -> b -> f4 (f3 (f2 (f1 (f0 (a -> c)))))
(<$$$$$~~>) f x = fmap (<$$$$~~> x) f

infixl 8 <$$$$$~~>

(<~~$$$$$>) :: (Functor f0, Functor f1, Functor f2, Functor f3, Functor f4) =>
               (a -> b -> c -> d)
            -> f4 (f3 (f2 (f1 (f0 c))))
            -> f4 (f3 (f2 (f1 (f0 (a -> b -> d)))))
(<~~$$$$$>) f = fmap (f <~~$$$$>)

infixr 8 <~~$$$$$>

(<$$$$$~~~>) :: (Functor f0, Functor f1, Functor f2, Functor f3, Functor f4) =>
                f4 (f3 (f2 (f1 (f0 (a -> b -> c -> d)))))
             -> c -> f4 (f3 (f2 (f1 (f0 (a -> b -> d)))))
(<$$$$$~~~>) f x = fmap (<$$$$~~~> x) f

infixl 8 <$$$$$~~~>

(<~~~$$$$$>) :: (Functor f0, Functor f1, Functor f2, Functor f3, Functor f4) =>
                (a -> b -> c -> d -> e)
             -> f4 (f3 (f2 (f1 (f0 d))))
             -> f4 (f3 (f2 (f1 (f0 (a -> b -> c -> e)))))
(<~~~$$$$$>) f = fmap (f <~~~$$$$>)

infixr 8 <~~~$$$$$>

-- * Nested Compositon

(<.$>) :: Functor f0 =>
          (b -> c)
       -> f0 (a -> b)
       -> f0 (a -> c)
f <.$> g = fmap (f .) g

infixr 8 <.$>

(<.*$>) :: Functor f0 =>
           (c -> d)
        -> f0 (a -> b -> c)
        -> f0 (a -> b -> d)
f <.*$> g = fmap (f .*) g

infixr 8 <.*$>

-- TODO: Expand
(<*.$>) :: Functor f0 =>
           (a -> c -> d)
        -> f0 (b -> c)
        -> f0 (a -> b -> d)
f <*.$> g = fmap (f *.) g

infixr 8 <*.$>

(<.**$>) :: Functor f0 =>
           (d -> e)
        -> f0 (a -> b -> c -> d)
        -> f0 (a -> b -> c -> e)
f <.**$> g = fmap (f .**) g

infixr 8 <.**$>


(<.$$>) :: (Functor f0, Functor f1) =>
          (b -> c)
       -> f1 (f0 (a -> b))
       -> f1 (f0 (a -> c))
f <.$$> g = (f .) <$$> g

infixr 8 <.$$>

(<.*$$>) :: (Functor f0, Functor f1) =>
           (c -> d)
        -> f1 (f0 (a -> b -> c))
        -> f1 (f0 (a -> b -> d))
f <.*$$> g = (f .*) <$$> g

infixr 8 <.*$$>

(<.**$$>) :: (Functor f0, Functor f1) =>
           (d -> e)
        -> f1 (f0 (a -> b -> c -> d))
        -> f1 (f0 (a -> b -> c -> e))
f <.**$$> g = (f .**) <$$> g

infixr 8 <.**$$>


(<.$$$>) :: (Functor f0, Functor f1, Functor f2) =>
          (b -> c)
       -> f2 (f1 (f0 (a -> b)))
       -> f2 (f1 (f0 (a -> c)))
f <.$$$> g = (f .) <$$$> g

infixr 8 <.$$$>

(<.*$$$>) :: (Functor f0, Functor f1, Functor f2) =>
           (c -> d)
        -> f2 (f1 (f0 (a -> b -> c)))
        -> f2 (f1 (f0 (a -> b -> d)))
f <.*$$$> g = (f .*) <$$$> g

infixr 8 <.*$$$>

(<.**$$$>) :: (Functor f0, Functor f1, Functor f2) =>
           (d -> e)
        -> f2 (f1 (f0 (a -> b -> c -> d)))
        -> f2 (f1 (f0 (a -> b -> c -> e)))
f <.**$$$> g = (f .**) <$$$> g

infixr 8 <.**$$$>


(<.$$$$>) :: (Functor f0, Functor f1, Functor f2, Functor f3) =>
          (b -> c)
       -> f3 (f2 (f1 (f0 (a -> b))))
       -> f3 (f2 (f1 (f0 (a -> c))))
f <.$$$$> g = (f .) <$$$$> g

infixr 8 <.$$$$>

(<.*$$$$>) :: (Functor f0, Functor f1, Functor f2, Functor f3) =>
           (c -> d)
        -> f3 (f2 (f1 (f0 (a -> b -> c))))
        -> f3 (f2 (f1 (f0 (a -> b -> d))))
f <.*$$$$> g = (f .*) <$$$$> g

infixr 8 <.*$$$$>

(<.**$$$$>) :: (Functor f0, Functor f1, Functor f2, Functor f3) =>
           (d -> e)
        -> f3 (f2 (f1 (f0 (a -> b -> c -> d))))
        -> f3 (f2 (f1 (f0 (a -> b -> c -> e))))
f <.**$$$$> g = (f .**) <$$$$> g

infixr 8 <.**$$$$>
