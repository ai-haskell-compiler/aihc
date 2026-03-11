-- | Run 'Memoizer's on 'Memoizable' functions.
-- The beauty of 'runMemo' is that it decouples
-- the definition of a Memoizable function
-- from the process of actually memoizing it.
module Data.RunMemo (
    Memoizable
  , Memoizer
  , runMemo
  , noMemo
  ) where


-- | A memoizable thing takes itself as input
-- and produces itself.
-- 
-- Usually you will use this for functions:
-- e.g. @foo :: Memoizable (String -> String)@,
-- which desugars to @foo :: (String -> String) -> String -> String@
type Memoizable a = a -> a


-- | A memoizer from a to b
-- takes a function with input a and output b
-- and memoizes it
-- 
-- If you have a @Memo Foo@ from @Data.MemoCombinators@,
-- then it is also a @Memoizer Foo b@, which can unify
-- with any type @b@.
type Memoizer a b = (a -> b) -> a -> b


-- | Given a memoizable function and a memoizer,
-- put two and two together!
-- 
-- Your memoizable should look something like this:
-- 
-- > foo :: Memoizable (Foo -> Bar)
-- > foo self = go
-- >   where go x = ... self a ...
-- 
-- The main feature is that @self@ is the first input
-- of a @Memoizable@ function,
-- @self@ and is used for all recursive calls.
-- 
-- Memoizables can take as many arguments as you like,
-- given an appropriate Memoizer
-- 
-- > foo2 :: Memoizable (Bar -> Baz -> Quux)
-- > foo2 self = go
-- >   where go x y = ... self a b ...
-- 
-- Using @Data.MemoCombinators@, for example,
-- you could do @runMemo (Memo.memo2 Memo.bar Memo.baz) foo2@
runMemo :: Memoizer a b -> Memoizable (a -> b) -> a -> b
runMemo memo f = fix (f . memo)
  where fix h = let x = h x in x
        (g . h) x = g (h x)


-- | The trivial memoizer.
-- It doesn't actually memoize anything,
-- it just passes values straight through
-- to the original function.
-- 
-- It is not recommended that you actually use this memoizer.
noMemo :: Memoizer a b
noMemo f = f
