{- |
Caution:

- Although this module calls 'unsafeInterleaveIO' for you,
  it cannot take the responsibility from you.
  Using this module is still as unsafe as calling 'unsafeInterleaveIO' manually.
  Thus we recommend to wrap the lazy I/O monad
  into a custom @newtype@ with a restricted set of operations
  which is considered safe for interleaving I/O actions.

- Operations like 'System.IO.hClose' are usually not safe within this monad,
  since they will only get executed, if their result is consumed.
  Since this result is often @()@ this is quite unusual.
  It will also often be the case, that not the complete output is read,
  and thus the closing action is never reached.
  It is certainly best to call a closing action after you wrote
  the complete result of the lazy I/O monad somewhere.

- @return a :: LazyIO a@ is very different
  from @interleave (return a) :: LazyIO a@.
  The first one does not trigger previous IO actions,
  whereas the second one does.
  This is the reason why we do not instantiate @MonadIO@
  with @liftIO = LazyIO.interleave@,
  despite the matching type signatures.
  One of the @MonadIO@ laws explicitly requires @return a = liftIO (return a)@.

- We advise to lift strict IO functions into the lazy IO monad.
  Lifting a function like @readFile@ may lead to unintended interleaving.
  In the future we may enforce that using the @deepseq@ package.

Use it like

> import qualified System.IO.Lazy as LazyIO
>
> LazyIO.run $ do
>    LazyIO.interleave $ putStr "enter first line:"
>    x <- LazyIO.interleave getLine
>    LazyIO.interleave $ putStr "enter second line:"
>    y <- LazyIO.interleave getLine
>    return x

Because only the first entered line is needed,
only the first prompt and the first 'getLine' is executed.
-}
module System.IO.Lazy (
   T,
   run,
   interleave,
   ) where

import Control.Monad.Trans.State (StateT(StateT), mapStateT, evalStateT, {- runStateT, -} )
import Control.Monad (ap, {- liftM2, -} )
import Control.Applicative (Applicative(pure, (<*>)), )
import qualified System.Unsafe as Unsafe


newtype T a = Cons {decons :: StateT RunAll IO a}

data RunAll = RunAll
   deriving Show

instance Monad T where
   return x = Cons $ return x
   x >>= f = Cons $
      mapStateT Unsafe.interleaveIO . decons . f =<<
      mapStateT Unsafe.interleaveIO (decons x)

instance Functor T where
   fmap f = Cons . fmap f . decons

instance Applicative T where
   pure = return
   (<*>) = ap

interleave :: IO a -> T a
interleave m = Cons $ StateT $ \RunAll -> fmap (\x->(x,RunAll)) m

run :: T a -> IO a
run =
   flip evalStateT RunAll . decons

{-
correct:
run $ do x <- interleave getLine; y <- interleave getLine; a <- return (x,y); return (fst a)

*LazyIO> run (Control.Monad.replicateM 5 (interleave getChar)) >>= putStrLn
0011223344

*LazyIO> run (interleave (putStrLn "bla") >> interleave getLine) >>= print
"bla
1
1"

*LazyIO> run $ Monad.liftM (\ ((a,b),(c,d))->b) $ liftM2 (,) (liftM2 (,) (interleave getLine) (interleave getLine)) (liftM2 (,) (interleave getLine) (interleave getLine))
"1
2
2"
-}

{-
testLazy, testStrict :: IO String
testLazy   = run $ liftM2 (const)      (interleave getLine) (interleave getLine)
testStrict = run $ liftM2 (flip const) (interleave getLine) (interleave getLine)

test :: IO (String, RunAll)
test = flip runStateT RunAll $ decons $
   interleave getLine >>= \x ->
   interleave getLine >>
   return x
-}
