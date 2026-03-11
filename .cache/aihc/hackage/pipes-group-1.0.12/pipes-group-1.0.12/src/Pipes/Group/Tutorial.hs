{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-| @pipes-group@ builds upon @pipes@ to establish idioms for grouping streams
    into sub-streams without collecting elements into memory.  This tutorial
    assumes familiarity with @pipes@ and @pipes-parse@.
-}

module Pipes.Group.Tutorial (
    -- * Motivation
    -- $motivation

    -- * FreeT
    -- $freeT

    -- * How FreeT Works
    -- $advanced

    -- * Conclusion
    -- $conclusion
    ) where

import Pipes
import Pipes.Group

{- $motivation
    Dividing a stream into sub-streams is non-trivial.  To illustrate the
    problem, consider the following task: limit a stream to the first three
    groups of elements (a group means consecutive equal elements).

    The wrong way to do it is to read each group into memory like this:

> import Lens.Family.State.Strict (zoom)
> import Pipes
> import Pipes.Parse
> import qualified Pipes.Prelude as P
> 
> threeGroups :: (Monad m, Eq a) => Producer a m () -> Producer a m ()
> threeGroups p0 = loop 3 p0
>   where
>     loop 0 _ = return ()
>     loop n p = do
>         (as, p') <- lift $ runStateT (zoom group drawAll) p
>         each as
>         loop (n - 1) p'

    The first problem is that this approach does not output any elements from
    each group until after parsing the entire group:

>>> runEffect $ threeGroups P.stdinLn >-> P.stdoutLn
1<Enter>
1<Enter>
2<Enter>
1
1
2<Enter>
2<Enter>
3<Enter>
2
2
2
4<Enter>
3
>>>

    Worse, this program will crash without outputting a single value if fed an
    infinitely long group of identical elements:

>>> runEffect $ threeGroups (each (repeat 1)) >-> P.print
<Consumes all memory and crashes>

    A better approach is to just stream directly from the first three groups
    instead of storing the groups in intermediate lists:

> import Lens.Family ((^.))
> import Pipes
> import Pipes.Parse
> import qualified Pipes.Prelude as P
> 
> threeGroups :: (Monad m, Eq a) => Producer a m () -> Producer a m ()
> threeGroups p0 = loop 3 p0
>   where
>     loop 0 _ = return ()
>     loop n p = do
>         p' <- p ^. group
>         loop (n - 1) p'

    This will run in constant memory and stream values immediately:

>>> runEffect $ threeGroups P.stdinLn >-> P.stdoutLn
1<Enter>
1
1<Enter>
1
2<Enter>
2
2<Enter>
2
2<Enter>
2
3<Enter>
3
4<Enter>

    However, this code is not very modular: we have to integrate our group
    creation logic with our group consumption logic.  This conflicts with the
    @pipes@ philosophy of decoupling streaming programs into modular components.

    An more modular approach would be to split our logic into three steps:

    * Split our 'Producer' into groups

    * Take the first three groups

    * Join these three groups back into a 'Producer'

    But how do we split our 'Producer' into groups without loading an entire
    group into memory?  We want to avoid solutions like the following code:

> import Control.Monad (when, liftM2)
> import Lens.Family.State.Strict (zoom)
> import Pipes.Parse
> 
> split :: (Monad m, Eq a) => Producer a m () -> Producer [a] m ()
> split p = do
>     ((as, eof), p') <- lift (runStateT parser p)
>     yield as
>     when (not eof) (split p')
>   where
>     parser = liftM2 (,) (zoom group drawAll) isEndOfInput

    ... because then we're back where we started, loading entire groups into
    memory.
-}

{- $freeT
    Fortunately, you can group elements while still streaming individual
    elements at a time.  The 'FreeT' type from the @free@ package solves this
    problem by allowing us to build \"linked lists\" of 'Producer's.  This lets
    you work with streams in a list-like manner.

    The key idea is that:

> -- '~' means "is analogous to"
>
> -- If a Producer is like a list
> Producer a m ()            ~   [a]
>
> -- ... then a 'FreeT'-delimited 'Producer' is like a list of lists
> FreeT (Producer a m) m ()  ~  [[a]]

    Think of @(FreeT (Producer a m) m ())@ as a \"list of 'Producer's\".
    'FreeT' nests each subsequent 'Producer' within the return value of the
    previous 'Producer' so that you cannot access the next 'Producer' until you
    completely drain the current 'Producer'.  However, you rarely need to work
    with 'FreeT' directly.  Instead, you can structure most things using
    \"splitters\", \"transformations\" and \"joiners\":

> -- A "splitter"
> Producer a m ()           -> FreeT (Producer a m) m ()  ~   [a]  -> [[a]]
>
> -- A "transformation"
> FreeT (Producer a m) m () -> FreeT (Producer a m) m ()  ~  [[a]] -> [[a]]
>
> -- A "joiner"
> FreeT (Producer a m) m () -> Producer a m ()            ~  [[a]] ->  [a]

    An example splitter is @(view groups)@, which splits a 'Producer' into
    'FreeT'-delimited 'Producer's, one for each group of consecutive equal
    elements:

> view groups :: (Eq a, Monad m) => Producer a m x -> FreeT (Producer a m) m x

    An example transformation is @(takes 3)@, which takes the first three
    'Producer's from a 'FreeT' and drops the rest:

> takes 3 :: Monad m => FreeT (Producer a m) m () -> FreeT (Producer a m) m ()

    An example joiner is @concats@, which collapses a 'FreeT' of 'Producer's
    back down into a single 'Producer':

> concats :: Monad m => FreeT (Producer a m) m x -> Producer a m x

    If you compose these three functions together, you will create a function
    that transforms a 'Producer' to keep only the first three groups of
    consecutive equal elements:

> import Lens.Family
> import Pipes
> import Pipes.Group
> import qualified Pipes.Prelude as P
>
> threeGroups :: (Monad m, Eq a) => Producer a m () -> Producer a m ()
> threeGroups = concats . takes 3 . view groups

    Both splitting and joining preserve the streaming nature of 'Producer's and
    do not collect or buffer any values.  The transformed 'Producer' still
    outputs values immediately and does not wait for groups to complete before
    producing results.

>>> runEffect $ threeGroups P.stdinLn >-> P.stdoutLn
1<Enter>
1
1<Enter>
1
2<Enter>
2
2<Enter>
2
2<Enter>
2
3<Enter>
3
4<Enter>
>>>

    Also, lenses simplify things even further.  The reason that 'groups' is a
    lens is because it actually combines both a splitter and joiner into a
    single package.  We can then use 'over' to handle both the splitting and
    joining for us:

>>> runEffect $ over groups (takes 3) P.stdinLn >-> P.stdoutLn
<Exact same behavior>

    This behaves the same because 'over' takes care of calling the splitter
    before applying the transformation, then calling the inverse joiner
    afterward.

    Another useful lens is 'individually', which lets you apply transformations
    to each 'Producer' layer of a 'FreeT'.  For example, if we wanted to
    add an extra @"!"@ line to the end of every group, we would write:

>>> import Control.Applicative ((<*))
>>> runEffect $ over (groups . individually) (<* yield "!") P.stdinLn >-> P.stdoutLn
1<Enter>
1
1<Enter>
1
2<Enter>
!
2
2<Enter>
2
2<Enter>
2
3<Enter>
!
3
4<Enter>
!
>>>

    Note that 'individually' is only compatible with the @lens@ package.  You
    can alternatively use 'maps' if you are using @lens-family-core@:

>>> runEffect $ over groups (maps (<* yield "!")) P.stdinLn >-> P.stdoutLn
<Exact same behavior>

-}

{- $advanced
    You don't necessarily have to restrict yourself to predefined 'FreeT'
    functions.  You can also manually build or recurse over 'FreeT's of
    'Producer's.

    For example, here is how 'concats' is implemented, which collapses all the
    'Producer's within a 'FreeT' into a single 'Producer':

> concats :: Monad m => FreeT (Producer a m) m x -> Producer a m x
> concats = go
>   where
>     go f = do
>         x <- lift (runFreeT f)  -- Match against the "head" of the "list"
>         case x of
>             Pure r -> return r  -- The "list" is empty
>             Free p -> do        -- The "list" is non-empty
>                 f' <- p         -- The return value of the 'Producer' is
>                 go f'           --     the "tail" of the "list"

    Many patterns for 'FreeT's have equivalent analogs for lists.  'runFreeT'
    behaves like pattern matching on the list, except that you have to bind the
    result.  'Pure' is analogous to @[]@ and 'Free' is analogous to @(:)@.

    When you receive a 'Free' constructor that means you have a 'Producer' whose
    return value is the rest of the list (i.e. another 'FreeT').  You cannot
    access the rest of the list without running the 'Producer' to completion to
    retrieve this return value.  The above example just runs the entire
    'Producer', binds the remainder of the list to @f'@ and then recurses on
    that value.

    You can also build 'FreeT's in a manner similar to lists.  For example, the
    'chunksOf' lens uses the following splitter function internally:

> _chunksOf :: Monad m => Producer a m x -> FreeT (Producer a m) m x
> _chunksOf p = FreeT $ do
>     x <- next p                     -- Pattern match on the 'Producer'
>     return $ case x of
>         Left   r      -> Pure r     -- Build an empty "list"
>         Right (a, p') -> Free $ do  -- Build a non-empty "list"
>             p'' <- (yield a >> p')^.splitAt n0  -- Emit the "head"
>             return (_chunksOf p'')              -- Return the "tail"

    'Pure' signifies an empty 'FreeT' (one with no 'Producer' layers), just like
    @[]@ signifies an empty list (one with no elements).  We return 'Pure'
    whenever we cannot emit any more 'Producer's.

    'Free' indicates that we wish to emit a 'Producer' followed by another
    \"list\".  The 'Producer' we run directly within the body of the 'Free'.
    However, we store the remainder of the \"list\" within the return value of
    the 'Producer'.  This is where @_chunksOf@ recurses to build the rest of the
    \"list\".

    To gain a better understanding for how 'FreeT' works, consult the definition
    of the type, which you can find in "Control.Monad.Trans.Free":

> newtype FreeT f m a = FreeT { runFreeT :: m (FreeF f a (FreeT f m a)) }
>
> data FreeF f a b = Pure a | Free (f b)

    ... and just replace all occurrences of @f@ with @(Producer e m)@:

> -- This is pseudocode
>
> newtype FreeT' m a = FreeT { runFreeT :: m (FreeF' a (FreeT' m a)) }
>
> data FreeF' a b = Pure a | Free (Producer e m b)

    ... which you can further think of as:

> -- More pseudocode
>
> newtype FreeT' m a =
>     FreeT { runFreeT :: m (Pure a | Producer e m (FreeT' m a)) }

    In other words, 'runFreeT' unwraps a 'FreeT' to produce an action in the
    base monad which either finishes with a value of type @a@ or continues with
    a 'Producer' which returns a new 'FreeT'.  Vice versa, if you want to build
    a 'FreeT', you must create an action in the base monad which returns either
    a 'Pure' or a 'Producer' wrapping another 'FreeT'.
-}

{- $conclusion
    This library is very small since it only contains element-agnostic grouping
    utilities.  Downstream libraries that provide richer grouping utilities
    include @pipes-bytestring@ and @pipes-text@.

    To learn more about @pipes-group@, ask questions, or follow development, you
    can subscribe to the @haskell-pipes@ mailing list at:

    <https://groups.google.com/forum/#!forum/haskell-pipes>

    ... or you can mail the list directly at:

    <mailto:haskell-pipes@googlegroups.com>
-}
