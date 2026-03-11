[![img](https://img.shields.io/hackage/v/box.svg)](https://hackage.haskell.org/package/box) [![img](https://github.com/tonyday567/box/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/tonyday567/box/actions/workflows/haskell-ci.yml)

A profunctor effect system.

> What is all this stuff around me; this stream of experiences that I seem to be having all the time? Throughout history there have been people who say it is all illusion. ~ S Blackmore


# Usage

    :set -XOverloadedStrings
    import Box
    import Prelude
    import Data.Function
    import Data.Bool

Standard IO echoing:

    echoC = Committer (\s -> putStrLn ("echo: " <> s) >> pure True)
    echoE = Emitter (getLine & fmap (\x -> bool (Just x) Nothing (x =="quit")))
    glue echoC echoE

    hello
    echo: hello
    echo
    echo: echo
    quit

Committing to a list:

    > toListM echoE
    hello
    echo
    quit
    ["hello","echo"]

Emitting from a list:

    > glue echoC <$|> witherE (\x -> bool (pure (Just x)) (pure Nothing) (x=="quit")) <$> (qList ["hello", "echo", "quit"])
    echo: hello
    echo: echo


# Library Design


### Resource Coinduction

Haskell has an affinity with [coinductive functions](https://www.reddit.com/r/haskell/comments/j3kbge/comment/g7foelq/?utm_source=share&utm_medium=web2x&context=3); functions should expose destructors and allow for infinite data.

The key text, [Why Functional Programming Matters](https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf), details how producers and consumers can be separated by exploiting laziness, creating a speration of concern not available in other technologies. Utilising laziness, we can peel off (destruct) the next element of a list to be consumed without disturbing the pipeline of computations that is still to occur, for the cost of a thunk.

So how do you apply this to resources and their effects? One answer is that you destruct a (potentially long-lived) resource simply by using it. For example, reading and writing lines to standard IO:

    :t getLine
    :t putStrLn

    getLine :: IO String
    putStrLn :: String -> IO ()

These are the destructors that need to be transparently exposed if effects are to be good citizens in Haskell.


### What is a Box?

A Box is simply the product of a consumer destructor and a producer destructor.

    data Box m c e = Box
      { committer :: Committer m c,
        emitter :: Emitter m e
      }


### Committer

The library denotes a consumer by wrapping a consumption destructor and calling it a Committer. Like much of base, there is failure hidden in the getLine example type. A better approach, for a consumer, is to signal whether consumption actually occurred.

    newtype Committer m a = Committer
      { commit :: a -> m Bool
      }

You give a Committer an &rsquo;a&rsquo;, and the destructor tells you whether the consumption of the &rsquo;a&rsquo; was successful or not. A standard output committer is then:

    stdC :: Committer IO String
    stdC = Committer (\s -> putStrLn s >> pure True)

    <interactive>:19:1-4: warning: [GHC-63397] [-Wname-shadowing]
        This binding for ‘stdC’ shadows the existing binding
          defined at <interactive>:16:1

A Committer is a contravariant functor, so contramap can be used to modify this:

    import Data.Text as Text
    import Data.Functor.Contravariant
    
    echoC :: Committer IO Text
    echoC = contramap (Text.unpack . ("echo: "<>)) stdC


### Emitter

The library denotes a producer by wrapping a production destructor and calling it an Emitter.

    newtype Emitter m a = Emitter
      { emit :: m (Maybe a)
      }

An emitter returns an &rsquo;a&rsquo; on demand or not.

    stdE :: Emitter IO String
    stdE = Emitter (Just <$> getLine)

As a functor instance, an Emitter can be modified with fmap. Several library functions, such as witherE and filterE can also be used to stop emits or add effects.

    echoE :: Emitter IO Text
    echoE =
      witherE (\x -> bool (pure (Just x)) (putStrLn "quitting" *> pure Nothing) (x == "quit"))
        (fmap Text.pack stdE)

    <interactive>:52:1-5: warning: [GHC-63397] [-Wname-shadowing]
        This binding for ‘echoE’ shadows the existing binding
          defined at <interactive>:49:1


### Box duality

A Box represents a duality in two ways:

-   As the consumer and producer sides of a resource. The complete interface to standard IO, for example, could be:

    stdIO :: Box IO String String
    stdIO = Box (Committer (\s -> putStrLn s >> pure True)) (Emitter (Just <$> getLine))

-   As two ends of a computation.

> This is how we can use a profunctor to glue together two categories ~ Milewski
> [Promonads, Arrows, and Einstein Notation for Profunctors](https://bartoszmilewski.com/2019/03/27/promonads-arrows-and-einstein-notation-for-profunctors/)

`glue` is the primitive with which we connect a Committer and Emitter.

    > glue echoC echoE
    hello
    echo: hello
    echo
    echo: echo
    quit
    quitting

Effectively the same computation, for a Box, is:

    fuse (pure . pure) stdIO


### Continuation

As with many operators in the library, `qList` is actually a continuation:

    :t qList

    qList
      :: Control.Monad.Conc.Class.MonadConc m => [a] -> CoEmitter m a

    type CoEmitter m a = Codensity m (Emitter m a)

Effectively being a newtype wrapper around:

    forall x. (Emitter m a -> m x) -> m x

A good background on call-back style programming in Haskell is in the [managed](https://hackage.haskell.org/package/managed-1.0.10/docs/Control-Monad-Managed.html) library, which is a specialised version of Codensity.

Codensity has an Applicative instance, and lends itself to applicative-style coding. To send a (queued) list to stdout, for example, you could say:

    :t glue <$> pure toStdout <*> qList ["a", "b", "c"]

    glue <$> pure toStdout <*> qList ["a", "b", "c"]
      :: Codensity IO (IO ())

and then escape the continuation with:

    runCodensity (glue <$> pure toStdout <*> (qList ["a", "b", "c"])) id

    a
    b
    c

This closes the continuation. The following code is equivalent:

    close $ glue <$> pure toStdout <*> qList ["a", "b", "c"]

    a
    b
    c

    close $ glue toStdout <$> qList ["a", "b", "c"]

    a
    b
    c

Given the ubiquity of this method, the library supplies two applicative style operators that combine application and closure.

-   `(<$|>)` fmap and close over a Codensity:

    glue toStdout <$|> qList ["a", "b", "c"]

    a
    b
    c

-   `(<*|>)` Apply and close over Codensity

    glue <$> pure toStdout <*|> qList ["a", "b", "c"]

    a
    b
    c


# Explicit Continuation

Yield-style streaming libraries are [coroutines](https://rubenpieters.github.io/assets/papers/JFP20-pipes.pdf), sum types that embed and mix continuation logic in with other stuff like effect decontruction. `box` sticks to a corner case of a product type representing a consumer and producer. The major drawback of eschewing coroutines is that continuations become explicit and difficult to hide. One example; taking the first n elements of an Emitter:

    :t takeE
    takeE :: Monad m => Int -> Emitter m a -> Emitter (StateT Int m) a

A disappointing type. The state monad can not be hidden, the running count has to sit somewhere, and so different glueing functions are needed:

    -- | Connect a Stateful emitter to a (non-stateful) committer of the same type, supplying initial state.
    --
    -- >>> glueES 0 (showStdout) <$|> (takeE 2 <$> qList [1..3])
    -- 1
    -- 2
    glueES :: (Monad m) => s -> Committer m a -> Emitter (StateT s m) a -> m ()
    glueES s c e = flip evalStateT s $ glue (foist lift c) e


# Future directions

The design and concepts contained within the box library is a hodge-podge, but an interesting mess, being at quite a busy confluence of recent developments.


## Optics

A Box is an adapter in the [language of optics](http://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/poptics.pdf) and the relationship between a resource&rsquo;s committer and emitter could be modelled by other optics.


## Categorical Profunctor

The deprecation of Box.Functor awaits the development of [categorical functors](https://github.com/haskell/core-libraries-committee/issues/91#issuecomment-1325337471). Similarly to Filterable the type of a Box could be something like `FunctorOf Op(Kleisli Maybe) (Kleisli Maybe) (->)`. Or it could be something like the SISO type in [Programming with Monoidal Profunctors and Semiarrows](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4496714).


## Wider Types

Alternatively, the types could be widened:

    newtype Committer f a = Committer { commit :: a -> f () }
    
    instance Contravariant (Committer f) where
      contramap f (Committer a) = Committer (a . f)
    
    newtype Emitter f a = Emitter { emit :: f a }
    
    instance (Functor f) => Functor (Emitter f) where
      fmap f (Emitter a) = Emitter (fmap f a)
    
    data Box f g b a =
      Box { committer :: Committer g b, emitter :: Emitter f a }
    
    instance (Functor f) => Functor (Box f g b) where
      fmap f (Box c e) = Box c (fmap f e)
    
    instance (Functor f, Contravariant g) => Profunctor (Box f g) where
      dimap f g (Box c e) = Box (contramap f c) (fmap g e)

.. with the existing computations recovered with:

    type CommitterB m a = Committer (MaybeT m) a
    type EmitterB m a = Emitter (MaybeT m) a
    type BoxB m b a = Box (MaybeT m) (MaybeT m) b a


## Introduce a [nucleus](https://golem.ph.utexas.edu/category/2013/08/the_nucleus_of_a_profunctor_so.html)

Alternative to both of these, the Monad constraint could be rethought. There are the ends of the computational pipeline, but there is also the gluing/fusion/middle bit.

    connect :: (f a -> b) -> Committer g b -> Emitter f a -> g ()
    connect w c e = emit e & w & commit c
    
    glue :: Box f g (f a) a -> g ()
    glue (Box c e) = connect id c e
    
    nucleate ::
      Functor f =>
      (f a -> f b) ->
      Committer g b ->
      Emitter f a ->
      f (g ())
    nucleate n c e = emit e & n & fmap (commit c)

This has the nice property that the closure is not hidden (as is usually the case for a Monad constraint) so that, for instance, fusion along longer chains becomes possible.

