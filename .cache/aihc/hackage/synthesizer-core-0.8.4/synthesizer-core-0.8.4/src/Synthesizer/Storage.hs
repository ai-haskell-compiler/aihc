{- |
Rendering sound effects off-line has its virtue,
but really cool is real-time signal generation.
For a long time I thought that it is the compiler's responsibility
to make list based signal processing fast enough.
However, the compiler has to respect correctness first.
That is, it cannot do too fancy optimization,
since the optimized program must still do the same as the unoptimized program.
So, when we write functions that rely on the maximal flexibility,
the compiler cannot turn it to something less flexible.
Actually, a list as in "Synthesizer.Plain.Signal"
is the best representation of a signal
in terms of flexibility:
It allows free choice of the element type, even functions,
it is element-wise lazy, allowing for short feedback,
it allows sharing of computed data.
The drawback is, that it is slow and memory inefficient.
In most cases we don't need this full flexibility,
but the compiler has no chance to find this out automatically.
It can merge several operations on a list
to a single operation by the fusion technique,
however even a single list operation is hard to perform in real-time.

How do real-time software synthesizer achieve real-time performance?
They get the popular fast inner loops
by processing signals in chunks of raw data.
This way, they lose flexibility, because they cannot do quick feedback.
We can do the same in Haskell, getting the same restrictions.
Additionally, in order to store raw data
we must restrict the element types
e.g. to the @Storable@ class,
since we use @StorableVector@ in "Synthesizer.Storable.Signal".
With this technique single signal operations are fast,
but their combination cannot be optimized in many cases.
This is so, again, because top priority in optimization is correctness.
Consider @mix x (cons 0 x)@
where @cons 0 x@ means @0:x@ for our chunky signal data.
This expression is a perfect candidate for optimization.
But in this case it must not be applied since the chunk structures of
@x@ and @cons 0 x@ do not match.
In such cases we would not gain anything over SuperCollider and CSound.

Remember that we introduced the chunky signal representation
entirely because of efficiency concerns.
Actually we are not interested in a special chunk structure,
so this should not be a reason for disabling optimization.
Of course, we could ignore the correctness
and write incorrect optimizer rules
that are based on correct ideas.
However, experience shows that wrong optimization
leads to surprise and infelicities sooner or later.
The later the worse,
because the later the more code you have written
relying on invalid optimization.

What we can try is to use list representation,
enjoy the optimization that GHC already provides for it,
and then let fusion rules jump in
that make lists disappear when they are used in connection with chunky sequences.
E.g. @Chunky.fromList (List.oscillator freq)@
could be turned into @Chunky.oscillator freq@.
This approach would be really cool, but works only in theory.
In practice it is hard to predict how GHC transforms various operations.
Additionally to optimizer rule application
it also expands functions to their definitions (known as inlining/unfolding)
or specializes functions to fixed types.
We cannot rely on our optimizer rules being actually applied.
This means however, that in unpredictable cases
the optimization fails and the efficiency drops from real-time to non-real-time.
This is unacceptable.

The solution is a third signal representation,
see "Synthesizer.State.Signal".
(Already got tired?)
It consists of no actual data
but it is a function that generates elements.
Its type is @s -> Maybe (a,s)@ or short @StateT s Maybe a@.
Given a state of type @s@ it produces @Nothing@ when the list terminates
or @Just@ the next element and the updated state.
This can be easily converted from and to lists
while preserving laziness.
We convert to lists by @List.unfoldr@ and from lists using @viewL@.
Actually this signal representation is very close
to the list representation used in the streams package.
The main differences are:
Firstly, we do not use a list storage that is fused away when only used temporarily.
Thus we do not need a fusion rule (that could be skipped by the compiler).
Secondly, we have no notion of 'Skip',
since operations like 'filter' are uncommon in signal processing.
If we write our signal processing in terms of these virtual signals
and then convert the result to regular lists or chunky sequences,
then only one data structure will be built
and GHC does it's best to generate efficient inner loops.

We cannot use these virtual signals for sharing and feedback,
because there is no data structure that stores the data.
If we try to do so anyway, data will be recomputed.
Thus we still need chunky sequences or lists
for sharing of interim results and for feedback.
Actually, an expression like @mix x (reverse x)@
would definitely benefit from interim conversion to a chunky sequence,
but for @mix x (cons 0 x)@ this is overkill.

In order to get processes like the last one efficient
we have a new data type (no, not another one!)
but this time it is not a signal data type
but a signal processor type.
It is the result of thinking about
which processes allow sharing on a per-sample basis at all.
We come to the conclusion that these can be only causal processes,
i.e. processes that depend only on current and past data,
not on data from the future.
So, we already have a good name: "Synthesizer.Causal.Process".
Causal processes are "Control.Arrow"s,
however the higher level variant does no longer fit into the Arrow type class.
This means that there are various combinations
that turn causal processes into larger causal processes.
It needs a bit experience in pointfree coding style
in order to use the arrow combinators,
but there is no way around it,
when you want to use physical dimensions.
GHC's arrow notation does only support types of the Arrow class.
E.g. the expression @mix x (cons 0 x)@
becomes @Causal.mix <<< (Causal.id &&& Causal.cons 0)@.
When you manage this burden
you get processes that are warranted to be causal.
They can not only be used to make something efficient,
but they also allow to process data from the outside world
in a streaming way without 'unsafeInterleaveIO'
as required e.g. in JACK plugins.

We have now a pretty big set of signal storage types
that differ considerably in performance
but not in the set of operations.
This calls for a type class!
You find it in "Synthesizer.Generic.Cut"
and "Synthesizer.Generic.Signal".
-}
module Synthesizer.Storage where
