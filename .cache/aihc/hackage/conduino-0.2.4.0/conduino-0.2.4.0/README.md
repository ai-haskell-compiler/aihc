# conduino

A lightweight continuation-based stream processing library.

It is similar in nature to pipes and conduit, but useful if you just want
something quick to manage composable stream processing without focus on IO.

## Why a stream processing library?

A stream processing library is a way to stream processors in a *composable* way:
instead of defining your entire stream processing function as a single
recursive loop with some global state, instead think about each "stage" of the process,
and isolate each state to its own segment.  Each component can contain its own
isolated state:

```haskell
runPipePure $ sourceList [1..10]
           .| scan (+) 0
           .| sinkList
-- [1,3,6,10,15,21,28,36,45,55]
```

All of these components have internal "state":

*   `sourceList` keeps track of "which" item in the list to yield next
*   `scan` keeps track of the current running sum
*   `sinkList` keeps track of all items that have been seen so far, as a list

They all work together without knowing any other component's internal state, so
you can write your total streaming function without concerning yourself, at
each stage, with the entire part.

In addition, there are useful functions to "combine" stream processors:

*   `zipSink` combines sinks in an "and" sort of way: combine two sinks in
    parallel and finish when all finish.
*   `altSink` combines sinks in an "or" sort of way: combine two sinks in
    parallel and finish when any of them finish
*   `zipSource` combines sources in parallel and collate their outputs.

Stream processing libraries are also useful for streaming composition of
monadic effects (like IO or State), as well.

## Details and usage

API-wise, is closer to *conduit* than *pipes*.  Pull-based, where the main
"running" function is:

```haskell
runPipe :: Pipe () Void u m a -> m a
```

That is, the "production" and "consumption" is integrated into one single pipe,
and then run all at once.  Contrast this to *pipes*, where consumption is not
integrated into the pipe, but rather your choice of "runner" determines how
your pipe is consumed.

One extra advantage over *conduit* is that we have the ability to model pipes
that will never stop producing output, so we can have an `await` function that
can reliably fetch items upstream.  This matches more *pipes*-style requests.

For a `Pipe i o u m a`, you have:

*    `i`: Type of input stream (the things you can `await`)
*    `o`: Type of output stream (the things you `yield`)
*    `u`: Type of the *result* of the upstream pipe (Outputted when upstream
     pipe terminates)
*    `m`: Underlying monad (the things you can `lift`)
*    `a`: Result type when pipe terminates (outputted when finished, with
     `pure` or `return`)

Some specializations:

*   If `i` is `()`, the pipe is a *source* --- it doesn't need anything to
    produce items.  It will pump out items on its own, for pipes downstream to
    receive and process.

*   If `o` is `Void`, the pipe is a *sink* --- it will never `yield` anything
    downstream.  It will consume items from things upstream, and produce a
    result (`a`) if and when it terminates.

*   If `u` is `Void`, then the pipe's upstream is limitless, and never
    terminates.  This means that you can use `awaitSurely` instead of `await`,
    to get await a value that is guaranteed to come.  You'll get an `i` instead
    of a `Maybe i`.

    ```haskell
    await       :: Pipe i o u m (Maybe i)
    awaitsurely :: Pipe i o Void m i
    ```

*   If `a` is `Void`, then the pipe never terminates --- it will keep on
    consuming and/or producing values forever.  If this is a sink, it means
    that the sink will never terminate, and so `runPipe` will also never
    terminate. If it is a source, it means that if you chain something
    downstream with `.|`, that downstream pipe can use `awaitSurely` to
    guarantee something being passed down.

Usually you would use it by chaining together pipes with `.|` and then running
the result with `runPipe`.

```haskell
runPipe $ someSource
       .| somePipe
       .| someOtherPipe
       .| someSink
```


## Why does this package exist?

This package is taking some code I've used some closed-source projects and
pulling it out as a full library.  I wrote it, despite the existence of *pipes*
and *conduit*, because:

1.  I wanted conduit-style semantics for stream composition (source - producer -
    sink all in one package).
2.  I wanted type-enforced guaranteed "awaits" based on type-enforced
    guaranteed infinite producers.
3.  I wanted to be able to combine stream processors "in parallel" in
    different ways (`zipSink`, for "and", and `altSink`, for "or").
3.  I wanted something lightweight without the dependencies dealing with IO,
    since I wasn't really doing resource-sensitive IO.

*conduino* is a small, lightweight version that is focused not necessarily on
"effects" streaming, but rather on composable bits of logic.  It is basically a
lightweight version of conduit-style streaming.  It is slightly different from
pipes in terms of API.

One major difference from *conduit* is the `u` parameter, which allows for
things like `awaitSurely`, to ensure that upstream pipes will never terminate.

If you need to do some important IO and handle things like managing resources,
or leverage interoperability with existing libraries...switch to a more mature
library like *conduit* or *pipes* immediately :)
