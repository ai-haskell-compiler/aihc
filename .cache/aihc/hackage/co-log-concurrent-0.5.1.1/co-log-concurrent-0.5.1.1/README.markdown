[![Hackage](https://img.shields.io/hackage/v/co-log-concurrent.svg)](https://hackage.haskell.org/package/co-log-concurrent)
[![Stackage LTS](http://stackage.org/package/co-log-concurrent/badge/lts)](http://stackage.org/lts/package/co-log-concurrent)
[![Stackage Nightly](http://stackage.org/package/co-log-concurrent/badge/nightly)](http://stackage.org/nightly/package/co-log-concurrent)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/qnikst/co-log-concurrent/blob/master/LICENSE)
[![Build status](https://secure.travis-ci.org/qnikst/co-log-concurrent.svg)](https://travis-ci.org/qnikst/co-log-concurrent)

# co-log-concurrent

`co-log-concurrent` is an asynchronous backend for the [co-log](https://hackage.haskell.org/package/co-log) library.
The core idea of co-log-concurrent is that you may easily make your logs asynchronous by adding a few
lines of code to the program that is using `co-log`. But we design this library in a way that you can easily inject it into your logs pipeline, in case if you need some particular functionality, and the library does not prevent you from doing so.

## When do you need co-log-concurrent?

In some applications storing logs may become a bottleneck, it happens because if logs are synchronous, then before writing a log thread must take a lock on the resource. It means that we serialize the processing of the messages and introduce additional contention. Asynchronous logging can improve the situation, a thread emits logs and knows that it logs are written at some future point of time. 

There are a few kinds of applications that may benefit from asynchronous logs writing:
CPU intensive applications
Web services
In general asynchronous logging has some downsides.
  1. _Unbounded memory usage_ - if there is no backpressure mechanism the user threads, threads may generate more logs that can we can store at the same amount of time. In such cases messages are accumulated in memory. It extends GC times and memory usage.	
  2. _Persistence requirements_ - sometimes application may want to ensure that we persisted the logs before it moved to the next statement. It is not a case with concurrent log systems in general; some we lose logs even the thread moves forward. It may happen when the application exits before dumping all logs.
  3. _Non-precise logging_ - sometimes there may be anomalies when storing logs, such as logs reordering or imprecise timestamps.

`co-log-concurrent` provides a framework that you can use to have precisely the properties you need. But you still need to carefully think if a violation of the properties may harm your application.

## How to use?

For a general description of the co-log framework refer to the [co-log documentation](https://github.com/kowainik/co-log/blob/master/co-log/tutorials/1-intro/Intro.md)
it's always up to date with the latest library version. In this tutorial, we concentrate on the co-log-concurrent alone.

### Simple case

You should use simple API if you don't need anything special and want the library to work. Simple API provides the following defaults:
There is a backpressure mechanism: a thread is blocked if there are too many pending messages.
Messages event from the different threads is never reordered.
Messages may be lost if the program is abnormally killed (e.g. using sigKILL), but the library does it's best effort to dump all logs in all other cases.

To use simple API, you should wrap your program with `withBackgroundWorker` function. It takes the following parameters:

```haskell
withBackgroundLogger
   (defCapacity :: Capacity)
   (logDumper :: LoggerAction IO a)
   (program :: IO a)
```
where `defCapacity` is the size of the buffer for not yet stored messages, it acts as a backpressure mechanism protecting your program from memory overflows.
`logDumper` an action to store logs, you can use simple `Colog.IO.logStringStdout` or any other `Logger` function `program` is your ordinary program. So the skeleton for your application may look like:

```haskell
-- usual imports:
import Colog.Actions
import Colog.Monad

import Colog.Concurrent -- import of the library

main :: IO ()
main =
  withBackgroundLogger
    defCapacity
    (pure ())
    logByteStringStdout $ \log ->
      usingLoggerT log $ do
        logMsg @ByteString "Starting application..."
        ...
        logMsg @ByteString "Finishing application..."
```

This approach is enough for most of the cases, and we try to keep this API fast and safe.

### Advanced usage

Sometime your application may need some advanced features, in such a case, you need to know how to use `co-log-concurrent` for constructing log pipelines. Please refer to the [haddocks](https://hackage.haskell.org/package/co-log-concurrent) for that. 

## Other implementations.

Comparison with other libraries may be outdated, as other libraries are improving and may
have solved the issued described below.

 * [fast-logger](https://hackage.haskell.org/package/fast-logger) — fast logger implements concurrent approach to storing logs, it's very effective. It has a buffer for each capability that each thread can append messages to the buffer without locking. However, that approach comes at some costs, in pathological cases runtime may reschedule a thread to another capability and logs from that thread may be reordered. Even if a case is pathological, it happens in real-world applications. Current `co-log-concurrent` implementation guarantees no logs reordering by the cost of using `STM` channels.
   Another scenario that may have downsides of using `fast-logger` is that it takes over a timer control.  Library uses [auto-update]() package that updates timer every second, to reduce syscall count, it means that precision of the timer is 1s, that may not be suitable for some applications. `co-log-concurrent` is abstracted over the timer so that user can use any approach that suits it well, including not to give any timestamp to the message at all.

 * [katip](https://hackage.haskell.org/package/katip) — katip uses the approach that is very similar to `co-log-concurrent` and has the same runtime properties. However `co-log-concurrent` allows to use any messages, timers and doesn't restrict the user to use particular ones. It makes co-log-concurrent more flexible.
