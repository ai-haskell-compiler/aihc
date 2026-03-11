
# chan

This is just some extra `Chan` and `TChan` kit that might help the average user. It relies on
spawning threads with `async` and either canceling (debouncing) or waiting (throttling) messages.

Unfortunately, the current design is slightly untyped in the sense that the channel which you supply is the
_output_ channel, and the returned channel is the one you would write to. I'm not sure how this should
be fixed. There's a series of "typed" alternatives to each underlying channel type, that allows users to
restrict them to "read-only", "write-only", or "Writable a => ..." etc.

An example might be the following:

```haskell
import Control.Concurrent.Chan (readChan)
import Control.Concurrent.Chan.Scope (Scope (..))
import Control.Concurrent.Chan.Typed (ChanRW)
import Control.Concurrent.Chan.Extra (allowReading, throttleStatic, intersperseStatic)



-- For example, some websockets:

data SomeMessage
  = Ping
  | Foo
  -- | ...

throttleLayer :: ChanRW 'Read SomeMessage -> IO (ChanRW 'Write SomeMessage)
throttleLayer output = do
  (x,_) <- throttleStatic output 1000000 -- nanoseconds, = 1 second
  pure x

pingLayer :: ChanRW 'Read SomeMessage -> IO (ChanRW 'Write SomeMessage)
pingLayer output = do
  (x,_,_) <- intersperseStatic output (pure Ping) 1000000
  pure x

performWebsocket :: ChanRW 'Read SomeMessage -> IO ()
performWebsocket output = do
  sendable <- pingLayer . allowReading =<< throttleLayer output
  emittingThread <- async $ forever $ do
    -- the thread that actually facilitates the sending of messages queued in the chans
    msg <- readChanRW output
    send msg -- something like that - it'll include Ping messages for us,
             -- and throttle the outgoing messages at the same time.
  writeChanRW sendable Foo
```
