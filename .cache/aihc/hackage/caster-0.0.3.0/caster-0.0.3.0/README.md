# caster
caster is a multicast, thread-safe and fast logger.
It's convenient when log towards multiple outputs, 
for example, when you want to log to stdout and files same time.
It's possible to change format and log level each outputs.
Also, it converts text-like values and Show instances into log messages automatically.

## Usage

First, the logger requires concurrency.

```haskell
import Control.Concurrent
```

Then, prepare to use the logger. 
In following code, make `LogQueue`, `LogChan` and `Listener`,
start threads which relay messages from `LogChan` to the outputs 
and one which broadcasts messages to `LogChan`.
Be careful not to change the order of `relayLog` and `broadcastLog`.
It causes that messages at the first are not logged correctly.

```haskell
import System.IO
import System.Log.Caster

main = do
  chan <- newLogChan
  lq <- newLogQueue

  -- log to a file..
  handle <- openFile "./foo.log" "caster_test_0.log"
  let FileListener = handleListener defaultFormatter handle
  _ <- forkIO $ relayLog chan LogDebug fileListener

  -- log to stdout.
  _ <- forkIO $ relayLog chan LogInfo stdoutListener

  -- start to broadcast.
  _ <- forkIO $ broadcastLog lq chan
  
  -- give the LogQueue to the function which you want to log in.
  someFunc lq

```

Last, push a message into the `LogQueue` with given functions in order to log.

``` haskell

someFunc :: LogQueue -> IO ()
someFunc lq = do

  -- log levels are matched to syslog.
  -- if you use OverloadedStrings, need to add type annotation.
  debug lq ("debug message" :: String)
  
  -- or you can use helper operator or helper function.
  info lq $: "info messages"
  info lq $ fix  "info messages"
  
  -- if the type is obviously, need not to annotate.
  let msg = Data.Text.pack "notice message"
  notice lq msg
  
  -- it's possible to give incetanse of Show.
  warn lq True
  
  -- there is the useful operator which concatrates text-like values.
  -- the values are converted into byte string as utf-8.
  let text = "foo" :: Data.Text.Text
  let bs = "bar" :: Data.ByteString.Lazy.ByteString
  err lq $ text <:> bs <:> fix "baz"
  
```

The logger requires to be given `LogQueue`, 
so functions which log need to take `LogQueue` as a parameter. 
You can inject `LogQueue` with Reader Monad and so on.
`System.Log.Caster.Monad` provides `MonadCaster` for ReaderT patteren.
Another way, you can use [Data\.Reflection](https://hackage.haskell.org/package/reflection).


```haskell
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}

import Control.Concurrent
import Control.Monad
import Data.Reflection
import System.Log.Caster

type UseLogger = Given LogQueue

useLogger :: UseLogger => LogQueue
useLogger = given

someIO :: UseLogger => IO ()
someIO = do
  critical useLogger "critical message"
  alert useLogger "alert message"
  emergency useLogger "..."
  
inject :: (UseLogger => a) -> IO a
inject io = do
  lq <- newLogQueue
  give io lq
  
main :: IO ()
main = do
  chan <- newLogChan
  _ <- forkIO $ relayLog chan LogDebug stdoutListener
  join $ inject someIO
  
```
