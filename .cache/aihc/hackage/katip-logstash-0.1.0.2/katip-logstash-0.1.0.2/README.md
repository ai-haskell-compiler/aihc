# Haskell client library for Logstash

![MIT](https://img.shields.io/github/license/mbg/logstash)
![CI](https://github.com/mbg/logstash/workflows/Build/badge.svg?branch=master)
![stackage-nightly](https://github.com/mbg/logstash/workflows/stackage-nightly/badge.svg)
[![logstash](https://img.shields.io/hackage/v/logstash)](https://hackage.haskell.org/package/logstash)

This library implements a client for Logstash in Haskell. The following features are currently supported:

- Connections to Logstash via TCP or TLS (`tcp` input type).
- Support for the `json_lines` codec out of the box and custom codecs can be implemented (arbitrary `ByteString` data can be sent across the connections).
- This library can either be used without any logging framework, as a backend for [`monad-logger`](http://hackage.haskell.org/package/monad-logger/), or as a backend for [`katip`](http://hackage.haskell.org/package/katip/).
- Log messages can either be written synchronously or asynchronously.

For example, to connect to a Logstash server via TCP at `127.0.0.1:5000` (configuration given by `def`) and send a JSON document synchronously with a timeout of 1s and the default retry policy from [`Control.Retry`](https://hackage.haskell.org/package/retry/docs/Control-Retry.html):

```haskell
data Doc = Doc String

instance ToJSON Doc where
    toJSON (Doc msg) = object [ "message" .= msg ]

main :: IO ()
main = runLogstashConn (logstashTcp def) retryPolicyDefault 1000000 $
    stashJsonLine (Doc "Hello World")
```

Only the `tcp` input type (with or without TLS) is currently supported. For example, without TLS, the Logstash input configuration should roughly be:

```conf
input {
    tcp {
        port => 5000
        codec => "json_lines"
    }
}
```

With TLS, the expected Logstash configuration should roughly be:

```conf
input {
    tcp {
        port => 5000
        ssl_cert => "/usr/share/logstash/tls/cert.pem"
        ssl_key => "/usr/share/logstash/tls/key.pem"
        ssl_key_passphrase => "foobar"
        ssl_enable => true 
        ssl_verify => false
        codec => "json_lines"
    }
}
```

## Configuring connections

Connections to Logstash are represented by the `LogstashConnection` type. To connect to Logstash via `tcp` use the `Logstash.TCP` module which exports four principal functions. Note that none of these functions establish any connections when they are called - instead, they allow `runLogstashConn` and `runLogstashPool` to establish connections/reuse them as needed:

- `logstashTcp` which, given a hostname and a port, will produce an `Acquire` that can be used with `runLogstashConn`.
- `logstashTcpPool` which, given a hostname and a port, will produce a `Pool` that can be used with `runLogstashPool`.
- `logstashTls` which, given a hostname, a port, and TLS client parameters, will produce an `Acquire` that can be used with `runLogstashConn`.
- `logstashTlsPool` which, given a hostname, a port, and TLS client parameters, will produce a `Pool` that can be used with `runLogstashPool`.

For `logstashTls` and `logstashTlsPool`, TLS `ClientParams` are required. It is worth noting that the `defaultParamsClient` function in the `tls` package does **not** set any supported ciphers and does **not** load the system trust store by default. For relatively sane defaults, it is worth using `newDefaultClientParams` from [`network-simple-tls`](http://hackage.haskell.org/package/network-simple-tls/) instead. For example:

```haskell
main :: IO ()
main = do 
    params <- newDefaultClientParams ("127.0.0.1", "")

    runLogstashConn (logstashTls def params) retryPolicyDefault 1000000 $ 
        stashJsonLine myDocument
```

## Logging things

The `Logstash` module exports functions for synchronous and asynchronous logging. Synchronous logging is acceptable for applications or parts of applications that are largely single-threaded where blocking on writes to Logstash is not an issue. For multi-threaded applications, such as web applications or services, you may wish to write log messages to Logstash asynchronously instead. In the latter model, log messages are added to a bounded queue which is processed asynchronously by worker threads. 

### Synchronously

The logging functions exported by the `Logstash` module are backend-independent can be invoked synchronously with `runLogstash`, which is overloaded to work with either `Acquire LogstashConnection` or `LogstashPool` (`Pool LogstashConnection`) values and maps to one of the two implementations described below. In either case, you must supply a retry policy and a timeout (in microseconds). The retry policy determines whether performing the logging action should be re-attempted if an exception occurs. The order of operations is:

1. The retry policy is applied.
2. A connection is established using the provided Logstash context.
3. The timeout is applied.
4. The Logstash computation is executed.

If the computation is successful, each step will only be executed once. If an exception is raised by the computation or the timeout, the connection to the Logstash server is terminated and the exception propagated to the retry policy. If the retry policy determines that the computation should be re-attempted, steps 2-4 will happen again. The timeout applies to every attempt individually and should be chosen appropriately in conjunction with the retry policy in mind.

Depending on whether the Logstash context is a `Acquire LogstashConnection` value or a `LogstashPool` (`Pool LogstashConnection`) value, the `runLogstash` functions maps to one of:

- `runLogstashConn` for `Acquire LogstashConnection` (e.g. the result of `logstashTcp` or `logstashTls`).
- `runLogstashPool` for `Pool LogstashConnection` (e.g. the result of `logstashTcpPool` or `logstashTlsPool`). If a connection is available in the pool, that connection will be used. If no connection is available but there is an empty space in the pool, a new connection will be established. If neither is true, this function blocks until a connection is available. The computation that is provided as the second argument is then run with the connection. In the event of an exception, the connection is not returned to the pool.

#### Stashing things by hand

The following functions allow sending data synchronously via the Logstash connection:

- `stash` is a general-purpose function for sending `ByteString` data to the server. No further processing is performed on the data.
- `stashJsonLine` is for use with the `json_line` codec. The argument is encoded as JSON and a `\n` character is appended, which is then sent to the server. 

Any exception raised by the above `stash`ing functions will likely be due to a bad connection. The `runLogstash` functions apply the retry policy before establishing a connection, so in the event that an exception is raised, a new connection will be established for the next attempt.

### Asynchronously

The `withLogstashQueue` function is used for asynchronous logging. When called, it sets up a bounded queue that is then used to communicate log messages to worker threads which dispatch them to Logstash. A minimal example with default settings is shown below:

```haskell
data Doc = Doc String

instance ToJSON Doc where
    toJSON (Doc msg) = object [ "message" .= msg ]

main :: IO ()
main = do
    let ctx = logstashTcp def
    let cfg = defaultLogstashQueueCfg ctx
    
    withLogstashQueue cfg (const stashJsonLine) [] $ \queue -> do
        atomically $ writeTBMQueue queue (Doc "Hello World")
```

The `[]` given to `withLogstashQueue` allows installing exception handlers that are used to handle the case where a log message has exhausted the retry policy. This can e.g. be used to fall back to the standard output for logging as a last resort and to stop the worker thread from getting terminated by an exception that may be recoverable.

The queue is automatically closed when the inner computation returns. The worker threads will continue running until the queue is empty and then terminate. `withLogstashQueue` will not return until all worker threads have returned.

### Usage with `monad-logger`
[![monad-logger-logstash](https://img.shields.io/hackage/v/monad-logger-logstash)](https://hackage.haskell.org/package/monad-logger-logstash)

The `monad-logger-logstash` package provides convenience functions and types for working with [`monad-logger`](http://hackage.haskell.org/package/monad-logger/). 

#### Synchronous logging

The following example demonstrates how to use the `runLogstashLoggingT` function with a TCP connection to Logstash, the default retry policy from [`Control.Retry`](https://hackage.haskell.org/package/retry/docs/Control-Retry.html), a 1s timeout for each attempt, and the `json_lines` codec:

```haskell
main :: IO ()
main = do 
    let ctx = logstashTcp def
    runLogstashLoggingT ctx retryPolicyDefault 1000000 (const stashJsonLine) $ 
        logInfoN "Hello World"
```

Each call to a logging function such as `logInfoN` in the example will result in the log message being written to Logstash synchronously. 

#### Asynchronous logging

The `withLogstashLoggingT` function is the analogue of `withLogstashQueue` for `monad-logger`. It performs the same setup as `withLogstashQueue`, but automatically adds all log messages from logging functions to the queue. A minimal example with default settings is:

```haskell
main :: IO ()
main = do 
    let ctx = logstashTcp def
    withLogstashLoggingT (defaultLogstashQueueCfg ctx) (const stashJsonLine) [] $ 
        logInfoN "Hello World"
```

While `withLogstashLoggingT` is useful for scenarios where there is a single producer for which log messages should be dispatched asynchronously, we may wish to share the same queue among several producers. For such applications, the `runTBMQueueLoggingT` in combination with `withLogstashQueue` is a better fit:

```haskell
main :: IO ()
main = do 
    let ctx = logstashTcp def
    let cfg = defaultLogstashQueueCfg ctx

    withLogstashQueue cfg (const stashJsonLine) [] $ \queue -> do
        thread <- async $ runTBMQueueLoggingT queue $ do
            liftIO $ threadDelay (60*1000*1000)
            logInfoN "I am consumer #2" 
        
        runTBMQueueLoggingT queue $ do 
            logInfoN "I am consumer #1"

        wait thread
```

### Usage with `katip`
[![katip-logstash](https://img.shields.io/hackage/v/katip-logstash)](https://hackage.haskell.org/package/katip-logstash)

The `katip-logstash` package provides convenience functions and types for working with [`katip`](http://hackage.haskell.org/package/katip/). 

#### Asynchronous logging

The `withLogstashScribe` function is the analogue of `withLogstashQueue` for `katip`. It performs the same setup as `withLogstashQueue`, but provides a `Scribe` instead of the raw queue. A minimal example with default settings is (adapted from the `katip` documentation):

```haskell
main :: IO ()
main = do 
    let ctx = logstashTcp def
    withLogstashScribe (defaultLogstashQueueCfg ctx) (const $ pure True) (itemJson V3) (const stashJsonLine) [] $ \logstashScribe -> do
        let makeLogEnv = registerScribe "logstash" logstashScribe defaultScribeSettings =<< initLogEnv "MyApp" "production"
        bracket makeLogEnv closeScribes $ \le -> do
            let initialContext = ()
            let initialNamespace = "main"
            runKatipContextT le initialContext initialNamespace $ do
                $(logTM) InfoS "Hello World"
```
