# Changelog for monad-logger-logstash

## v0.2

- Add `runTBMQueueLoggingT` and `unTBMQueueLoggingT`
- Rename `runLogstashLoggerT` and `withLogstashLoggerT` to use the right monad name

## v0.1

- Support for synchronous logging with `runLogstashLoggerT` and asynchronous logging with `withLogstashLoggerT`