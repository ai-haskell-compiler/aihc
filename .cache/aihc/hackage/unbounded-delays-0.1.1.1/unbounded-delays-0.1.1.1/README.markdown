The [threadDelay] and [timeout] functions from the `base` library use the
bounded `Int` type for specifying the delay or timeout period. This packages
provides alternatives which use the unbounded `Integer` type.

[threadDelay]: http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Concurrent.html#v:threadDelay
[timeout]:     http://hackage.haskell.org/packages/archive/base/latest/doc/html/System-Timeout.html#v:timeout
