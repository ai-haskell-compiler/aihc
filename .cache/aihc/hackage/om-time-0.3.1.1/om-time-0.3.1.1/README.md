# om-time

Some simple time utilities used by other `om-*` packages.

Mostly this contains a newtype wrapper around `UTCTime` that has a
`Binary` instance, and a monad interface to provide the current time.

