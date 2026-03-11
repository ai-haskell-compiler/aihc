# streaming-binary

This library offers incremental serialization and deserialization of
Haskell values using the `binary` package. Under the hood, we wrap the
incremental decoders from `binary`'s Incremental API as first-class
streams as defined in `streaming`.
