## 0.2.1 [2022.09.24]
* Add `sizeAndAlignmentOfCType` to `Foreign.LibFFI.Base`, which can be used to
  retrieve the size and alignment of a `CType`.

## 0.2 [2022.08.11]
* The `libffi` library now uses `bracket` internally and should now be
  exception-safe.
* There is a now a `ghc-bundled-libffi` `cabal` flag that makes this library
  statically link against GHC's bundled copy of `libffi` rather than attempt to
  link against the system `libffi`. On the vast majority of GHCs, this is the
  most reasonable option, as linking against the system `libffi` is inherently
  fragile. As a result, `+ghc-bundled-libffi` is now the defalut setting. See
  the [`README`](https://github.com/remiturk/libffi/blob/master/README.md#notes-on-ghcs-bundling-of-libffi)
  for more discussion on this point.
* The definition of `Arg` has changed:

  ```diff
  -newtype Arg = Arg { unArg :: IO (Ptr CType, Ptr CValue, IO ()) }
  +newtype Arg = Arg { unArg :: forall a. (Ptr CType -> Ptr CValue -> IO a) -> IO a }
  ```
* The definition of `RetType` has changed:
  ```diff
  -data RetType a = RetType (Ptr CType) ((Ptr CValue -> IO ()) -> IO a)
  +newtype RetType a = RetType { unRetType :: (Ptr CType -> Ptr CValue -> IO ()) -> IO a }
  ```

## 0.1 [2009.03.17]
* Initial release.
