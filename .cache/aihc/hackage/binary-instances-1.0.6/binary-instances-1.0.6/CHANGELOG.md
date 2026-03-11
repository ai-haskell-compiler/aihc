## 1.0.6

- Support GHC-9.12.1
- Fix a small bug QuarterOfYear instance

## 1.0.5

- Support GHC-8.6.5...9.10.1

## 1.0.4

- Add instances for `ByteArray` from `data-array-byte` or `base`
- Support `primitive-0.8`
- Support `tagged-0.8.7`

## 1.0.3

- Add instance for `primitive`s `ByteArray`.

## 1.0.2

- Support `aeson-2.0.0.0`

## 1.0.1

- Add instances for `time-1.11` types: `Month`, `Quarter`, `QuarterOfYear`

## 1

- **Breaking change** rename this package to `binary-instances`.
It was previously known as `binary-orphans`, which is now only a subset.

- Depend on [`time-compat`](http://hackage.haskell.org/package/time-compat)
  to provide `Binary` instances for `time` types.

## 0.1.8.0

- `ZonedTime` instance (by @mstksg)

## 0.1.7.0

- GHC-8.2 support
- `SystemTime` instance (`time-1.8`)

## 0.1.6.0

- Add instance for `hashed` in `hashable >=1.2.5.0`

## 0.1.5.2

- Fix issue with binary-0.5.x

## 0.1.5.1

- Fix issue with binary-0.8.4.x and GHC-7.x

## 0.1.5.0

- Add `CI a` instance
- Add `Alt f a` instance for `base >= 4.8.0.0`
- Add `WrappedMonoid m` and `Arg a b` instances
- Support `binary-0.8.4.0`
- Add `Void` instance (`base <4.8`)
- Add `Natural` instance (`nats <1.1`, `base <4.8`)

## 0.1.4.0

- Add `AbsoluteTime` instance (thanks @neongreen)

## 0.1.3.1

- Support GHC 7.6

## 0.1.3.0

- Add `Min`, `Max`, `First`, `Last`, `Option`, and `NonEmpty` instances (from `semigroups`)

## 0.1.2.0

- Support `scientific >= 0.3.4`

## 0.1.1.0

- Add `Dual`, `All`, `Any`, `Sum`, `Product`, `First` and `Last` instances
