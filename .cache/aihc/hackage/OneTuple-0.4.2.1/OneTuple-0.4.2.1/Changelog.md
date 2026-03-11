# 0.4.1.1

- Support GHC-7.2 and GHC-7.0.

# 0.4.1

- Mark `MkSolo` pattern synonym as `COMPLETE`

# 0.4

- Rename constructor to `MkSolo` as in `base-4.17`.
  The compatibility pattern synonym is provided.
- Add `Foldable1 Solo` instance

# 0.3.1

- Add `Data.Tuple.Solo.TH` with `tupE` using `Solo` from this package
- Add `Hashable` and `Hashable1` instances
- Drop GHC-7.0 and GHC-7.2 support

# 0.3

- Rename `OneTuple` to `Solo`
- Add `Typeable`, `Data`, `MonadZip`, `Eq1`, `Ord1`, `Show1`, `Read1`,
  `Generic` and `Generic1` instances

# 0.2.2.1

- Compatible with GHC-8.6

# 0.2.2

- Add `Semigroup` instances
- Compatible with GHC-8.4
