
# 0.3

- Extracted the `Is` and `IsSome` typeclasses into a separate library ["lawful-conversions"](https://github.com/nikita-volkov/lawful-conversions)
- Restored the `IsomorphicTo` typeclass from the `0.1` design without restoring the `String` instance and the `showAs` utility

# 0.2

- `IsomorphicTo` renamed to `Is`
- `IsSome` is introduced as a parent class to it
- `to` moved to `IsSome`
- `showAs` dropped
- `String` isomorphism instances for textual types dropped, since they were not exactly isomorphisms
- `IsSome` instances added for `String` for textual types
