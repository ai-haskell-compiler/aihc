# Within

`Within` is a type for simply scoping well-typed paths. A `Within b a` is
simply an `Env` comonad with the environment specialised to `Path b Dir`. This
is useful for when you want to keep track of things that live within a parent
folder and need the extra degree of articulation. You can construct a value `a`
living within a `Path b Dir` using the `within` infix operator.

```{.haskell}
5 `within` $(mkRelDir "foo")
```

There are also convenience functions for dealing with the special case where
the inner type is a `Path Rel File`, which represents a path to a file within a
directory. This does not need to be an immediate child of the directory, and
does not have to exist. You can assert that you can assert that an existing
path lies in a directory by using `asWithin`, which throws if the directory is
not a proper prefix of the `Path`.

```{.haskell}
$(mkRelFile "foo/a.txt") `asWithin` $(mkRelDir "foo") -- fine
$(mkRelFile "a.txt") `asWithin` $(mkRelDir "foo") -- throws NotAProperPrefix Exception
```

You can also use `fromWithin` to get from a `Within a (Path Rel t)` to a `Path a t`.

There are also `Eq`, `Show` and `Hashable` instances when the extract target is of
that class.
