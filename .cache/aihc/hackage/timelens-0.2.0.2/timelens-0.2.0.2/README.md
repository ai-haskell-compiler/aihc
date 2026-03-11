# timelens

This package contains `lens` lenses for the `time` package.

In theory, you can write lenses without depending on any lens library.
However, that is more work, so this package simply depends on `lens`.
`lens` is a heavyweight library, but it has nice things like `Iso`s,
which are used in this package, so I will stick with `lens` rather
than using a more lightweight lens library like `microlens`.

You should also look at the `lens-datetime` package:

https://www.stackage.org/package/lens-datetime

The main difference between `lens-datetime` and this package is that
this package does not attempt to do anything fancy.  For example,
`lens-datetime` has typeclasses to allow you to access types of
different values by using functions of the same name.  `timelens` does
not attempt anything like this.

Some functions or values in `timelens` do not obey the lens laws;
everything in `timelens` obeys the lens laws.  `timelens` does not
attempt to do any sort of conversions or operations on dates or times.

`lens-datetime` does not cover all types in the `time` package, while
`timelens` does.

In short, `lens-datetime` is more ambitious, while `timelens` is
pretty much what you would get if the `time` library had included the
`Control.Lens.makeLenses` Template Haskell splices.

## License

This package is released under the BSD3 license. Please see the LICENSE file.

## Building this project

If you are viewing the source code repository, a `.cabal` file might
not be included.  Please see the README.md in the main project directory
for details on how to generate the `.cabal` file.
