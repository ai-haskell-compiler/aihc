[hackage-flatparse]: https://hackage.haskell.org/package/flatparse
[hackage-megaparsec]: https://hackage.haskell.org/package/megaparsec

# generic-data-functions
Generic functions that approximate familiar term-level functions. The generics
handle the sum of products representation and have "holes" for the base case,
which must be filled by the user via a type class. Perhaps you may also think of
these as "reusable" or "generic" generics.

Most relevant for simple parsing and printing/serializing/reducing tasks where
the only "work" to do for the given data type is mechanical field sequencing. If
you require more logic than that (and can't place it in types/newtypes), you
will not be able to use this library.

Emphasis is put on type safety and performance. Notably, for sum type generics,
we permit parsing constructor names on the type level (via
[symparsec](https://hackage.haskell.org/package/symparsec)). For cases where you
want to implement your own high-performance sum type handling, we provide
`Generic.Data.FOnCstr`.

## Rationale
There are a number of competing parsing and serialization Haskell libraries.
Most have a type class for enumerating permitted types, and some simple generics
which use that type class for the base case. Other than that base case, everyone
is writing largely the same generic code.

While writing my own libraries, I realized that if another developer wanted use
my serializer, they would probably need to write their own type class for base
cases (due to some specific library design). Alas, this would mean they have to
copy-paste my generics and swap the type classes. Very silly. But it turned out
my generics were otherwise highly general, so I spun them out into this library.
Now you can swap the type class just by filling in some holes.

## Design notes
### Sum types are handled by prepending a sum tag
Unless otherwise specified, this is how we disambiguate constructors. How the
sum tag is parsed from a constructor is up to the user (and may be done on the
type-level if one wishes).

## Functions
### `foldMap` (L->R)
```haskell
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
```

The user provides the `a -> m` dictionary via a special type class instance.
Constructor fields are mapped and combined left-to-right. Sum representations
are handled by mappending the constructor via a user-supplied `String -> m`
first.

Useful for:

  * simple binary serializers which just concatenate fields together
  * reducing to a numeric value

### `traverse` (L->R)
```haskell
traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
```

The user provides the `f a` dictionary via a special type class instance.
Constructor field actions are run left-to-right.
Sum representations are handled by running a constructor action first (thus
requiring `Monad f`).

Useful for:

  * simple binary parsers which can parse generic `foldMap` output

### `contra`
I don't know what this is exactly. It's like contramap?

## Notes
### Orphan instances
This library is designed to work with and around existing libraries and type
classes. Thus, you will likely be dealing in orphans. Instances, that is. That's
life, Jim.

### Funky generics
I have made some weird design choices in this library. Here are some rationales.

#### Handling badness on type & term levels
I don't like silently erroring on badly-configured generics usage, e.g. asking
for a function via generics for an empty data type. Originally, I made those
type error, and that was that. But it meant I would write the same instance over
and over again. And that requirement was hidden in a type class implementation.
Really, it'd be nice if I could put such requirements directly in the types of
the functions that have them.

I've done that. Now, on certain *representation errors*, e.g. you tried to use
non-sum generics on a sum type, we runtime error instead. However, there's a
separate layer for making *assertions about generic representation on the type
level*, the use of which is highly suggested.

Note that if you like to write wrappers over generic functions to fill in
certain bits of info, your job just got a lot uglier. Soz. Anything for type
safety my sweet.

## License
Provided under the MIT license. See `LICENSE` for license text.
