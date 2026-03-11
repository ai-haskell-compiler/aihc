# persistent-discover

![build status](https://github.com/parsonsmatt/persistent-discover/actions/workflows/haskell.yml/badge.svg)


Supports automatic discovery of your
[`persistent`](https://hackage.haskell.org/package/persistent) models.

## As an executable

Let's say you've got a ton of `persistent` database models, all defined in a
module hierarchy like this:

```
src/
  Models/
    Foo.hs
    Bar.hs
    Baz.hs
    Blargh.hs
    What.hs
    OhNo.hs
```

If you're using `persistent` to automatically generate migrations, you'll want
to have all the `[EntityDef]` in scope from each module. You can do that by
importing each module that defines models and calling `$(discoverEntities)`,
introduced in `persistent-2.13`.

But you may forget to import a module.

This utility imports *all* the modules defined in the current directory and
sub-directories, and then it calls `$(discoverEntities)` for you.

To use it, place the following command in a file, located in the same directory
that your Haskell modules are in:

```
{-# OPTIONS_GHC -F -pgmF persistent-discover #-}
```

Let's say we put that in `src/Models/All.hs`.
This generates a module `Models.All` that exports a single item: `allEntities`,
which is a `[EntityDef]` and can be used by `migrateModels` to properly perform
migrations.

You'll need to add `persistent-discover` to the `build-tool-depends` on your cabal file
for this to work.
