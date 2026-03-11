# `some-dict-of`

Have you ever needed an existential wrapper that just guaranteed class
membership, but you didn't need to know the specifics of the type in question?

Well, you probably *don't* - it's not idiomatic Haskell for the most part. But
sometimes this comes up, and I wanted a nice packaging of the technique. I wrote
this mostly to support the `discover-instances` library.

## `SomeDictOf`

Let's consider a type that wraps anything that can be shown, along with a value
of that type. We can make one by hand like this:

```haskell
data SomeShowable where
    SomeShowable :: forall a. Show a => a -> SomeShowable

showList :: [SomeShowable] -> [String]
showList = map (\(SomeShowable a) -> show a)
```

With `SomeDictOf`, we can generalize this pattern to other classes and even
other containers.

```haskell
someShowable :: SomeDictOf Identity Show
someShowable =
    SomeDictOf (Identity (3 :: Int))
```

We're carrying an `Int`, but the type does not reveal this. We can create a list
of values like this, and we can call `show` on them.

```haskell
showValues
    :: [SomeDictOf Identity Show]
    -> [String]
showValues =
    map (\(SomeDictOf (Identity showable)) -> show showable)
```

We can also carry around evidence of a type class instance, and then write
generic stuff based on it. Consider the
[`PersistEntity`](https://hackage.haskell.org/package/persistent-2.13.1.1/docs/Database-Persist-Class-PersistEntity.html#t:PersistEntity)
from the `persistent` database library.

```haskell
tables :: [SomeDictOf Proxy PersistEntity]
tables = [ SomeDictOf (Proxy @User), SomeDictOf (Proxy @Organization) ]
```

Now we can iterate over these types and, say, load all the rows out of the
database and verify they parse.

```haskell
checkRows :: SqlPersistT [[PersistValue]]
checkRows = do
    forM tables $ \(SomeDictOf (Proxy :: Proxy table)) -> do
        results <- selectList [] [] :: SqlPersistT m [Entity table]
        pure (map toPersistValue results)
```

Or we can do some metaprogramming based on their `EntityDef`s, since
`PersistEntity` has a method `Proxy a -> EntityDef`.

```haskell
getDefinitions :: [EntityDef]
getDefinitions =
    map (\(SomeDictOf p) -> entityDef p) tables
```
