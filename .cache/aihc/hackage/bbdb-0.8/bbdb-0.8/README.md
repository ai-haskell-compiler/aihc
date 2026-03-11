Database.BBDB
=============

A relatively primitive (but working) Haskell library to read and write
BBDB (Insidious Big Brother Database) files.  One major goal was to be
able to have the following be the identity function:

```Haskell
d <- readBBDB "/path/to/.bbdb"
-- d is now a Haskell data type
writeFile (asLisp d) "path/to/copy-of-.bbdb"
```

Because of this goal, the interface is based on lists, and tends to be
a lot of work to get to the actual data you are looking for.  Sorry
about that.