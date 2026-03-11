# quiet

Generic deriving of `Read` / `Show` with no record labels.

[![Hackage][hackage-shield]][hackage] [![Travis][travis-shield]][travis]

Often one wants to create a `newtype` which has a convenient field
accessor like `unUserId` below, but that unfortunately makes the `Show`
instance which is derived overly verbose.

For example:

```hs
newtype UserId = UserId { unUserId :: String }
  deriving (Read, Show)
```

```
ghci> show (UserId "simon")
UserId {unUserId = "simon"}
ghci> read "UserId {unUserId = \"simon\"}" :: UserId
UserId {unUserId = "simon"}
```

With `DerivingVia` `Quiet` you can have a `Show` instance which doesn't
print the field labels. It will render as if the `unUserId` accessor
wasn't present at all.

```hs
newtype UserId = UserId { unUserId :: String }
  deriving (Generic)
  deriving (Read, Show) via (Quiet UserId)
```

```
ghci> show (UserId "simon")
UserId "simon"
ghci> read "UserId \"simon\"" :: UserId
UserId "simon"
```

If you want to derive `Read` / `Show` without using `DerivingVia` then
you can use `qreadPrec` and `qshowsPrec` directly.

```hs
instance Read UserId where readPrec = qreadPrec
instance Show UserId where showsPrec = qshowsPrec
```


 [hackage]: http://hackage.haskell.org/package/quiet
 [hackage-shield]: https://img.shields.io/hackage/v/quiet.svg?style=flat

 [travis]: https://travis-ci.org/jacobstanley/quiet
 [travis-shield]: https://travis-ci.org/jacobstanley/quiet.svg?branch=master
