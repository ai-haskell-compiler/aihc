# polysemy-fs

This package offers small filesystem effects for polysemy, such as:

```
data FSExist m a where
  DoesFileExist :: Path b File -> FSExist m Bool
  DoesDirExist  :: Path b Dir  -> FSExist m Bool
```

It should be noted that these are very weak semantic abstractions, as it does
not provide a way to speak to the filesystem as a whole, and therefore is
somewhat unsuitable for mocking. These effects exist primarily to give
interpreters a way to be precise in the type of filesystem operations they use,
and should be treated in the same way as `Embed IO` in that regard. However, if
you need a quick fix - then using this directly in application code is a slight
improvement over using `Embed IO` directly.

Using these as very low level compilation units allows you to inject debugging
everywhere you use a filesystem operation. For example, using [co-log-polysemy](https://hackage.haskell.org/package/co-log-polysemy) we can do this:

```
data FileExists where
  FileExists    :: Path b File -> FileExists
  FileNotExists :: Path b File -> FileExists

data DirExists where
  DirExists    :: Path b Dir -> DirExists
  DirNotExists :: Path b Dir -> DirExists

logFileExists :: Members '[FSExist, Log FileExists] r => Sem r a -> Sem r a
logFileExists = intercept \case
  DoesFileExist x -> do
    z <- doesFileExist x
    case z of
      True  -> log $ FileExists x
      False -> log $ FileNotExists x
    return z
  DoesDirExist x -> doesDirExist x

logDirExists :: Members '[FSExist, Log DirExists] r => Sem r a -> Sem r a
logDirExists = intercept \case
  DoesDirExist x  -> do
    z <- doesDirExist x
    case z of
      True  -> log $ DirExists x
      False -> log $ DirNotExists x
    return z
  DoesFileExist x -> doesFileExist x
```

For a look at a unified version of filesystem operations, check out [polysemy-fskvstore](https://hackage.haskell.org/package/fskv-store)
