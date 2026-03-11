[![Hackage](http://img.shields.io/hackage/v/simple-cmd.png)](http://hackage.haskell.org/package/simple-cmd)
[![Stackage LTS](http://stackage.org/package/simple-cmd/badge/lts)](http://stackage.org/lts/package/simple-cmd)
[![Stackage Nightly](http://stackage.org/package/simple-cmd/badge/nightly)](http://stackage.org/nightly/package/simple-cmd)

# simple-cmd

Some simple String wrappers of `readProcess`, `readProcessWithExitCode`,
`rawSystem` from the Haskell `process` library.

## Usage

```haskell
import SimpleCmd
```

```haskell
cmd_ :: String -> [String] -> IO ()
```
outputs to stdout. For example

```haskell
cmd_ "echo" ["Hello"]
cmd_ "git" ["clone", url]
```
This can shortened to `git_ "clone" [url]`.

```haskell
cmd :: String -> [String] -> IO String
```
returns stdout as a `String`. eg

```haskell
date <- cmd "date" []
```

There are also `cmdBool`, `cmdMaybe`, `cmdList`, `shell`, and others.

Simple pipes are also supported:
```haskell
pipe_ ("echo",["hello"]) ("grep",["ello"])
```
`pipeBool` returns True if both commands succeed.

Other examples:
```haskell
gitBranch :: IO String
grep_ pat file :: IO Bool
sudo_ c args :: IO ()
timeIO :: IO a -> IO a
```

See the library documentation for more details.
