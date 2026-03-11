# simple-cmd-args

[![Hackage](https://img.shields.io/hackage/v/simple-cmd-args.svg)](https://hackage.haskell.org/package/simple-cmd-args)
[![BSD license](https://img.shields.io/badge/license-BSD-blue.svg)](LICENSE)
[![Stackage Lts](http://stackage.org/package/simple-cmd-args/badge/lts)](http://stackage.org/lts/package/simple-cmd-args)
[![Stackage Nightly](http://stackage.org/package/simple-cmd-args/badge/nightly)](http://stackage.org/nightly/package/simple-cmd-args)

A thin layer over optparse-applicative that avoids type plumbing for
subcommands by using `Parser (IO ())`.

Various wrapper functions are also provided for common option/arg idioms.

See the library's [documentation](https://hackage.haskell.org/package/simple-cmd-args/docs/SimpleCmdArgs.html) for details of all the functions provided.

## Usage

```shellsession
$ cat readme.hs
```
```haskell
import SimpleCmdArgs
import Control.Applicative (some)
import System.Directory

main =
  simpleCmdArgs Nothing "readme example" "Longer description..." $
  subcommands
    [ Subcommand "echo" "Print words" $
      putStrLn . unwords <$> some (strArg "STR...")
    , Subcommand "ls" "List directory" $
      ls <$> strArg "DIR"
    , Subcommand "mkdir" "Create directory" $
      mkdir <$> parentsOpt <*> strArg "DIR"
    ]
  where
    parentsOpt = switchWith 'p' "parents" "Make missing directories"

ls :: FilePath -> IO ()
ls dir =
  listDirectory dir >>= mapM_ putStrLn

mkdir :: Bool -> FilePath -> IO ()
mkdir parents =
  if parents then createDirectoryIfMissing True else createDirectory
```
```shellsession
$ ghc readme.hs
$ ./readme --help
readme example

Usage: readme COMMAND
  Longer description...

Available options:
  -h,--help                Show this help text

Available commands:
  echo                     Print words
  ls                       List directory
  mkdir                    Create directory
$ ./readme echo hello world
hello world
$ ./readme mkdir -h
Usage: readme mkdir [-p|--parents] DIR
  Create directory

Available options:
  -p,--parents             Make missing directories
  -h,--help                Show this help text
```

# Examples
See the [examples](https://github.com/juhp/simple-cmd-args/tree/main/examples).

Hackage packages using this library: https://packdeps.haskellers.com/reverse/simple-cmd-args
