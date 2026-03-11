# hwk ![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)

**hwk** (pronounced "hawk") is a simple Haskell-based commandline text processing tool, somewhat similar to tools like *awk*, *grep*, *sed*.
`hwk` applies composed pure Haskell functions to a list of lines of input, enabling text processing without having to remember an obscure DSL or awkward cli options. This tool can also help to encourage people to think functionally.

<img align="right" alt="[hawk image]" src="hwk.png" />

hwk was originally written by Lukas Martinelli in 2016-2017:
see the [original README file](README.md.orig).

**hwk** is pretty similar to [Hawk](https://github.com/gelisam/hawk),
so you may also want to try that for a different more sophisticated monadic
implementation. Some of main differences are:

- hwk uses String for input for type simplicity, whereas hawk uses ByteString
- hawk has special options for controlling input and output field/lines delimiters, whereas in hwk everything is roughly just `[String] -> [String]` (more details below)
- by default hwk applies a function to the list of all the lines of stdin: `hwk -l` corresponds to `hawk -m` and `hawk -a` to `hwk`. `hwk -a` applies the function to the whole stdin.

## Examples
Some simple use-cases are in the [examples](examples/) directory.

Change and append a string to each line:
```bash
$ seq 0 2 | hwk --line '(++ ".txt") . show . (+1) . int'
1.txt
2.txt
3.txt
```
or without line-mode: `hwk 'map ((++ ".txt") . show . (+1) . int)'`.

Sum all negative numbers:
```bash
$ seq -100 100 | hwk 'sum . filter (< 0) . ints'
-5050
```
The ints function transforms a list of strings into a list of ints

Factorials in your shell scripts!:
```bash
$ seq 10 12 | hwk --line 'let {fact 0 = 1; fact n = n * fact (n - 1)} in fact . int'
3628800
39916800
479001600
```

Extract data from a file:
```bash
$ cat /etc/passwd | hwk -l 'reverse . filter (/= "x") . take 3 . splitOn ":"' | head -3
0 root
1 bin
2 daemon
```
(uses `splitOn` from the extra library; `-l` is the short form of `--line`).

The argument passed to `hwk` must be a valid Haskell function: a function that takes a list of strings and returns a new list or a single value.

Check whether the input contains a certain string:
```
$ cat /etc/passwd | hwk --all 'bool "no" "yes" . isInfixOf "1000"'
yes
```

You can also type-check functions:
```bash
$ hwk --typecheck take
Int -> [a] -> [a]
```
or expressions:
```bash
$ hwk -t [1,2]
Num a => [a]
```

And evaluate expressions:
```bash
$ hwk -e '2 ^ 32 `div` 1024'
4194304
```

Run commands:
```bash
$ hwk --run getCurrentDirectory
/home/user/src
```

## Configuration
`hwk` uses a Haskell configuration file `~/.config/hwk/Hwk.hs` which provides the context for the hint evaluation of the supplied function. Hint (ghci) checks the current directory first when loading, so one can override the configuration on a directory basis.

The first time hwk is run it sets up `~/.config/hwk/Hwk.hs`.

The default [Hwk module](data/Hwk.hs) configuration imports
`Prelude`, `Data.List`, `Data.Char`, and `System.FilePath`
into the hint interpreter.

You can add other modules to import to `userModules` or
define your own functions to use in hwk expressions if you wish.

After a hwk version update you may need or wish to sync up your Hwk.hs file to take account of any new changes: a copy of the latest default Hwk.hs is also put in `~/.config/hwk/` with version suffix.

One can specify `-c`/`--config-dir` to load Hwk.hs from a different location.

## Installation
Either use the `install.sh` script, or install by cabal-install or stack
as described below:

### install.sh script from source tree or git
Use `stack unpack hwk` or `git clone https://github.com/juhp/hwk`.

Then go to the source directory and run the `install.sh` script, which first runs `stack install`, then moves the binary installed by `stack install` to `~/.local/lib/hwk`, and sets up a wrapper script `~/.local/bin/hwk` which runs it.

If you wish you can change the resolver in stack.yaml first: it is also used to determine the resolver used by the created `hwk` wrapper script.

### cabal
If you are on a Linux distro with a system installed ghc and Haskell libaries,
you can install with `cabal install` to make use of them.

### stack
Installing and running with stack is better if you do not have a system ghc
and/or global system Haskell libraries installed.

If you prefer not to use `install.sh` in the source dir,
you can install by hand: run `stack install`,
and then run it with `stack exec hwk ...` using the same resolver.

## How does `hwk` work?
- `hwk` use the hint library to apply haskell functions to input.
- By default it splits the input to a list of lines and applies the function to them
- Use `-a` or `--all` to apply a function to all the input,
  or `-l`/`--line` to map the function on each line separately,
  or `-w`/`--words` to map the function on each line of words,
- If you pass file arguments, their contents will be read and passed to the function.
- You can also typecheck the function or an expression with `-t`/`--typecheck`,
  evaluate an expr with `-e`/`--eval`, or `-r`/`--run` an IO statement.

Note the equivalences:
- `hwk 'f . unlines'` == `hwk -a f`
- `hwk 'map f'` == `hwk -l f`
- `hwk -a 'f . lines'` == `hwk f`
- `hwk -l 'f . words'` == `hwk -w f`
- `hwk 'map (f . words)'` == `hwk -w f`

## Supported return types

The following return values are supported:

- `String`
- `[String]`
- `[[String]]`
- `Int`
- `[Int]`
- `[[Int]]`

## Contribute

Open an issue or pull request at https://github.com/juhp/hwk
to report problems or make suggestions and contributions.

Usage example contributions are also welcome.

## Related/alternative projects
- https://github.com/gelisam/hawk
- https://github.com/bawolk/hsp
- https://en.wikipedia.org/wiki/AWK
- https://en.wikipedia.org/wiki/Sed
- https://code.google.com/p/pyp/
