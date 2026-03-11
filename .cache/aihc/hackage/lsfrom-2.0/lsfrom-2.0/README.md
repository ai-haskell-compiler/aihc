# lsfrom

`lsfrom` lists the files and/or dirs in a directory starting from
the given filename or the next one after that,
using the current locale collation order.

## Tutorial

Consider a directory with a few files:
```shellsession
$ ls
A b B C
```
(`lsfrom` is typically more useful in larger directories,
but to keep things simple let's use a small example here.)

Let's list the files after `b`:
```shellsession
$ lsfrom --after b/
B
C
```

Equivalently the files from `B`:
```shellsession
$ echo $(lsfrom --from B)
B C
```

We can also list until:
```shellsession
$ lsfrom --until B
A
b
B
```
Note the file (collation) order will depend on the locale in general:
```shellsession
$ LC_COLLATE=C echo $(lsfrom -f B)
B C b
```

`--from`/`--after` can be combined with `--until`/`--before`.
Without any of these options, the output should be similar to `ls -A`,
but only 1 file per line.

The file of the path given does not need to exist (unless using --strict mode):
```shellsession
$ lsfrom -a /t
/tmp
/usr
/var
```

It is also possible to filter the listing with `--dirs` or `--files`.

A common use-case is continuing a program or script on
the entries of a directory after a failure, etc:
```shellsession
$ myscript.sh $(lsfrom -f next)
```
or only running a command on a subrange of files or dirs in a directory.

## Usage

`$ lsfrom --version`

```
2.0
```
`$ lsfrom -h`

```
List directories files starting from file

Usage: lsfrom [--version] [-s|--strict] [-A|--all] [--dirs | --files]
              [(-f|--from STARTFILE) | (-a|--after STARTFILE)]
              [(-u|--until LASTFILE) | (-b|--before LASTFILE)]

  lsfrom lists the files in a directory that follow from the given file

Available options:
  -h,--help                Show this help text
  --version                Show version
  -s,--strict              fail if specified file(s) do not exist
  -A,--all                 include hidden (dot) files
  --dirs                   Only list directories
  --files                  Only list files
  -f,--from STARTFILE      files from STARTFILE
  -a,--after STARTFILE     files after STARTFILE
  -u,--until LASTFILE      files until LASTFILE
  -b,--before LASTFILE     files before LASTFILE
```

## Requirements
Tested on Linux (may also work on MacOS):
specifically it uses libc collation (`wcscoll()`) for file ordering.

It also uses `ls` to list files sorted in the locale collation
with the `-A` option to exclude `./` and `../` (ie coreutils).

## Installation

`stack install lsfrom` or `cabal install lsfrom`.

## Contribute or feedback
lsfrom is distributed with BSD license
and the project is hosted at <https://github.com/juhp/lsfrom>.
