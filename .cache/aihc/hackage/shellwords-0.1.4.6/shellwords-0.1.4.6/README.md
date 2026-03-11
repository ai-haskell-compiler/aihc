# ShellWords

[![Hackage](https://img.shields.io/hackage/v/shellwords.svg?style=flat)](https://hackage.haskell.org/package/shellwords)
[![Stackage Nightly](http://stackage.org/package/shellwords/badge/nightly)](http://stackage.org/nightly/package/shellwords)
[![Stackage LTS](http://stackage.org/package/shellwords/badge/lts)](http://stackage.org/lts/package/shellwords)

Parse a string into words, like a shell would.

## Motivation

If you want to execute a specific command with input given to you from an
untrusted source, you should not give that text as-is to a shell:

```hs
let userInput = "push origin main"

callCommand $ "git " <> userInput
-- Forward output of the push command...
```

You may be tempted to do this because you want to correctly handle quoting and
other notoriously-difficult word-splitting problems. But doing so is a severe
security vulnerability:

```hs
let userInput = "push origin main; cat /etc/passwd"

callCommand $ "git " <> userInput
-- Forward output of the push command...
-- And then dump /etc/passwd. Oops.
```

Furthermore, any attempts to sanitize the string are unlikely to be 100%
affective and should be avoided. The only safe way to do this is to not use a
shell intermediary, and always `exec` a process directly:

```hs
let userInput = "push origin main"

callProcess "git" $ words userInput
-- Forward output of the push command...
```

Now, there's no vulnerability:

```hs
let userInput = "push origin main; cat /etc/passwd"

callProcess "git" $ words userInput
-- Invalid usage. :)
```

The new problem (but not a security-related one!) is how to correctly parse a
string like `"push origin main"` into command arguments. The rules are complex
enough that you probably want to get a library to do it.

So here we are.

## Example

```hs
Right args <- parse "some -complex --command=\"Line And\" 'More'"

callProcess cmd args
--
-- Is equivalent to:
--
-- > callProcess cmd ["some", "-complex", "--command=Line And", "More"]
--
```

## Unsafe Usage

The following is a perfectly reasonable thing one might do with this library:

```hs
Right (cmd:args) <- parse userInput

callProcess cmd args
```

However, if:

1. `userInput` is un-trusted, and
1. You do no further validation of what `cmd` can be,

Then this re-introduces the original security vulnerability and, at that point,
you might as well just pass `userInput` to a shell.

## Lineage

This package is inspired by and named after

- [`python-shellwords`][python-shellwords], which was itself inspired by
- [`go-shellwords`][go-shellwords], which was itself inspired by
- [`Parser::CommandLine`][parser-commandline]

[python-shellwords]: https://github.com/mozillazg/python-shellwords
[go-shellwords]: https://github.com/mattn/go-shellwords
[parser-commandline]: https://github.com/Songmu/p5-Parse-CommandLine

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
