% GUESS-CHANGELOG(1) Guess change descriptions between releases

# NAME

guess-changelog -- Extract additions to change log file between releases

# SYNOPSIS

**guess-changelog** OLD-DIR NEW-DIR

# DESCRIPTION

Many free software authors include a (manually maintained) change log file in
their release tarballs that describes important changes from one version to the
next, and it's good practice for distribution packagers to include that
information in meta sections of their packaging efforts so that the package
managing software can easily display it to users during updates, etc. In the
rpm(8) world, this is usually accomplished by adding a `pkg-name.changes` file
next to the `pkg-name.spec` file that mentions relevant bits of the upstream
change log.

Now, this tools makes tries to extract the necessary information from upstream
releases automatically. Given to release tarballs `foo-X.tar.gz` and
`foo-Y.tar.gz`, just extract those tarballs and run `guess-changelog` with the
appropriate directories as arguments:

    $ guess-changelog foo-X foo-Y

If `guess-changelog` can determine the part of the change log that was added
between the two releases, it will write the text to standard output. On some
occasions, however, `guess-changelog` will fail:

* Neither release contains a change log file.

* A change log file exists, but it's identical in both releases. In other
  words, upstream probably forgot to document the release.

* Both releases contain a set of files that look like they might be a change
  log, but their intersection is empty! This happens, for example, when
  upstream has renamed the file.

* Multiple change log files exists in both directories. Now, it would probably
  work out okay if we'd just look at the diffs of both of them, respectively,
  but it felt like a good idea to err on the side of caution. This case is rare
  anyways.

* `guess-changelog` accepts up to 10 lines of unmodified text at the top of the
  upstream change log file because some people like to have a short
  introduction text there etc. If that header becomes too large, however, an
  error is returned because we expect upstream to add text at the *top*, not in
  the middle of the file.

* Upstream has edited the file in some non-trivial way other than just adding
  at the top. Sometimes people re-format old entries or rewrite URLs or fix
  typos, and in such a case it feels to risky to trust the diff.

# RETURN VALUES

`guess-changelog` returns a non-zero exit code only if some kind of
system-level error ocurred, such as a permission error while trying to access
the given directories. In all other cases, the tool exists with 0.

If a change log entry was detected successfully, it will be written to standard
output. In no change log entry could be detected, the tool writes a brief
explanation of the issue to the standard error stream, but it won't write to
standard output.

# AUTHOR

ShellCheck is written and maintained by Peter Simons. Please report any bugs
you may find at <https://github.com/peti/distribution-opensuse/>.

# COPYRIGHT

Copyright 2018 by Peter Simons of SUSE Linux GmbH.

Licensed under the terms of the [BSD-3-Clause license](https://opensource.org/licenses/BSD-3-Clause).
