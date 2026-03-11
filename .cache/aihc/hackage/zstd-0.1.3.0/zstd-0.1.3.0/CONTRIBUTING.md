# Contributing to `hs-zstd`

We want to make contributing to this project as easy and transparent as
possible.

## Our development process

This library is developed in the open on GitHub.  We publish
occasional releases to Hackage, either to fix bugs or add new
functionality.

If you run into problems with either the code, documentation, or
examples, please
[file a GitHub issue](https://github.com/luispedro/hs-zstd/issues).

(Note: if you'd like to file an issue requesting new functionality,
rather than bugfixes, please plan to implement that functionality
yourself and submit a pull request. We regret that we are very
unlikely to have time to extend the library for you.)

## Pull Requests

We very much welcome your pull requests! To respect our time, we
request that you follow the conventions below.

1. Fork the repo and create your branch from `master`.
2. If you're adding code or functionality, please add tests (and, if
   necessary) examples.
3. If you're changing APIs, please update the appropriate
   documentation and examples, and the [changelog](changelog.md).
   (Breaking API changes have a high bar for approval.)
4. Ensure that the test suite passes (`stack build --test`).
5. Make sure your code conforms to the existing style you see in the
   code base.
6. Tidy up your commit history before submitting a pull request.  This
   means one commit per logical change, please.

## Issues

We use GitHub issues to track public bugs.  Please ensure that your
description is clear and has sufficient instructions for us be able to
reproduce the issue.

## License

By contributing to `hs-zstd`, you agree that your contributions will
be licensed under the [`LICENSE`](LICENSE) file in the root directory
of this source tree.
