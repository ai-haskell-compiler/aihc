# Error messages:
{{ERROR_MESSAGES}}

Re-test by running: nix run .#aihc-dev -- hackage-tester {{PACKAGE_NAME}}
Haskell 2010 language report (contains a slightly outdated specification for syntax): docs/haskell2010-language-report.md

Starting hints:
- Do not assume the HSE minimizer has found the real problem. The minimizer may be incorrect.
- There may be more than one issue with the package. It is okay to fix just a single issue. Reducing the number of failed files is a win.

Fix the parsing issue that prevents '{{PACKAGE_NAME}}' from parsing successfully. Remember to include unit tests, golden tests, oracle tests, and quickcheck properties if appropriate. Open a PR when done.
