# main-tester

Capture stdout/stderr/exit code, and replace stdin of your main function.

## Why?

Against the best practice, I often prefer large, end-to-end (E2E) tests.  
Because:

- E2E tests can directly test your users' requirement.
- E2E tests can detect bugs caused by misassumption of external components
    - Which happenes relatively more often than the others, according to my experience.
- I often write small programs where E2E tests are sufficient.

This library main-tester provides utility functions for E2E testing of your CLI applications.  
With main-tester, You can test your apps with arbitrary stdin data, check their output and exit code.

## Comparison with [silently](https://hackage.haskell.org/package/silently) and [System.IO.Fake](https://hackage.haskell.org/package/imperative-edsl-0.7.1/docs/System-IO-Fake.html)

- Can test exit code of your main.
- All input and outputs are strict bytestrings except the command line arguments, which is very important when testing with non-UTF8 input and output.
- Currently doesn't support suppressing stdout and stderr (Pull request welcome!).

## Usage

See [the document of `Test.Main` module](https://hackage.haskell.org/package/main-tester/docs/Test-Main.html).
