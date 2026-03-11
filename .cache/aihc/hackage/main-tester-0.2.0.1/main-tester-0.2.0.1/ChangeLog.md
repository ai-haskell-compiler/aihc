# 0.2.0.1

- Fix: Fix `Handle` leak (!2)

# 0.2.0.0

- Breaking change:
    - Add `prException` field to `ProcessResult`.
    - Now `captureProcessResult` catches any exception (including `ExitCode`) thrown by the given action (usually your `main` function).
        - The caught exception is available by `prException` function (except `ExitCode`).
- :new: Add `withEnv` function.

# 0.1.0.0

- Initial release.
