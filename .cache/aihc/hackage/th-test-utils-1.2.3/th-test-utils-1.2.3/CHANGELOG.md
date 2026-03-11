# v1.2.3

* Support GHC 9.14

# v1.2.2

* Support GHC 9.6 - 9.12

# v1.2.1

* Support GHC 9.4

# v1.2.0

* Drop support for GHC < 8.10

# v1.1.1

* Support GHC 9.2

# v1.1.0

* Rewrite with `runTestQ`, allowing for both recoverable `Q` actions and mocked `Q` actions in `IO`.

    The previous `tryQ'` function can be reimplemented as:

    ```hs
    tryQ' :: Q a -> Q (Either String a)
    tryQ' = tryTestQ unmockedState
    ```

    with the other helpers defined as before, using `tryQ'`.

# v1.0.2

* Support GHC 8.10

# v1.0.1

* Support GHC 8.8

# v1.0.0

Initial release:

* Add `tryQ`, `tryQErr`, `tryQErr'` functions
