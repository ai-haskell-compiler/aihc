0.2.1.3 / 2020-09-25
--------------------

Two changes by Simon Hengel/@sol
- Fix a concurrency bug
- Cleanups / proper test suite declarations

0.2.1.2 / 2017-11-26
--------------------

- Remove version dependencies from cabal file, to be handled by the stackage ecosystem henceforth

0.2.1.1 / 2017-10-24
--------------------

- Create an index to enhance session expiration performance

0.2.1.0 / 2016-05-27
--------------------

- Make compatible with ghc8

0.2.0.4 / 2016-01-09
--------------------

- Make compatible with the newest versions of wai (3.2.0) and time (1.6)

0.2.0.1 / 2016-01-03
--------------------

- Add an instance declaration for Data.Pool connection pools, so they can be used directly with wai-postgresql-session

0.2.0.0 / 2016-01-02
--------------------

- Use two PostgreSQL tables, store each session id/key pair in its own db row

0.1.1.1 / 2016-01-01
--------------------

- Make compatible with postgresql-simple-0.5
- Add instance for Data.Default
- Improve example

0.1.1.0 / 2015-11-25
--------------------

- Add clearSession function force assigning a new session/session ID
