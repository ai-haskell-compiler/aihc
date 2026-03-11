0.8.0
-----------------------------------------------------------------------------
- Require non-empty `ByteString`s in `setZapDomain` (#63)

0.7.0
-----------------------------------------------------------------------------
- Add `setZapDomain` (thanks to Alain O'Dea).
- `ByteString`s returned from the following socket option getters will no
  longer include the trailing NUL byte from the original C string:
    * `plainUserName`
    * `plainPassword`

0.6.7
-----------------------------------------------------------------------------
- Bugfix release.

0.6.6
-----------------------------------------------------------------------------
- Declare `zmq_ctx_term` FFI import as safe.

0.6.5
-----------------------------------------------------------------------------
- `MonadBase` and `MonadBaseControl` instances for ZMQ (by Maciej Woś).

0.6.4
-----------------------------------------------------------------------------
- Update dependencies.

0.6.3
-----------------------------------------------------------------------------
- Make internal modules available.
- Typeable instance for `Context`.
- Update dependencies.

0.6.2
-----------------------------------------------------------------------------
- Bug fixes: #56 (we no longer call zmq_msg_close after successfull sends)

0.6.1
-----------------------------------------------------------------------------
- Bug fixes: #55
- Build fixes for GHC versions < 7.6

0.6
-----------------------------------------------------------------------------
- Update to `exceptions` 0.6

0.5.1
-----------------------------------------------------------------------------
- Constrain `exceptions` dependency to < 0.6

0.5
-----------------------------------------------------------------------------
- bugfix release (#44, PR #47) which exposes `DontWait` flag on Windows
- exports `socketMonitor`
- `Eq`, `Typable` and `Generic` instances of socket types

0.4.1
-----------------------------------------------------------------------------
- adjust dependencies constraints

0.4
-----------------------------------------------------------------------------
- update `exceptions` and rework tests

0.3.2
-----------------------------------------------------------------------------
- adjust dependencies constraints

0.3.1
-----------------------------------------------------------------------------
- preliminary Windows support (#8)

0.3
-----------------------------------------------------------------------------
- remove `MonadCatchIO-transformers`
- use `pkg-config` (except on Windows)

0.2
-----------------------------------------------------------------------------
- add `disconnect`

0.1
-----------------------------------------------------------------------------
- initial release supporting 0MQ 4.x
