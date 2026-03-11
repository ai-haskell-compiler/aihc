See also http://pvp.haskell.org/faq

### 0.1.7.2

* Fix compilations with `-Werror={unused-packages,incomplete-uni-patterns}`.

Tested with GHC 8.0 - 9.14 alpha1.

### 0.1.7.1

* Drop support for GHC 7.
* Drop unused dependency `system-filepath`.

Tested with GHC 8.0 - 9.12.2.

### 0.1.7.0

* New function `voidContextSSL` for creating a _void_ SSL Context which rejects any TLS handshake attempts
* New function `contextSetCASystemStore` exposing functionality embedded in `baselineContextSSL`

NB: These functions were originally introduced in the retroactively deprecated 0.1.6.0 release but due to an oversight were dropped again in 0.1.6.1 inadvertently. This minor release reintroduces them in a PVP-compliant way.

#### 0.1.6.4

* Depend on `directory` rather than `system-fileio` ([PR #18](https://github.com/haskell-hvr/http-io-streams/pull/18)).

Tested with GHC 8.0 - 9.8.

#### 0.1.6.3

* Test-suite: add dependency `attoparsec-aeson` for `aeson-2.2`.

Tested with GHC 8.0 - 9.8.

#### 0.1.6.2

* Allow `mtl-2.3` (no code change).
* Run test-suite via `cabal test`.

Tested with GHC 7.4 - 9.6.

#### 0.1.6.1 revision 1

* Allow `text-2.0` (no code change).

#### 0.1.6.1

* Build with GHC 9.2 and `ghc-prim-0.8` (via `base-4.16`).
* Accidentally removed function `voidContextSSL` and `contextSetCASystemStore` introduced in 0.1.6.0

### 0.1.6.0 **deprecated**

* New function `voidContextSSL` for creating a _void_ SSL Context which rejects any TLS handshake attempts
* New function `contextSetCASystemStore` exposing functionality embedded in `baselineContextSSL`
* New function `openConnectionAddress''` supporting supplying local `SSLContext`s as well as modifying the `SSL` connection before initiating the client SSL handshake.
* New function `openConnectionSSL'` which allows to customize the SSL connection _before_ a client SSL handshake is attempted.
* New convenience function `getContextSSL` function allowing to retrieve global `SSLContext`.

### 0.1.5.0

* New function `openConnectionAddress'` function supporting supplying local `SSLContext`s.

### 0.1.4.0

* New module `Network.Http.Client.WebSocket` providing basic RFC6455 support.
* New function `inputStreamBodyChunked` supporting breaking up over-sized chunks.

### 0.1.3.0

* New functions `receiveUpgradeResponse`, `receiveConnectResponse`, and `unsafeWithRawStreams` for accessing full-duplex low-level streams (e.g. for upgrading to Websockets protocol).
* New function `makeConnection` for constructing a `Connection` object over custom streams.

### 0.1.2.0

* New functions `unsafeReceiveResponse` and `unsafeReceiveResponseRaw` that do not automatically skip to end-of-stream.

### 0.1.1.0

* New alternative connection-setup API (`ConnectionAddress` et al.).
* New function `getHeaderMap` for exporting all response headers at once.
* Add convenience functions `bytestringBody`, `lazyBytestringBody`, `utf8TextBody`, `utf8LazyTextBody`.
* Add support for Brotli HTTP compression.

## 0.1.0.0

* First version. Released on an unsuspecting world.
* Derived from `http-streams-core-0.8.6.1` & `http-common-0.8.2.0`.
