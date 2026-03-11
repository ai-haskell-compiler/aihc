## 0.7.2.1
* Update default server type for new servers.
  The previous value became obsolete.

## 0.7.2.0
* Add `getTokenFromEnvThrow`.
* Add the `MissingToken` constructor to the `CloudException` type.

## 0.7.1.1
* Metadata update.
* Remove server type and image list webpages.

## 0.7.1.0
* Introduce `HasActions` typeclass.

## 0.7.0.0
* base-19 and base-20 support.
* Make hetzner-test executable.
* Fix typo in documentation.
* Server type and location price API update.
* Add support for OpenSUSE OS flavor.

## 0.6.0.0
* Add new fields to the `Image` type: `imageArchitecture`, `imageName`.
* Add image list webpage.

## 0.5.0.0
* Do not export internal fingerprint parser.
* Wrap `Fingerprint` with a newtype to provide custom
  `FromJSON` instance.
* Firewall support.
* Fix label parsing and rendering.

## 0.4.0.1
* Update links in documentation.

## 0.4.0.0
* Fix `getServerTypes`. It wasn't returning them all.
  Unfortunately, the type signature had to change.
* Add `hetzner-docs` executable.

## 0.3.0.0
* Added support for the following server actions:
  - `setServerReverseDNS`
  - `powerOnServer`
  - `powerOffServer`
  - `shutdownServer`
  - `rebootServer`
  - `changeServerType`
* Removed `getActions`, as it has been discontinued by Hetzner.

## 0.2.1.1
* base-4.18 support.

## 0.2.1.0
* Add support for primary IPs.
* Add function to set reverse DNS for a primary IP.
* Add test.

## 0.2.0.0
* Allow to attach servers to networks on creation.
* Modify `streamPages` to cover more cases.
* Add support for DNS operations on zones and records.

## 0.1.2.0
* New function: `getTokenFromEnv`. This function allows the user
  to obtain a token from the `HETZNER_API_TOKEN` environment variable.
* Support for (private) networks.
* New instances for the `Token` type: `IsString`, `Show`, `Eq`, `Ord`.

## 0.1.1.0
* Added support for volumes.
* Fixed parsing of HTTP 204 responses.

## 0.1.0.0
* Initial release.
