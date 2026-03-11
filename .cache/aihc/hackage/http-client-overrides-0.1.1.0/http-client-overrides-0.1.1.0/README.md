# http-client-overrides

A library for applying overrides to [`ManagerSettings`][manager-settings]
when using the [`http-client`][http-client] Haskell library.

## Usage

You probably have some `IO` do notation hidden somewhere which looks like this:

```haskell
manager <- newManager tlsManagerSettings
```

Replace it with this:

```haskell
settings <- withHttpClientOverridesThrow tlsManagerSettings
manager  <- newManager settings
```

Set the `HTTP_CLIENT_OVERRIDES` environment variable to the location of your
configuration file. When this environment variable is not set there is no
additional overhead for HTTP requests as the `ManagerSettings` will not be
modified. Refer to the [example](./example/) directory for a sample Haskell
program and configuration file.

## Features

### Log HTTP requests and responses

The following fields can be used to print log messages for all HTTP responses,
requests and request overrides:

```yaml
version: v1
logOptions:
  responses: simple
  requests: simple
  requestOverrides: simple
```

The currently supported log formats are `simple` and `detailed`. Sample output
for the `simple` log format is shown below:

```
Overriding request: https://unreachable.domain/ -> https://github.com/
Request: GET https://github.com/ HTTP/1.1
Response: HTTP/1.1 200 OK
```

### Override HTTP requests

Match HTTP requests corresponding to a match URL and transform the request
according to an override URL:

```yaml
version: v1
requestOverrides:
- match: unreachable.domain
  override: github.com
```

#### Match semantics

Matches the first HTTP request which:
- has the same scheme, host and port as the match URL
- has a path which is a suffix of the path in the match URL

Any URL segments ommitted from the match URL are not required for matching.

#### Override semantics

Overrides an HTTP request with:
- the scheme, host and port specified in the override URL
- replaces the prefix of a matched path with the path in the override URL

Any URL segments ommitted from the override URL will not be applied to the HTTP
request.

#### Examples

Override all HTTP requests on port 80 to use HTTPS on port 443 (note: the port
is never automatically interpreted from the scheme):

```yaml
version: v1
requestOverrides:
- match: http://:80
  override: https://:443
```

Override all requests to the host `github.com` which have a path prefix of
`/dhall-lang/dhall-lang/` to use a path prefix of
`/robbiemcmichael/dhall-lang/` instead.

```yaml
version: v1
requestOverrides:
- match: https://github.com/dhall-lang/dhall-lang/
  override: https://github.com/robbiemcmichael/dhall-lang/
```

#### Debugging

Set `logOptions.requestOverrides` to `detailed` to see how the URLs have been
parsed and how any matched HTTP requests are being overridden.


[manager-settings]: https://hackage.haskell.org/package/http-client/docs/Network-HTTP-Client.html#t:ManagerSettings
[http-client]: https://hackage.haskell.org/package/http-client
