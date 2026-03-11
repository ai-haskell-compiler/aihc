# IPv6DB

## A RESTful Web service for IPv6 related data ![CI](https://github.com/MichelBoucey/IPv6DB/actions/workflows/haskell-ci.yml/badge.svg) [![Hackage](https://img.shields.io/hackage/v/IPv6DB.svg)](https://hackage.haskell.org/package/IPv6DB)

IPv6DB is a RESTful microservice using Redis as backend to store lists of IPv6 addresses and attach to each of them any valuable data in a schema-free valid JSON value. Each resource can be permanent or TTLed.

```bash
[user@box ~]$ ipv6db --help
IPv6DB v0.3.3.1 APIv1, (c) Michel Boucey 2017-2024

Usage: ipv6db [-h|--host ARG] [-p|--port ARG] [-l|--log-file ARG]
              [-o|--redis-host ARG] [-r|--redis-port ARG]
              [-d|--redis-database ARG] [-a|--redis-auth ARG]
  RESTful Web Service for IPv6 related data

Available options:
  -h,--host ARG            Alternative host (default: "::")
  -p,--port ARG            Alternative listening port (default: 4446)
  -l,--log-file ARG        Log file (default: "/var/log/ipv6db.log")
  -o,--redis-host ARG      Redis host (default: "localhost")
  -r,--redis-port ARG      Redis listening port (default: 6379)
  -d,--redis-database ARG  Redis database (default: 0)
  -a,--redis-auth ARG      Redis authentication password
  -h,--help                Show this help text
```

A resource example:

```json
    {
      "list": "black",
      "address": "abcd::1234",
      "ttl": 34582,
      "source":
        {
          "services": [25,587,143]
        }
    }
```

The field "source" is mandatory and carry any valid JSON value.

The field "ttl" is optional in API requests for a permanent resource.

See the full [IPv6DB APIv1](https://github.com/MichelBoucey/IPv6DB/blob/master/IPv6DB_APIv1.md).

The package includes binary and library.
