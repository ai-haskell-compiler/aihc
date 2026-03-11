# IPv6DB APIv1

## A RESTful Web service for IPv6-related data

- Default Web service port: 4446

- IPv6 addresses have just to be valid ones, but they are
  stored and returned canonized in conformation with RFC 5952.

- The field "source" takes a schema-free JSON value.

- In case of success, POST, PUT and DELETE returns a HTTP status 204 "No Content".

- The field "ttl" is optional (default to null).

### /ipv6db/v1/list/\{*listName*\}/addresses/{*IPv6Address*}

__POST /ipv6db/v1/list/\{__*listName*__\}/addresses/\{__*IPv6Address*__\}__

Creates a resource.

    POST /ipv6db/v1/list/hosts/addresses/abcd::1234

```json
    {
      "ttl": 3600,
      "source":
        {
          "services": ["smtp","imap"]
        }
    }
```

__PUT /ipv6db/v1/list/\{__*listName*__\}/addresses/\{__*IPv6Address*__\}__

Updates a resource.

```
    PUT /ipv6db/v1/list/hosts/addresses/abcd::1234
```

```json
    {
      "ttl": 3600,
      "source":
        {
          "services": ["smtp","imap","ssh"]
        }
    }
```

Response:

If successful, returns HTTP Status 204 No Content

__GET /ipv6db/v1/list/\{__*listName*__\}/addresses/\{__*IPv6Address*__\}__

```
    GET /ipv6db/v1/list/hosts/addresses/abcd::1234
```

Response:

```json
    {
      "list": "hosts",
      "address": "abcd::1234",
      "ttl": null,
      "source":
        {
          "services": ["smtp","imap","ssh"]
        }
    }
```

__DELETE /ipv6db/v1/list/\{__*listName*__\}/addresses/\{__*IPv6Address*__\}__

```
    DELETE /ipv6db/v1/hosts/addresses/abcd::1234
```

Response:


### /ipv6db/v1/list/\{*listName*\}/addresses

__POST /ipv6db/v1/list/\{__*listName*__\}/addresses__

Creates many resources that belongs to the given list.

```json
    POST /ipv6db/v1/hosts/addresses
    [
      {
        "address": "abcd::1234",
        "ttl": null,
        "source":
          {
            "services": ["smtp","imap","ssh"]
          }
      },
      {
        "address": "abcd::1235",
        "ttl": null,
        "source":
          {
            "services": ["http","https","ssh"]
          }
      }
    ]
```

__PUT /ipv6db/v1/list/\{__*listName*__\}/addresses__

Updates many resources that belongs to the given list.

```json
    PUT /ipv6db/v1/hosts/addresses
    [
      {
        "address": "abcd::1234",
        "ttl": null,
        "source":
          {
            "services": ["smtp","imap","ssh"]
          }
      },
      {
        "address": "abcd::1235",
        "ttl": null,
        "source":
          {
            "services": ["http","https","ssh"]
          }
      }
    ]
```

__GET /ipv6db/v1/list/\{__*listName*__\}/addresses__

Gets many resources from the list based on a JSON array of IPv6 addresses.

```json
    GET /ipv6db/v1/hosts/addresses
    [
      "abcd::1234",
      "abcd::1235"
    ]
```

__DELETE /ipv6db/v1/list/\{__*listName*__\}/addresses__

Deletes many resources from the list based on a JSON array of IPv6 addresses.

```json
    DELETE /ipv6db/v1/hosts/addresses
    [
      "abcd::1234",
      "abcd::1235"
    ]
```

### /ipv6db/v1/batch

__PUT /ipv6db/v1/batch__

Updates many resources possibly from different lists based on a JSON array.

```json
    PUT /ipv6db/v1/batch
    [
      {
        "list": "hosts",
        "address": "abcd::1234",
        "ttl": null,
        "source":
          {
            "services": ["smtp","imap"]
          }
      },
      {
        "list": "blacklist",
         "address": "bad::1235",
         "ttl": 604800,
         "source": null
      }
    ]
```

__GET /ipv6db/v1/batch__

Gets many resources possibly from different lists based on a JSON array.

```json
    GET /ipv6db/v1/batch
    [
      {
        "list": "hosts",
        "address": "abcd::1234"
      },
      {
        "list": "blacklist",
        "address": "bad::1234"
      }
    ]
```

__DELETE /ipv6db/v1/batch__

Deletes many resources possibly from different lists based on a JSON array.

```json
    DELETE /ipv6db/v1/batch
    [
      {
        "list": "hosts",
        "address": "abcd::1234"
      },
      {
        "list": "backlist",
        "address": "bad::1234"
      }
    ]
```

