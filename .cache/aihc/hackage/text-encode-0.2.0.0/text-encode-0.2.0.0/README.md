# text-encode

Classes and newtypes for deriving uniform textual encodings.

This repository hosts seven packages.
The core package is `text-encode`.
Subsidiary packages are provided in order to avoid unnecessary dependencies in downstream packages.
The subsidiary packages re-export the modules from the core package, so there is no need to import the core package if you are importing one or more of the subsidiary packages.

- `text-encode-aeson`
- `text-encode-cassava`
- `text-encode-http-api-data`
- `text-encode-persistent`
- `text-encode-postgresql-simple`
- `text-encode-sqlite-simple`

See the module documentation in `Text.Encode` for usage.

## Contribute

We use Fourmolu and Hlint, each with their default configuration.
