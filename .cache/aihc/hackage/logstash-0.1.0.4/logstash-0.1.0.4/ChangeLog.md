# Changelog for logstash

## v0.1.0.4

- Compatibility with GHC 9.6

## v0.1.0.1

- Fixes a bug which caused `LogstashConnection`s from a `LogstashPool` to not get released properly in `runLogstashPool`
## v0.1

- First release with support for connecting to `tcp` (incl. TLS) Logstash inputs and the `json_lines` codec.
