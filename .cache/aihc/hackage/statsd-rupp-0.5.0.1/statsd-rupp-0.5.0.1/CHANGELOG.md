# Changelog for `statsd-rupp`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## 0.5.0.1 - 2025-05-09

- Bump LTS Haskell.
- Simplify author name.

## 0.5.0.0 - 2024-05-22

- Fix namespace not showing up in samples.

## 0.4.0.4 - 2023-09-11

- Relax constraints on values to avoid runtime errors.

## 0.4.0.3 - 2023-09-11

- Show invalid value and key.

## 0.4.0.2 - 2023-09-05

- Flush interval cannot be zero or below.

## 0.4.0.1 - 2023-09-05

- Improve resilience against bad input.

## 0.4.0.0 - 2023-09-04

- Use vectors where pertient.
- Use bytestrings internally.
- Change configuration parameters.
- Code and test refactoring.

## 0.3.0.1 - 2023-09-02

- Ignore IO errors when sending UDP packets to the network.

## 0.3.0.0 - 2023-09-02

- Throw errors instead of returning Maybe when creating metrics sharing a key.

## 0.2.0.0 - 2023-09-02

- Fix gauges.

## 0.1.0.0 - 2023-09-02

- Initial release.
