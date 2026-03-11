# Change Log

All notable changes to this package are documented in this file. This project adheres to [Haskell PVP](https://pvp.haskell.org/) versioning.

## 0.3.0.2

- #43, Fix spaces not allowed in relative query and singular query segments
- #50, Fix wrong normalized path with descendant segment
- #30, Fix escape characters not handled properly in normalized path

## 0.3.0.1

- #31, Fix `test/cts.json` not included in hackage bundle

## 0.3.0.0

- Always return `Vector Value`
- Implement Filter Selector
- Introduce JSONPath Standard Compliance Testing, See: [test-suite](https://github.com/jsonpath-standard/jsonpath-compliance-test-suite)
- Add function `queryLocated` which also returns node locations

## 0.2.0.0

- Remove dependency on `protolude`
- Fix parsing bug with Wildcard Selector
- Implement Descendant Segment
- Fix allowed characters in the `member-name-shorthand`
- Add `QuasiQuoter` for compile-time syntax checking

## 0.1.0.0

- Initial Release
