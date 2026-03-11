# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## 0.4.2
### Adedd
- Forgot to add retrieval using column families.

## 0.4.1
### Added
- Support column families.

## 0.4.0
### Changed
- Depend on my new Haskell bindings.

## 0.3.2
### Changed
- Use MIT license.
- Udate dependencies.

## 0.3.1
### Changed
- List functions do not use conduits.
- Reuse serialized version of key prefix.
- Avoid building extra bytestring for key prefix comparison.

## 0.3.0
### Changed
- Make conduits polymorphic on input parameter.

## 0.2.0
### Changed
- Accept read options instead of snapshot for read operations.

## 0.1.4
### Changed
- Documentation improvements.

## 0.1.3
### Changed
- Reduce documentation verbosity and correct wording.

## 0.1.2
### Added
- Documentation for all functions.

## 0.1.1
### Changed
- Separate package dependencies.

## 0.1.0
### Added
- Typeclasses for key/value pairs, base keys and key seekers.
- Functions for creating, updating, deleting records.
- Functions for retrieving multiple records.
- Fuunctions for streaming records.
