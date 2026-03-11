# Changelog for `wled-json`

Hopefully all notable changes to this project will be documented in this file.

The project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

- Awesome upcoming features!

## 0.1.0.2 - Allow `rhine-1.6` - 2025-12-01

- Allow `rhine-1.6`.

## 0.1.0.1 - Keep compatibility with ghc 9.2/9.4 - 2025-10-26

- Keep compatibility with ghc 9.2/9.4.

## 0.1.0.0 - Support differently designed lamps - 2025-05-29

- Separate lamp specification from WLED interaction - so that own designs could be integrated.

## 0.0.1.1 - Small cosmetic improvements - 2025-03-28

- Demonstration:
  - Added menu and option to change wled device URL via terminal or command line argument
  - Added tool to traverse every single LED (to be able to better detect positions)
- Octolamp Support:
  - Added more flags with three vertical stripes

## 0.0.1.0 - Initial release - 2024-12-02

- Core Functionality:
  - Basic connection to WLED devices
  - Setting color, brightness, etc.
- Octolamp Support:
  - Initial support for Octolamp-specific features (provide the French flag)