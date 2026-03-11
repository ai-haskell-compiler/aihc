# Revision history for leb128-serialize

## 1.2 -- 2020-11-20

* Add `Data.Serialize.LEB128.Lenient` for a decoder that
  allows overlong encodings.
  For now, a simple code copy; may be refactored later.

## 1.1 -- 2020-06-08

* Fix check for overlong encodings; it was not strict enough

## 1.0 -- 2020-04-23

* First version. Released on an unsuspecting world.
