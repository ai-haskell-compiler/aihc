## 0.6.0.8
* Metadata and documentation update.

## 0.6.0.7
* base-4.20 support.
* Drop support for aeson < 2.

## 0.6.0.5
* Package metadata update.

## 0.6.0.4
* base-4.18 support.

## 0.6.0.3
* Update parsing error message.
* Document `Lines` constructor further.
* Show stackage distributions in readme.

## 0.6.0.2
* Accept null values for `entryMessage`, returning `Nothing`.

## 0.6.0.1
* Fix build with aeson < 2.

## 0.6.0.0
* Add support for bytestring messages in `entryMessage`.

## 0.5.0.0
* Fix for field: `entryMessage`. This field is not always present.

## 0.4.0.0
* Changed arguments of `entryStream`. It now takes a value of type
  `StreamStart`. This value can be used to indicate where the stream
  will start. The unit filter argument is gone. Instead, use conduit
  functions to filter the stream.
* New instances for `Cursor`: `Ord`, `ToJSON`.

## 0.3.0.0
* Fix for fields: `entryPID`, `entryProcess`, `entryExecutable`.
  They are optional now, as they should have been.

## 0.2.0.0
* Added new fields: `entryNamespace`, `entryProcess`, `entryExecutable`.

## 0.1.0.0
* First release.
