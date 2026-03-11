0.4
---
* Reinstate Text and ByteString instances because some users require them.

0.3.0.1
-------
* Add yes/no and y/n parsing to the Bool instance

0.3
---
* Remove Readable instances for ByteString and Text because they are probably
  not what the user wants and could cause compilation to succeed when you
  probably want to see an error.

0.2.0.2
-------
* Fix fromBS for ByteString so it is a correct passthrough instead of doing
  encodeUtf8 . decodeUtf8 under the hood.

0.2.0.1
-------
* Use decodeUtf8' so that we can catch character encoding exceptions and
  return mzero instead of generating an exception.
