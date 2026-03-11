Readable
========

The Read type class is very useful for building data types from String
representations.  But String has high overhead, so sometimes it isn't suitable
for applications where space usage and performance are important.  This
library provides a simpler version of Read's functionality for Text and UTF8
encoded ByteStrings.

