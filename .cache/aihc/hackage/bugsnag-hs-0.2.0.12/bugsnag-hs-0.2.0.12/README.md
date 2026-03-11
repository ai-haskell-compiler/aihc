# bugsnag-hs

A [Bugsnag][] client for Haskell. Bugsnag is an application monitoring and error reporting tool. This library provides an API for sending reports to the [Bugsnag error reporting API][].

The library is low-level and provides a faithful representation of version 5 of the [Bugsnag error reporting API][] payload. Almost all documentation comments in this code come from that API's documentation. The intent is for this library to make no assumptions about the structure of the application using it.

[bugsnag]: https://www.bugsnag.com/
[bugsnag error reporting api]: https://bugsnagerrorreportingapi.docs.apiary.io/#reference
