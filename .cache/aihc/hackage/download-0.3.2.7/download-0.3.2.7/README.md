# download

[![Build Status](https://travis-ci.org/psibi/download.svg?branch=master)](https://travis-ci.org/psibi/download)

 Download web content as strict bytestring, strings, HTML tags, XML, RSS
 or Atom feeds or JSON, via HTTP, FTP or file protocols, using a URL
 interface.

## Using the library:

 Importing the library:

``` haskell
import Network.Download
```

 Loading a webpage as a `ByteString`:

``` haskell
doc  <- openURI "http://google.com"
```

 Loading from a file:

``` haskell
doc  <- openURI "file:///tmp/A.hs"
```

 Loading a HTML page as a list of tags:

``` haskell
tags <- openAsTags "http://google.com"
```

 Loading a HTML page as XML:

``` haskell
tags <- openAsXML "http://google.com"
```

 Loading an RSS or Atom feed:

``` haskell
feed <- openAsFeed "http://google.com"
```

 These data types can the be processed further with the XML, Feed and
 TagSoup libraries.

