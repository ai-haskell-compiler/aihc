# webex-teams-conduit

[![License: MIT](https://img.shields.io/badge/License-MIT-brightgreen.svg)](https://opensource.org/licenses/MIT)
[![Build Status](https://github.com/nshimaza/webex-teams-api/workflows/build/badge.svg)](https://github.com/nshimaza/webex-teams-api/actions?query=workflow%3Abuild)
[![Hackage](https://img.shields.io/hackage/v/webex-teams-conduit.svg?style=flat)](https://hackage.haskell.org/package/webex-teams-conduit)
[![Stackage Nightly](http://stackage.org/package/webex-teams-conduit/badge/nightly)](http://stackage.org/nightly/package/webex-teams-conduit)
[![Stackage LTS](http://stackage.org/package/webex-teams-conduit/badge/lts)](http://stackage.org/lts/package/webex-teams-conduit)

Conduit wrapper of webex-teams-api.

Webex-teams-conduit is thin wrappers of list API.  It transform chunky response
from list API into seamless stream of elements.

A Haskell bindings for Cisco Webex Teams (formerly Cisco Spark) API

This package also provides some sample usage in command line application style.
See source under app directory of the source package.

### Sample Usage

Following example is calling List Membership API which returns membership between
Rooms and users (Person).  You can extract each Membership from
Conduit pipe.  The `streamListWithFilter`, provided by webex-teams-conduit,
automatically performs pagenation when it is asked more element and last
response had link of subsequent page in HTTP Link Header.

```haskell
    let auth   = Authorization "your authorization token"
        filter = MembershipFilter yourRoomId Nothing Nothing
    runConduit $ streamListWithFilter auth def filter .| takeC 200 .| mapM_C print
```

You can find more examples in app/Main.hs.

### Limitation

- Relative reference in Link Header is not recognized as next page
