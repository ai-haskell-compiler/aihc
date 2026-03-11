# webex-teams-api

[![License: MIT](https://img.shields.io/badge/License-MIT-brightgreen.svg)](https://opensource.org/licenses/MIT)
[![Build Status](https://github.com/nshimaza/webex-teams-api/workflows/build/badge.svg)](https://github.com/nshimaza/webex-teams-api/actions?query=workflow%3Abuild)
[![Hackage](https://img.shields.io/hackage/v/webex-teams-api.svg?style=flat)](https://hackage.haskell.org/package/webex-teams-api)
[![Stackage Nightly](http://stackage.org/package/webex-teams-api/badge/nightly)](http://stackage.org/nightly/package/webex-teams-api)
[![Stackage LTS](http://stackage.org/package/webex-teams-api/badge/lts)](http://stackage.org/lts/package/webex-teams-api)

A Haskell bindings for Cisco Webex Teams (formerly Cisco Spark) API

Webex-teams-api (formerly cisco-spark-api) package provides types and functions
for accessing Cisco Webex Teams REST API.  Details of the API is available
from [developer site of Cisco Webex](https://developer.webex.com/)

This package is designed to improve type safety over the API.  Each entity is
separately typed.  JSON messages contained in REST responses are decoded into
appropriate type of Haskell record.  JSON messages sent in REST requests are
encoded only from correct type of record.

Some Webex Teams REST API return list of objects.  Those APIs require HTTP Link
Header based pagination.  Haskell functions for those APIs automatically request
subsequent pages as needed.  This repo provides optional packages for streaming
response of list API.  Webex-teams-conduit and webex-teams-pipes are thin
wrappers of list API.  They transform chunky response from list API into
seamless stream of elements.

This package also provides some sample usage in command line application style.
See source under app directory of the source package.

### Sample Usage

Sending a message to a Room.

```haskell
    let auth        = Authorization "your authorization token"
        roomId      = RoomId "Room ID your message to be sent"
        messageText = MessageText "your message"
        message     = CreateMessage (Just roomId) Nothing Nothing (Just messageText) Nothing Nothing
    createEntity auth def createMessage >>= print . getResponseBody
```

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

You can find more examples in app/Main.hs under webex-teams-pipes or
webex-teams-conduit.

### Support for Lens

This package provides many of records representing objects communicated via
Webex Teams REST API.  Those records are designed to allow create lenses by
Control.Lens.TH.makeFields.

Following example creates overloaded accessors for 'Person', 'Room' and 'Team'.

```haskell
makeFields ''Person
makeFields ''Room
makeFields ''Team
```

You can access 'personId', 'roomId' and 'teamId' via overloaded accessor
function 'id' like this.

```haskell
    let yourPersonId = yourPerson ^. id
        yourRoomId = yourRoom ^. id
        yourTeamId = yourTeam ^. id
```

This package does not provide pre-generated lenses for you because not everyone
need it but you can make it by yourself so easily as described.

### Limitation

- WebHook API is not yet implemented.
- Relative reference in Link Header is not recognized as next page
