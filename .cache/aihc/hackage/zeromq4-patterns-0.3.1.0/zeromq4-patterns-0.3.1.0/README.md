zeromq4-patterns
================

[![Travis](https://travis-ci.org/hverr/zeromq4-patterns.svg?branch=master)](https://travis-ci.org/hverr/zeromq4-patterns)
[![Hackage](https://img.shields.io/hackage/v/zeromq4-patterns.svg?maxAge=2592000)](https://hackage.haskell.org/package/zeromq4-patterns)
[![Stackage Nightly](http://stackage.org/package/zeromq4-patterns/badge/nightly)](http://stackage.org/nightly/package/zeromq4-patterns)
[![Stackage LTS](http://stackage.org/package/zeromq4-patterns/badge/lts)](http://stackage.org/lts/package/zeromq4-patterns)

Haskell implementation of several ZeroMQ patterns that you can find in the [official ZeroMQ guide][zeromq-guide]

  [zeromq-guide]: http://zguide.zeromq.org/

## Implementations

- [`System.ZMQ4.Patterns.Clone`][clone-module]: [ZeroMQ Reliable Pub-Sub (Clone) pattern][zeromq-clone]
- [`System.ZMQ4.Patterns.RequestReply`][requestreply-module]: [ZeroMQ Request-Reply pattern][zeromq-requestreply]

  [clone-module]: https://hackage.haskell.org/package/zeromq4-patterns/docs/System-ZMQ4-Patterns-Clone.html
  [zeromq-clone]: http://zguide.zeromq.org/page:all#Reliable-Pub-Sub-Clone-Pattern

  [requestreply-module]: https://hackage.haskell.org/package/zeromq4-patterns/docs/System-ZMQ4-Patterns-RequestReply.html
  [zeromq-requestreply]: http://zguide.zeromq.org/page:all#Ask-and-Ye-Shall-Receive
