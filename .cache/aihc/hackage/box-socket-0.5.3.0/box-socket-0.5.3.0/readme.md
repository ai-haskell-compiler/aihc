[![img](https://img.shields.io/hackage/v/box-socket.svg)](https://hackage.haskell.org/package/box-socket) [![img](https://github.com/tonyday567/box-socket/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/tonyday567/box/actions/workflows/haskell-ci.yml)

Socket API based on the box library, with websockets and TCP support.


# Usage

    :set -XOverloadedStrings
    import Box
    import Box.Socket.Types
    import Box.Websocket

IO client:

    clientBox defaultSocketConfig (CloseAfter 0) (stdBox "q")

IO server:

    serverBox defaultSocketConfig (CloseAfter 0) (stdBox "q")

See examples in Box.Websocket.Example and Box.TCP.Example for a variety of usage.


# Design

-   The API attempts to be similar for TCP and Websocket

-   A Codensity, continuation passing style is encouraged, similar to the box library.

