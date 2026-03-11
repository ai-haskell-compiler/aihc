# ws

A really trivial websocket client for debugging and testing.

## Installation / Building

```bash
git clone http://github.com/athanclark/ws
```

Then depending on your build system:

```bash
stack install
```

or

```bash
cabal install
```

> It _should_ work with either, but if you run into issues, don't hesitate to
> file an issue.


## Usage

```
ws ws://echo.websocket.org/

ws://echo.websocket.org/> test
test
ws://echo.websocket.org/> ayoo
ayoo
```
