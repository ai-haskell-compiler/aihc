# port-utils

## openFreePort

This is another version of `warp`'s `openFreePort` function. This function has fewer dependencies than the `warp` version.

`openFreePort` returns a socket on a random port and the port it has been bound to.

```haskell
openFreePort :: IO (Int, Socket)
```

## wait

`wait` will attempt to connect to a host and port until it is successful. Between each unsuccessful attempt, it sleeps for 10 ms.

Here is an example of the primary function:

```haskell
import Network.Socket.Wait (wait)

void $ forkIO $ Warp.run 7000 app
-- Wait for the server to start listening on the socket
wait "127.0.0.1" 7000
-- Communicate with the server
...
```

In bash one could write:

```bash
while ! nc -z localhost 7000 ; do sleep 0.01 ; done
```

The bash script above was copied from this stackoverflow answer https://stackoverflow.com/a/50008755