# keep-alive

This module allows you to set per-connection keep alive parameters on windows and linux enviroments.
For more information on keep alive signals see <https://en.wikipedia.org/wiki/Keepalive>.
See also <https://tldp.org/HOWTO/html_single/TCP-Keepalive-HOWTO/> for a linux specific implementation.

The module is meant to be used in conjuction with the "network" package. However, in order to ensure adaptability, all functions require
a socket file descriptor instead of an implementation dependent socket type. For the network package, such a  descriptor can be obtained
with the withFdSocket function:

```hs
 -- sock is a Socket type
 withFdSocket sock $ \fd -> do
    before <- getKeepAliveOnOff fd
    print before -- False
    -- set keep alive on, idle 60 seconds, interval 2 seconds
    rlt <- setKeepAlive fd $ KeepAlive True 60 2
    case rlt of
        Left err -> print err
        Right () -> return ()
    after <- getKeepAliveOnOff fd
    print after -- True
```

Please note that only the envocing process can manipulate sockets based on their file descriptors.
