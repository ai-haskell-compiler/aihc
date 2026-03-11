# FTP Client

ftp-client is a client library for the FTP protocol in Haskell.

# Examples

## Insecure
```haskell
withFTP "ftp.server.com" 21 $ \h welcome -> do
    print welcome
    login h "username" "password"
    print =<< nlst h []
```

## Secured with TLS
```haskell
withFTPS "ftps.server.com" 21 $ \h welcome -> do
    print welcome
    login h "username" "password"
    print =<< nlst h []
```
