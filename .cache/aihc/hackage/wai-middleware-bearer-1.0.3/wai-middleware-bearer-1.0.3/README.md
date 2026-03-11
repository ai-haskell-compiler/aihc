# wai-middleware-bearer

WAI middleware providing [bearer token authentication](https://swagger.io/docs/specification/authentication/bearer-authentication/).
## Usage example

The following code shows a secure **Hello World** application:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai.Middleware.BearerTokenAuth
import Network.Wai
import Network.HTTP.Types.Status (status200)
import Network.Wai.Handler.Warp (run)

myApp :: Application
myApp req rsp = rsp $ responseLBS status200 [] "Hello World"

secureApp :: Application
secureApp = tokenListAuth ["abc", "123"] myApp

main :: IO ()
main = run 3000 secureApp
```

Valid token request example (200 OK):
```sh
$ curl -H "Authorization: bearer abc" 'localhost:3000'

Hello World⏎
```

Invalid token request example (401 Unauthorized):
```sh
$ curl -H "Authorization: bearer otherToken" 'localhost:3000'

Bearer token authentication is required⏎ 
```

Missing token request example (401 Unauthorized):
```sh
curl 'localhost:3000'

Bearer token authentication is required⏎
```
