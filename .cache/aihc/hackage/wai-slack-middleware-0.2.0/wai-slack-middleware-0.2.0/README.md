# wai-slack-middleware

[![Build Status](https://travis-ci.org/psibi/wai-slack-middleware.svg?branch=master)](https://travis-ci.org/psibi/wai-slack-middleware)

A Slack middleware for [wai](https://www.stackage.org/package/wai)
Used for logging the request information into Slack from a WAI
application via middleware.

## Usage:

Settings are controlled via the type `SlackConfig`:

``` haskell
import Network.HTTP.Types.Status
let slackConfig = SlackConfig {
    webHookUrl = "https://hooks.slack.com/services/xxx/xxxxxxxx",
    httpManager = appHttpManager foundation,
    responseFilter = \resp -> status400 == responseStatus resp,
    requestFilter = \_ -> True
}
```

The above configuration will send slack notification for all 400 http
status code.

### Integrating with yesod scaffolding templates

Go to `Application.hs` and change the function `makeApplication` to
something like this:

``` haskell
makeApplication :: App -> IO Application
makeApplication foundation = do
  logWare <- makeLogWare foundation
  let slackConfig = SlackConfig {
                               webHookUrl = "https://hooks.slack.com/services/xxxx/xxxxxxx",
                               httpManager = appHttpManager foundation,
                               responseFilter = \resp -> status400 == responseStatus resp,
                               requestFilter = \_ -> True
                             }
  -- Create the WAI application and apply middlewares
  appPlain <- toWaiAppPlain foundation
  return $ slack slackConfig $ logWare $ defaultMiddlewaresNoLogging appPlain
```

Ref:

* [Slack messages API](https://api.slack.com/docs/messages)
