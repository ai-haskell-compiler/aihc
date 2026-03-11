# aws-xray-client-wai

A Haskell client for integrating AWS X-Ray with [WAI](https://hackage.haskell.org/package/wai).

To configure this, you will need to configure a `import Network.AWS.XRayClient.WAI.XRayClientConfig)`. You can use `xrayClientConfig :: Text -> XRayClientConfig` to create a default configuration, given just an app name.

`xrayTraceMiddleware` then accepts `XRayClientConfig` to create [WAI middleware](https://www.yesodweb.com/book/web-application-interface#web-application-interface_middleware).
