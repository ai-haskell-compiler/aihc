module Main where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Client.Overrides (withHttpClientOverridesThrow)

main :: IO ()
main = do
    settings <- withHttpClientOverridesThrow tlsManagerSettings
    manager  <- newManager settings
    request  <- parseRequest "https://unreachable.domain"
    response <- httpLbs request manager
    print $ responseStatus response
