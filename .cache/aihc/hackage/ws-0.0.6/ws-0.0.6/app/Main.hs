{-# LANGUAGE
    ScopedTypeVariables
  , FlexibleContexts
  , NamedFieldPuns
  , OverloadedStrings
  #-}

module Main where

import App (app)
import App.Types (Env (..), runAppM, handleInitException, InitException (..))
import Options.Applicative (Parser, strArgument, ParserInfo, metavar, help, fullDesc, progDesc, header, helper, info, execParser)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Strict.Maybe as Strict
import Data.Strict.Tuple (Pair (..))
import Data.URI (URI (..), parseURI, DirOrFile (Dir))
import Data.URI.Auth (URIAuth (..), printURIAuth)
import Data.Attoparsec.Text (parseOnly)

import Data.Monoid ((<>))
import Control.Monad.Catch (throwM, handle)
import Control.Applicative (optional)


-- * Options Parsing

-- | Application-wide options
newtype AppOpts = AppOpts
  { url :: Maybe String
  }

-- | Options for each field
appOpts :: Parser AppOpts
appOpts =
  AppOpts <$> urlOpt
  where
    urlOpt :: Parser (Maybe String)
    urlOpt = optional $ strArgument $
         metavar "TARGET"
      <> help "The websocket address to connect to - example:\
                \ `ws://localhost:3000/foo`"


-- | Options for entire app
opts :: ParserInfo AppOpts
opts = info (helper <*> appOpts) $
    fullDesc
 <> progDesc "Connect to a websocket"
 <> header "ws - a CLI websocket tool"



-- * Executable

main :: IO ()
main = do
  env <- handle handleInitException
       $ appOptsToEnv =<< execParser opts

  runAppM env app



-- | Translate the CLI parsed options to a sane type we can use in our app
appOptsToEnv :: AppOpts -> IO Env
appOptsToEnv (AppOpts mu) = do
  u <- case mu of
    Nothing -> do
      putStrLn "Websocket URI: "
      getLine
    Just x -> pure x
  case parseOnly parseURI (T.pack u) of
    Left _ -> throwM $ URIParseException u
    Right URI
      { uriScheme
      , uriAuthority = URIAuth
        { uriAuthUser
        , uriAuthPassword
        , uriAuthHost
        , uriAuthPort
        }
      , uriPath
      , uriQuery
      } ->
          let host = T.unpack $ printURIAuth URIAuth
                        { uriAuthUser
                        , uriAuthPassword
                        , uriAuthHost
                        , uriAuthPort = Strict.Nothing
                        }
              port = case uriAuthPort of
                Strict.Nothing
                  | uriScheme == Strict.Just "wss" -> 433
                  | otherwise -> 80
                Strict.Just p -> fromIntegral p
              path' = case uriPath of
                Strict.Nothing
                  -> Strict.Nothing
                Strict.Just (uriPath', dirOrFile)
                  -> Strict.Just $ T.unpack $
                        "/" <> T.intercalate "/" (V.toList uriPath') <>
                        ( case dirOrFile of
                            Dir -> "/"
                            _ -> ""
                        ) <>
                        ( if V.null uriQuery
                          then ""
                          else "?" <> T.intercalate "&" ((\(k :!: mv) -> k <> Strict.maybe "" ("=" <>) mv) <$> V.toList uriQuery)
                        )
          in  pure Env { envHost   = host
                       , envPort   = port
                       , envPath   = case path' of
                           Strict.Nothing -> ""
                           Strict.Just x -> x
                       , envSecure = uriScheme == Strict.Just "wss"
                       }
