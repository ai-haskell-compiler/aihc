{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | A socket between a web page and haskell, based on the box library.
module Web.Rep.Socket
  ( socketPage,
    defaultSocketPage,
    SocketConfig (..),
    defaultSocketConfig,
    serveSocketBox,
    CodeBox,
    CoCodeBox,
    CodeBoxConfig (..),
    defaultCodeBoxConfig,
    codeBox,
    codeBoxWith,
    serveRep,
    serveRepWithBox,
    replaceInput,
    replaceOutput,
    replaceOutput_,
    sharedStream,
    PlayConfig (..),
    defaultPlayConfig,
    repPlayConfig,
    servePlayStream,
    servePlayStreamWithBox,
    Code (..),
    code,
    console,
    val,
    replace,
    append,
    clean,
    webSocket,
    refreshJsbJs,
    preventEnter,
    runScriptJs,
  )
where

import Box
import Box.Websocket (serverApp)
import Control.Category ((>>>))
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.State.Lazy
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C
import Data.Functor.Contravariant
import Data.HashMap.Strict as HashMap
import Data.Profunctor
import Data.Text (Text)
import Data.Text.Encoding
import FlatParse.Basic
import GHC.Generics
import MarkupParse
import Network.Wai.Handler.WebSockets
import Network.WebSockets qualified as WS
import Optics.Core hiding (element)
import Web.Rep.Bootstrap
import Web.Rep.Page
import Web.Rep.Server
import Web.Rep.Shared
import Web.Rep.SharedReps
import Web.Scotty (middleware, scotty)

-- | Page with all the trimmings for a sharedRep Box
socketPage :: Page
socketPage =
  mempty
    & #jsOnLoad
    .~ mconcat
      [ webSocket,
        runScriptJs,
        refreshJsbJs,
        preventEnter
      ]

-- | Bootstrapped base page for a web socket.
defaultSocketPage :: Page
defaultSocketPage =
  bootstrapPage
    <> socketPage
      & set #cssBody cssColorScheme
      & set
        #htmlBody
        ( element
            "div"
            [Attr "class" "container"]
            ( element
                "div"
                [Attr "class" "row"]
                (elementc "h1" [] "web-rep testing")
                <> element
                  "div"
                  [Attr "class" "row"]
                  ( mconcat $
                      ( \(t, h) ->
                          element
                            "div"
                            [Attr "class" "row"]
                            (element "h2" [] (elementc "div" [Attr "id" t] h))
                      )
                        <$> sections
                  )
            )
        )
  where
    sections =
      [ ("input", mempty),
        ("output", mempty)
      ]

-- | Socket configuration
--
-- >>> defaultSocketConfig
-- SocketConfig {host = "127.0.0.1", port = 9160, path = "/"}
data SocketConfig = SocketConfig
  { host :: Text,
    port :: Int,
    path :: Text
  }
  deriving (Show, Eq, Generic)

-- | official default
defaultSocketConfig :: SocketConfig
defaultSocketConfig = SocketConfig "127.0.0.1" 9160 "/"

-- | bidirectional websocket serving a 'Box'
serveSocketBox :: SocketConfig -> Page -> Box IO Text Text -> IO ()
serveSocketBox cfg p b =
  scotty (cfg ^. #port) $ do
    middleware $ websocketsOr WS.defaultConnectionOptions (serverApp b)
    servePageWith "/" (defaultPageConfig "") p

-- | A common Box pattern. [Code] is typically committed to the websocket and key-value elements, representing changes to the shared objects that are in the Dom are emitted.
type CodeBox = Box IO [Code] (ByteString, ByteString)

-- | Codensity CodeBox
type CoCodeBox = Codensity IO (Box IO [Code] (ByteString, ByteString))

-- | Configuration for a CodeBox serving.
data CodeBoxConfig = CodeBoxConfig
  { codeBoxSocket :: SocketConfig,
    codeBoxPage :: Page,
    codeBoxCommitterQueue :: Queue [Code],
    codeBoxEmitterQueue :: Queue (ByteString, ByteString)
  }
  deriving (Generic)

-- | official default config.
defaultCodeBoxConfig :: CodeBoxConfig
defaultCodeBoxConfig = CodeBoxConfig defaultSocketConfig defaultSocketPage Single Single

-- | Turn a configuration into a live (Codensity) CodeBox
codeBoxWith :: CodeBoxConfig -> CoCodeBox
codeBoxWith cfg =
  fromActionWith
    (view #codeBoxEmitterQueue cfg)
    (view #codeBoxCommitterQueue cfg)
    ( serveSocketBox (view #codeBoxSocket cfg) (view #codeBoxPage cfg)
        . dimap (either (C.unpack >>> error) id . runParserEither parserJ . encodeUtf8) (mconcat . fmap (decodeUtf8 . code))
    )

-- | Turn the default configuration into a live (Codensity) CodeBox
codeBox :: CoCodeBox
codeBox = codeBoxWith defaultCodeBoxConfig

-- | serve a SharedRep
serveRep :: SharedRep IO a -> (Markup -> [Code]) -> (Either ByteString a -> [Code]) -> CodeBoxConfig -> IO ()
serveRep srep i o cfg =
  serveRepWithBox srep i o <$|> codeBoxWith cfg

-- | non-codensity sharedRep server.
serveRepWithBox :: SharedRep IO a -> (Markup -> [Code]) -> (Either ByteString a -> [Code]) -> CodeBox -> IO ()
serveRepWithBox srep i o (Box c e) =
  sharedStream srep (contramap i c) (contramap o c) e

-- | Convert HTML representation to Code, replacing the input section of a page.
replaceInput :: Markup -> [Code]
replaceInput h = [Replace "input" (markdown_ Compact Html h)]

-- | Convert (typically parsed representation) to Code, replacing the output section of a page, and appending errors.
replaceOutput :: (Show a) => Either ByteString a -> [Code]
replaceOutput ea =
  case ea of
    Left err -> [Append "debug" err]
    Right a -> [Replace "output" (strToUtf8 $ show a)]

-- | Convert (typically parsed representation) to Code, replacing the output section of a page, and throwing away errors.
replaceOutput_ :: (Show a) => Either ByteString a -> [Code]
replaceOutput_ ea =
  case ea of
    Left _ -> []
    Right a -> [Replace "output" (strToUtf8 $ show a)]

-- | Stream a SharedRep
sharedStream ::
  (Monad m) => SharedRep m a -> Committer m Markup -> Committer m (Either ByteString a) -> Emitter m (ByteString, ByteString) -> m ()
sharedStream sr ch c e =
  flip evalStateT (0, HashMap.empty) $ do
    -- you only want to run unshare once for a SharedRep
    (Rep h fa) <- unshare sr
    b <- lift $ commit ch h
    when b (go fa)
  where
    go fa = do
      e' <- lift $ emit e
      case e' of
        Nothing -> pure ()
        Just (k, v) -> do
          hmap <- snd <$> get
          let hmap' = insert k v hmap
          let (hmap'', r) = fa hmap'
          modify (second (const hmap''))
          b <- lift $ commit c r
          when b (go fa)

-- * Play

-- | Configuration to control a (re)play of an emitter with a Gap (timing) element.
data PlayConfig = PlayConfig
  { playPause :: Bool,
    playSpeed :: Double,
    playFrame :: Int
  }
  deriving (Eq, Show, Generic)

-- | Start on pause at normal speed and at frame 0.
defaultPlayConfig :: PlayConfig
defaultPlayConfig = PlayConfig True 1 0

-- | representation of a PlayConfig
repPlayConfig :: PlayConfig -> SharedRep IO PlayConfig
repPlayConfig cfg =
  PlayConfig
    <$> repPause (view #playPause cfg)
    <*> repSpeed (view #playSpeed cfg)
    <*> repFrame (view #playFrame cfg)

-- | representation of the playFrame in a PlayConfig
repFrame :: Int -> SharedRep IO Int
repFrame x = read . utf8ToStr <$> textbox (Just "frame") (strToUtf8 $ show x)

-- | representation of the playSpeed in a PlayConfig
repSpeed :: Double -> SharedRep IO Double
repSpeed x = sliderV (Just "speed") 0.5 100 0.5 x

-- | representation of the playPause toggle in a PlayConfig
repPause :: Bool -> SharedRep IO Bool
repPause initial = toggle_ (Just "play/pause") initial

-- | representation of a Bool reset button
repReset :: SharedRep IO Bool
repReset = button (Just "reset")

-- | Serve an emitter controlled by a PlayConfig representation, with an explicit CodeBox.
servePlayStreamWithBox :: PlayConfig -> CoEmitter IO (Gap, [Code]) -> CodeBox -> IO ()
servePlayStreamWithBox pcfg pipe (Box c e) = do
  (playBox, _) <- toBoxM (Latest (False, pcfg))
  race_
    (sharedStream ((,) <$> repReset <*> repPlayConfig pcfg) (contramap (\h -> [Replace "input" (markdown_ Compact Html h)]) c) (witherC (either (const (pure Nothing)) (pure . Just)) (committer playBox)) e)
    (restart (fst <$> emitter playBox) (glue c <$|> speedSkipEffect ((\x -> (playFrame (snd x), playSpeed (snd x))) <$> emitter playBox) . pauser (playPause . snd <$> emitter playBox) =<< pipe))
  pure ()

-- | Serve an emitter controlled by a PlayConfig representation.
servePlayStream :: PlayConfig -> CodeBoxConfig -> CoEmitter IO (Gap, [Code]) -> IO ()
servePlayStream pcfg cbcfg s = servePlayStreamWithBox pcfg s <$|> codeBoxWith cbcfg

-- * low-level JS conversions

-- | {"event":{"element":"textid","value":"abcdees"}}
parserJ :: Parser e (ByteString, ByteString)
parserJ = do
  _ <- $(string "{\"event\":{\"element\":\"")
  e <- byteStringOf $ some (satisfy (/= '"'))
  _ <- $(string "\",\"value\":\"")
  v <- byteStringOf $ some (satisfy (/= '"'))
  _ <- $(string "\"}}")
  pure (e, v)

-- * code hooks

-- * code messaging

-- | A simple schema for code that communicates changes to a Html page via JS code.
data Code
  = Replace ByteString ByteString
  | Append ByteString ByteString
  | Console ByteString
  | Eval ByteString
  | Val ByteString
  deriving (Eq, Show, Generic, Read)

-- | Convert 'Code' to a 'ByteString'
code :: Code -> ByteString
code (Replace i t) = replace i t
code (Append i t) = append i t
code (Console t) = console t
code (Eval t) = t
code (Val t) = val t

-- | write to the console
console :: ByteString -> ByteString
console t = " console.log(" <> t <> ") "

-- | send arbitrary byestrings.
val :: ByteString -> ByteString
val t = " jsb.ws.send(" <> t <> ") "

-- | replace a container and run any embedded scripts
replace :: ByteString -> ByteString -> ByteString
replace d t =
  "\n     var $container = document.getElementById('" <> d <> "');\n     $container.innerHTML = '" <> clean t <> "';\n     runScripts($container);\n     refreshJsb();\n     "

-- | append to a container and run any embedded scripts
append :: ByteString -> ByteString -> ByteString
append d t =
  "\n     var $container = document.getElementById('" <> d <> "');\n     $container.innerHTML += '" <> clean t <> "';\n     runScripts($container);\n     refreshJsb();\n     "

-- | Double backslash newline and single quotes.
clean :: ByteString -> ByteString
clean =
  C.intercalate "\\'"
    . C.split '\''
    . C.intercalate "\\n"
    . C.lines

-- * initial javascript

-- | create a web socket for event data
webSocket :: Js
webSocket =
  Js
    "\nwindow.jsb = {ws: new WebSocket('ws://' + location.host + '/')};\njsb.event = function(ev) {\n    jsb.ws.send(JSON.stringify({event: ev}));\n};\njsb.ws.onmessage = function(evt){ \n    eval('(function(){' + evt.data + '})()');  \n};\n"

-- * scripts

-- | Event hooks that may need to be reattached given dynamic content creation.
refreshJsbJs :: Js
refreshJsbJs =
  Js
    "\nfunction refreshJsb () {\n  $('.jsbClassEventInput').off('input');\n  $('.jsbClassEventInput').on('input', (function(){\n    jsb.event({ 'element': this.id, 'value': this.value});\n  }));\n  $('.jsbClassEventChange').off('change');\n  $('.jsbClassEventChange').on('change', (function(){\n    jsb.event({ 'element': this.id, 'value': this.value});\n  }));\n  $('.jsbClassEventFocusout').off('focusout');\n  $('.jsbClassEventFocusout').on('focusout', (function(){\n    jsb.event({ 'element': this.id, 'value': this.value});\n  }));\n  $('.jsbClassEventButton').off('click');\n  $('.jsbClassEventButton').on('click', (function(){\n    jsb.event({ 'element': this.id, 'value': this.value});\n  }));\n  $('.jsbClassEventToggle').off('click');\n  $('.jsbClassEventToggle').on('click', (function(){\n    jsb.event({ 'element': this.id, 'value': ('true' !== this.getAttribute('aria-pressed')).toString()});\n  }));\n  $('.jsbClassEventCheckbox').off('click');\n  $('.jsbClassEventCheckbox').on('click', (function(){\n    jsb.event({ 'element': this.id, 'value': this.checked.toString()});\n  }));\n  $('.jsbClassEventChooseFile').off('input');\n  $('.jsbClassEventChooseFile').on('input', (function(){\n    jsb.event({ 'element': this.id, 'value': this.files[0].name});\n  }));\n  $('.jsbClassEventShowSum').off('change');\n  $('.jsbClassEventShowSum').on('change', (function(){\n    var v = this.value;\n    $(this).parent('.sumtype-group').siblings('.subtype').each(function(i) {\n      if (this.dataset.sumtype === v) {\n        this.style.display = 'block';\n        } else {\n        this.style.display = 'none';\n      }\n    })\n  }));\n  $('.jsbClassEventChangeMultiple').off('change');\n  $('.jsbClassEventChangeMultiple').on('change', (function(){\n    jsb.event({ 'element': this.id, 'value': [...this.options].filter(option => option.selected).map(option => option.value).join(',')});\n  }));\n};\n"

-- | prevent the Enter key from triggering an event
preventEnter :: Js
preventEnter =
  Js
    "\nwindow.addEventListener('keydown',function(e) {\n  if(e.keyIdentifier=='U+000A' || e.keyIdentifier=='Enter' || e.keyCode==13) {\n    if(e.target.nodeName=='INPUT' && e.target.type !== 'textarea') {\n      e.preventDefault();\n      return false;\n    }\n  }\n}, true);\n"

-- | script injection js.
--
-- See https://ghinda.net/article/script-tags/ for why this might be needed.
runScriptJs :: Js
runScriptJs =
  Js
    "\nfunction insertScript ($script) {\n  var s = document.createElement('script')\n  s.type = 'text/javascript'\n  if ($script.src) {\n    s.onload = callback\n    s.onerror = callback\n    s.src = $script.src\n  } else {\n    s.textContent = $script.innerText\n  }\n\n  // re-insert the script tag so it executes.\n  document.head.appendChild(s)\n\n  // clean-up\n  $script.parentNode.removeChild($script)\n}\n\nfunction runScripts ($container) {\n  // get scripts tags from a node\n  var $scripts = $container.querySelectorAll('script')\n  $scripts.forEach(function ($script) {\n    insertScript($script)\n  })\n}\n"

-- | Run a Parser, throwing away leftovers. Returns Left on 'Fail' or 'Err'.
runParserEither :: Parser ByteString a -> ByteString -> Either ByteString a
runParserEither p bs = case runParser p bs of
  Err e -> Left e
  OK a _ -> Right a
  Fail -> Left "uncaught parse error"
