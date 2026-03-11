{-# LANGUAGE QuasiQuotes #-}

-- |This modules provides support for using JMacro with Happstack.
--
-- It provides the instance,
--
-- > instance ToMessage JStat
--
-- Which will serve a 'JStat' value as @text/javascript; charset=UTF-8@.
-- The rendered JavaScript will be wrapped in an anonymous function that is
-- then called, so as to ensure the statements execute in a local scope.
-- An implication of this is that top-level unhygienic variables in JMacro
-- will /not/ be globally available; instead, you should set properties on
-- the global @window@ object.
module Happstack.Server.JMacro (jmResponse) where

import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as S

import Data.ByteString.Base64.URL   (encode)
import Data.Digest.Adler32          (adler32)
import Data.Serialize               (runPut, putWord32le)
import qualified Data.Text.Lazy.Encoding as T
import Happstack.Server             (ToMessage(..), ServerMonad, Request(Request, rqUri), Response, askRq)
import Language.Javascript.JMacro   (JStat(..), renderJs, renderPrefixJs, jmacro, jLam, toStat)
import Text.PrettyPrint.Leijen.Text (Doc, displayT, renderOneLine)
-- import Text.PrettyPrint           (Style(mode), Mode(OneLineMode), style, renderStyle)

mkId :: String -> String
mkId = S.unpack
     . S.map dollar
     . S.takeWhile (/= '=')
     . encode
     . B.dropWhile (== 0)
     . runPut
     . putWord32le
     . adler32
     . S.pack
  where
    dollar '-' = '$'
    dollar c   = c

data PrefixedJStat = PrefixedJStat String JStat

instance ToMessage JStat where
    toContentType _ = S.pack "text/javascript; charset=UTF-8"
    toMessage    js =
        T.encodeUtf8 . displayT . renderOneLine . renderJs $ scoped
      where
        scoped = [jmacro| (function { `(js)`; })(); |]

instance ToMessage PrefixedJStat where
    toContentType _ = S.pack "text/javascript; charset=UTF-8"
    toMessage (PrefixedJStat prefix js) =
        T.encodeUtf8 . displayT . renderOneLine . renderPrefixJs (mkId prefix) $ js

-- | Render a 'JStat' into a 'Response', saturating the variable names with
-- a hash computed from the 'rqUri'.  Unlike the 'ToMessage' instance for
-- @JStat@, this doesn't wrap the statements in a function and so the
-- workaround for global unhygienic names isn't necessary.  On the other
-- hand, generated variable names are a bit longer.
jmResponse :: ServerMonad m => JStat -> m Response
jmResponse jstat =
    do Request{rqUri = uri} <- askRq
       return . toResponse $ PrefixedJStat uri jstat
