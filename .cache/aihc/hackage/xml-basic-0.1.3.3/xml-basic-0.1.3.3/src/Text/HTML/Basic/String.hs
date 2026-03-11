module Text.HTML.Basic.String (Encoded, decode, ) where

import Text.XML.Basic.String (Encoded, )
import qualified Text.XML.Basic.String as XMLString
import qualified Text.HTML.Basic.Character as HTMLChar
import qualified Text.HTML.Basic.Entity as HTMLEnt

{- |
Invalid references are silently skipped.
-}
decode ::
   (Encoded -> String) ->
   [HTMLChar.T] ->
   String
decode =
   XMLString.decodeGen HTMLEnt.mapNameToChar
