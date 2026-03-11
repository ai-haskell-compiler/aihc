module Text.XML.Basic.String where

import qualified Text.XML.Basic.Character as XMLChar
import qualified Text.XML.Basic.Entity as XMLEnt
import qualified Data.Map as Map
import qualified Data.Char as Char


-- | should be [Word8]
type Encoded  = String

{- |
Decode encoded characters and XML references.
Invalid references are silently skipped.
-}
decode ::
   (Encoded -> String) ->
   [XMLChar.T] ->
   String
decode =
   decodeGen XMLEnt.mapNameToChar

decodeGen ::
   Map.Map XMLEnt.Name Char ->
   (Encoded -> String) ->
   [XMLChar.T] ->
   String
decodeGen mapNameToChar decoder =
   foldr ($) [] .
   XMLChar.switchUnicodeRuns
      ((++) . decoder)
      (\n ->
         if XMLChar.validCharRef n
           then (Char.chr n :)
           else id)
      (\n ->
         maybe id (:) $
         Map.lookup n mapNameToChar)


{-
type Reference = Either Int String

data T = Cons String Reference R
data R = R Reference R | S T
-}
