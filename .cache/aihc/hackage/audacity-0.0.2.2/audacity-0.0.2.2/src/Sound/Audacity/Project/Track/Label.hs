module Sound.Audacity.Project.Track.Label (
   T (Cons, name_, height_, minimized_, track_),
   deflt,
   toXML,
   intervalToXML,
   tracksFromXML,
   parse,
   parseInterval,
   labelName,
   labeltrackName,
   ) where

import qualified Sound.Audacity.LabelTrack as LabelTrack
import qualified Sound.Audacity.XML.Attribute as Attr
import qualified Sound.Audacity.XML.Parser as Parser
import qualified Sound.Audacity.XML as XML

import qualified Text.HTML.Tagchup.Tag as Tag
import qualified Text.HTML.Tagchup.Tag.Match as TagMatch
import qualified Text.XML.Basic.Name.MixedCase as Name

import Text.Printf (printf)

import qualified Control.Monad.Trans.State as MS
import qualified Control.Monad.Trans.Maybe as MM
import qualified Control.Monad.Exception.Synchronous as ME
import Control.Applicative (many, (<*))

import qualified Data.NonEmpty.Mixed as NonEmptyM
import qualified Data.NonEmpty as NonEmpty
import Data.Maybe (mapMaybe)


data T =
   Cons {
      name_ :: String,
      height_ :: Int,
      minimized_ :: Bool,
      track_ :: LabelTrack.T Double String
   }
   deriving (Show)

deflt :: T
deflt =
   Cons {
      name_ = "",
      height_ = 100,
      minimized_ = False,
      track_ = LabelTrack.empty
   }


toXML :: T -> [[Tag.T Name.T String]]
toXML x =
   XML.tag "labeltrack" x
      (Attr.string "name" name_ :
       Attr.int "numlabels" (length . LabelTrack.decons . track_) :
       Attr.int "height" height_ :
       Attr.bool "minimized" minimized_ :
       [])
      $
      map intervalToXML (LabelTrack.decons $ track_ x)

{-
nanosecond precision as in ALSA
-}
intervalToXML :: LabelTrack.Interval Double String -> [Tag.T Name.T String]
intervalToXML ((from,to), title) =
   (Tag.open labelName $
      XML.attr "t"  (printf "%.9f" from) :
      XML.attr "t1" (printf "%.9f" to) :
      XML.attr "title" title :
      []) :
   Tag.close labelName :
   []


maybeExc ::
   MM.MaybeT (ME.Exceptional Parser.Message) a ->
   Maybe (ME.Exceptional Parser.Message a)
maybeExc (MM.MaybeT act) =
   case act of
      ME.Exception msg -> Just $ ME.Exception msg
      ME.Success ma -> fmap ME.Success ma

tracksFromXML :: [Tag.T Name.T String] -> ME.Exceptional Parser.Message [T]
tracksFromXML =
   sequence . mapMaybe (maybeExc . MS.evalStateT parse . NonEmpty.flatten) .
   snd . NonEmptyM.segmentBefore (TagMatch.open (labeltrackName==) (const True))

{- |
Currently we ignore the 'numlabels' attribute.
Alternatively we could check whether that value matches
the number of read intervals.
-}
parse :: Parser.T T
parse = do
   attrs <- Parser.tagOpen labeltrackName
   name <- Parser.lookupAttr "name" attrs
   height <- Parser.lookupAttrRead "height" attrs
   minimized <- Parser.lookupAttrBool "minimized" attrs
   Parser.skipSpaces
   intervals <- many (parseInterval <* Parser.skipSpaces)
   Parser.tagClose labeltrackName
   return $
      Cons {
         name_ = name,
         height_ = height,
         minimized_ = minimized,
         track_ = LabelTrack.Cons intervals
      }

parseInterval :: Parser.T (LabelTrack.Interval Double String)
parseInterval = do
   attrs <- Parser.tagOpen labelName
   from <- Parser.lookupAttrRead "t" attrs
   to <- Parser.lookupAttrRead "t1" attrs
   title <- Parser.lookupAttr "title" attrs
   Parser.tagClose labelName
   return ((from, to), title)


labelName :: Tag.Name Name.T
labelName = Tag.Name $ Name.Cons "label"

labeltrackName :: Tag.Name Name.T
labeltrackName = Tag.Name $ Name.Cons "labeltrack"
