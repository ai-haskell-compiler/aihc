module Sound.Audacity.Project where

import qualified Sound.Audacity.Project.Track.Label as LabelTrack
import qualified Sound.Audacity.Project.Track.Wave as WaveTrack
import qualified Sound.Audacity.XML.Attribute as Attr
import qualified Sound.Audacity.XML as XML

import qualified Text.HTML.Tagchup.Format as Format
import qualified Text.HTML.Tagchup.Tag as Tag
import qualified Text.XML.Basic.ProcessingInstruction as PI
import qualified Text.XML.Basic.Name.MixedCase as Name
import qualified Text.XML.Basic.Tag as XmlTag


data T =
   Cons {
      name_ :: String,
      selectionStart_, selectionEnd_ :: Double,
      vpos_ :: Int,
      h_ :: Double,
      zoom_ :: Double,
      rate_ :: Double,
      tracks_ :: [Track]
   }

deflt :: T
deflt =
   Cons {
      name_ = "",
      selectionStart_ = 0, selectionEnd_ = 0,
      vpos_ = 0,
      h_ = 0,
      zoom_ = 1,
      rate_ = 44100,
      tracks_ = []
   }


data Track =
     WaveTrack WaveTrack.T
   | LabelTrack LabelTrack.T


format :: T -> ShowS
format x =
   Format.xmlCondensed (XML.unlines $ toXML x)

toXML :: T -> [[Tag.T Name.T String]]
toXML x =
   [Tag.processing XmlTag.xmlName $ PI.Known $
      XML.attr "version" "1.0" :
      XML.attr "standalone" "no" :
      []] :
   [Tag.special XmlTag.doctypeName $
      "project PUBLIC \"-//audacityproject-1.3.0//DTD//EN\" " ++
      "\"http://audacity.sourceforge.net/xml/audacityproject-1.3.0.dtd\""] :
   XML.tag "project" x
     (Attr.string "xmlns" (const "http://audacity.sourceforge.net/xml/") :
      Attr.string "projname" name_ :
      Attr.string "version" (const "1.3.0") :
      Attr.string "audacityversion" (const "2.0.0") :
      Attr.double "sel0" selectionStart_ :
      Attr.double "sel1" selectionEnd_ :
      Attr.int "vpos" vpos_ :
      Attr.double "h" h_ :
      Attr.double "zoom" zoom_ :
      Attr.double "rate" rate_ :
      [])
     (concatMap trackToXML (tracks_ x))

trackToXML :: Track -> [[Tag.T Name.T String]]
trackToXML (LabelTrack x) = LabelTrack.toXML x
trackToXML (WaveTrack x) = WaveTrack.toXML x
