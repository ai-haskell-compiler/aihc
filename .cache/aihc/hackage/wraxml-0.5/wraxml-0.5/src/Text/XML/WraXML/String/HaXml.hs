module Text.XML.WraXML.String.HaXml where

import Text.XML.HaXml(AttValue(AttValue), Reference, )
import qualified Text.XML.HaXml as HaXml

import qualified Text.XML.WraXML.String  as XmlString
import qualified Text.XML.Basic.Character as XmlChar

import Data.Maybe.HT (alternatives, (?->), )


toXmlString :: AttValue -> XmlString.T
toXmlString (AttValue s) =
   concatMap (either XmlString.fromString ((:[]) . refToXmlAtom)) s

fromXmlString :: XmlString.T -> AttValue
fromXmlString =
   AttValue .
   XmlChar.switchUnicodeRuns
      Left
      (Right . HaXml.RefChar)
      (Right . HaXml.RefEntity)

lift :: (XmlString.T -> XmlString.T) -> (AttValue -> AttValue)
lift f = fromXmlString . f . toXmlString

refToXmlAtom :: HaXml.Reference -> XmlString.Atom
refToXmlAtom ref =
   case ref of
      HaXml.RefChar   num  -> XmlChar.fromCharRef   num
      HaXml.RefEntity name -> XmlChar.fromEntityRef name

charFromXmlAtom' :: XmlChar.T -> Either Char Reference
charFromXmlAtom' c =
   case c of
      XmlChar.Unicode   char -> Left char
      XmlChar.EntityRef name -> Right (HaXml.RefEntity name)
      XmlChar.CharRef   num  -> Right (HaXml.RefChar   num)

charFromXmlAtom :: XmlChar.T -> Either Char Reference
charFromXmlAtom c =
   alternatives
      (error "HaXml.charFromXmlAtom: unsupported constructor in XmlChar.T") $
      XmlChar.maybeUnicode   c ?-> Left :
      XmlChar.maybeEntityRef c ?-> Right . HaXml.RefEntity :
      XmlChar.maybeCharRef   c ?-> Right . HaXml.RefChar :
      []
