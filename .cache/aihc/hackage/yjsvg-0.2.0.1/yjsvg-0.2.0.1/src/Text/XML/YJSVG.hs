module Text.XML.YJSVG (
	SVG(..), Position(..), Color(..), Transform(..),
	Font(..), FontWeight(..),
	showSVG, topleft, center,
) where

import Text.XML.HaXml(
	AttValue(..), QName(..), Prolog(..),
	EncodingDecl(..), XMLDecl(..), SystemLiteral(..), PubidLiteral(..),
	ExternalID(..), DocTypeDecl(..), Misc(..), Element(..), Content(..),
	Document(..) )
import Text.XML.HaXml.Pretty (document)
import Data.Word(Word8)

data FontWeight = Normal | Bold deriving (Show, Read)

weightValue :: FontWeight -> String
weightValue Normal = "normal"
weightValue Bold = "bold"

data Font = Font{ fontName :: String, fontWeight :: FontWeight }
	deriving (Show, Read)

data Position
	= TopLeft { posX :: Double, posY :: Double }
	| Center { posX :: Double, posY :: Double }
	deriving (Show, Read)

topleft, center :: Double -> Double -> Position -> Position
topleft w h Center { posX = x, posY = y } =
	TopLeft { posX = x + w / 2, posY = - y + h / 2 }
topleft _ _ pos = pos
center w h TopLeft { posX = x, posY = y } =
	Center { posX = x - w / 2, posY = - y + h / 2 }
center _ _ pos = pos

getPos :: Double -> Double -> Position -> (Double, Double)
getPos _ _ TopLeft { posX = x, posY = y } = (x, y)
getPos w h Center { posX = x, posY = y } = (x + w / 2, - y + h / 2)

type Id = String

data SVG   = Line Position Position Color Double |
             Polyline [Position] Color Color Double |
             Rect Position Double Double Double Color Color |
             Fill Color |
	     Circle Position Double Color |
             Text Position Double Color Font String |
	     Image Position Double Double FilePath |
	     Use Position Double Double String |
	     Group [ Transform ] [ (Id,SVG) ] | 
	     Defs [ (Id,SVG) ]
	     --Symbol [ (Id,SVG) ]
	deriving (Show, Read)

data Color
	= ColorName{ colorName :: String }
	| RGB {	colorRed :: Word8,
		colorGreen ::  Word8,
		colorBlue ::  Word8 }
	deriving (Eq, Show, Read)

mkColorStr :: Color -> String
mkColorStr ColorName { colorName = n } = n
mkColorStr RGB { colorRed = r, colorGreen = g, colorBlue = b } =
	"rgb(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"

data Transform
	= Matrix Double Double Double Double Double Double
	| Translate Double Double
	| Scale Double Double
	| Rotate Double (Maybe (Double, Double))
	| SkewX Double
	| SkewY Double
	deriving (Show, Read)

showTrans :: Transform -> String
showTrans (Translate tx ty) = "translate(" ++ show tx ++ "," ++ show ty ++ ") "
showTrans (Scale sx sy) = "scale(" ++ show sx ++ "," ++ show sy ++ ") "
showTrans (SkewX s) = "skewX(" ++ show s ++ ") "
showTrans (SkewY s) = "skewY(" ++ show s ++ ") "
showTrans (Rotate s _) = "rotate(" ++ show s ++ ") "
showTrans (Matrix a b c d e f) = "matrix(" ++
	show a ++ "," ++ show b ++ "," ++ show c ++ "," ++
	show d ++ "," ++ show e ++ "," ++ show f ++ ") "
-- showTrans _ = error "not implemented yet"

showSVG :: Double -> Double -> [ (Id, SVG) ] -> String
showSVG w h = show . document . svgToXml w h

svgToElem :: Double -> Double -> (Id, SVG) -> Element ()
svgToElem pw ph (idn, (Line p1 p2 color lineWidth)) = Elem (N "line") [
	(N "id", AttValue [Left idn]),
	(N "x1", AttValue [Left $ show x1]),
	(N "y1", AttValue [Left $ show y1]),
	(N "x2", AttValue [Left $ show x2]),
	(N "y2", AttValue [Left $ show y2]),
	(N "stroke", AttValue [Left $ mkColorStr color]),
	(N "stroke-width", AttValue [Left $ show lineWidth]),
	(N "stroke-linecap", AttValue [Left "round"]) ] []
	where
	(x1, y1) = getPos pw ph p1
	(x2, y2) = getPos pw ph p2
svgToElem pw ph (idn, (Polyline points fillColor lineColor lineWidth)) =
	Elem (N "polyline") [
		(N "id", AttValue [ Left $ idn ]),
		(N "points", AttValue [ Left $ pointsToAttVal points ]),
		(N "fill"  , AttValue [ Left $ mkColorStr fillColor ]),
		(N "stroke", AttValue [ Left $ mkColorStr lineColor ]),
		(N "stroke-width", AttValue [ Left $ show lineWidth ]) ] []
	where
	pointsToAttVal :: [Position] -> String
	pointsToAttVal [] = ""
	pointsToAttVal (p : ps) = let (x, y) = getPos pw ph p in
		show x ++ "," ++ show y ++ " " ++ pointsToAttVal ps
svgToElem pw ph (idn, (Rect p w h sw cf cs)) = Elem (N "rect") [
	(N "id", AttValue [Left $ idn]),
	(N "x", AttValue [Left $ show x]),
	(N "y", AttValue [Left $ show y]),
	(N "width", AttValue [Left $ show w]),
	(N "height", AttValue [Left $ show h]),
	(N "stroke-width", AttValue [Left $ show sw]),
	(N "fill", AttValue [Left $ mkColorStr cf]),
	(N "stroke", AttValue [Left $ mkColorStr cs]) ] []
	where (x, y) = getPos pw ph p
svgToElem pw ph (idn, (Fill c)) =
	svgToElem pw ph $ (idn, Rect (TopLeft 0 0) pw ph 0 c c)
svgToElem pw ph (idn, (Text p s c f t)) = Elem (N "text") [
		(N "id", AttValue [Left $ idn]),
		(N "x", AttValue [Left $ show x]),
		(N "y", AttValue [Left $ show y]),
		(N "font-family", AttValue [Left $ fontName f]),
		(N "font-weight", AttValue [Left $ weightValue $ fontWeight f]),
		(N "font-size", AttValue [Left $ show s]),
		(N "fill", AttValue [Left $ mkColorStr c]) ]
	[CString False (escape t) ()]
	where (x, y) = getPos pw ph p
svgToElem pw ph (idn, (Circle p r c)) = Elem (N "circle") [
	(N "id", AttValue [Left $ idn]),
	(N "cx", AttValue [Left $ show x]),
	(N "cy", AttValue [Left $ show y]),
	(N "r", AttValue [Left $ show r]),
	(N "fill", AttValue [Left $ mkColorStr c]) ] []
	where (x, y) = getPos pw ph p
svgToElem pw ph (idn, (Image p w h path)) = Elem (N "image") [
	(N "id", AttValue [Left $ idn]),
	(N "x", AttValue [Left $ show x]),
	(N "y", AttValue [Left $ show y]),
	(N "width", AttValue [Left $ show w]),
	(N "height", AttValue [Left $ show h]),
	(N "xlink:href", AttValue [Left path]) ] []
	where (x, y) = getPos pw ph p
svgToElem pw ph (_idn, (Use p w h path)) = Elem (N "use") [
	(N "x", AttValue [Left $ show x]),
	(N "y", AttValue [Left $ show y]),
	(N "width", AttValue [Left $ show w]),
	(N "height", AttValue [Left $ show h]),
	(N "xlink:href", AttValue [Left ("url(#"++path++")")]) ] []
	where (x, y) = getPos pw ph p
svgToElem pw ph (_idn, (Group trs svgs)) = Elem (N "g")  
	[(N "transform", AttValue (map Left (map showTrans trs)))]
	$ map (flip CElem () . svgToElem pw ph) svgs
svgToElem pw ph (_idn, (Defs svgs)) = Elem (N "defs") []
	$ map (flip CElem () . svgToElem pw ph) svgs
-- svgToElem pw ph (id, (Symbol svgs)) = Elem (N "symbol") []
-- 	$ map (flip CElem () . svgToElem pw ph) svgs

svgToXml :: Double -> Double -> [ (Id,SVG) ] -> Document ()
svgToXml w h svgs = Document prlg [] (Elem (N "svg") (emAtt w h)
	$ map (flip CElem () . svgToElem w h) svgs) []

pmsc :: [Misc]
pmsc = [ Comment " MADE BY SVG.HS " ]

pblit, sslit :: String
pblit = "-//W3C//DTD SVG 1.1//EN"
sslit = "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd"

doctype :: DocTypeDecl
doctype = DTD (N "svg")
	(Just (PUBLIC (PubidLiteral pblit) (SystemLiteral sslit))) []

xmldcl :: XMLDecl
xmldcl = XMLDecl "1.0" (Just (EncodingDecl "UTF-8")) Nothing

prlg :: Prolog
prlg = Prolog (Just xmldcl) pmsc (Just doctype) []

xmlns, ver, xlink :: String
xmlns = "http://www.w3.org/2000/svg"
ver = "1.1"
xlink = "http://www.w3.org/1999/xlink"

emAtt :: (Show a, Show b) => a -> b -> [(QName, AttValue)]
emAtt w h = [
	(N "xmlns", AttValue [Left xmlns]),
	(N "version", AttValue [Left ver]),
	(N "xmlns:xlink", AttValue [Left xlink]),
	(N "width", AttValue [Left $ show w]),
	(N "height", AttValue [Left $ show h]) ]

escape :: String -> String
escape "" = ""
escape ('&' : cs) = "&amp;" ++ escape cs
escape ('<' : cs) = "&lt;" ++ escape cs
escape ('>' : cs) = "&gt;" ++ escape cs
escape (c : cs) = c : escape cs
