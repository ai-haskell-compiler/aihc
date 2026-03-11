{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
  -- for Data.Semigroup

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Postscript
-- Copyright   :  (c) 2013 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Generic tools for generating Postscript files.  There is some
-- limited support for tracking the state of the renderer when
-- given a side-effecting (in the Postscript) command.  Only drawing
-- operations are supported, not general Postscript language generation.
--
-- In the future the tracking of rendering state could lead to optimizing
-- output, but for now little optimization is attempted.  Most systems are
-- equiped with tools to optimize Postscript such as 'eps2eps'.
--
-- For details on the PostScript language see the PostScript(R) Language
-- Reference: <http://www.adobe.com/products/postscript/pdfs/PLRM.pdf>
-----------------------------------------------------------------------------
module Graphics.Rendering.Postscript
  ( Render
  , RenderState, drawState
  , Surface
  , PSWriter(..)
  , renderWith
  , renderPagesWith
  , renderBuilder
  , renderPagesBuilder
  , withEPSSurface
  , newPath
  , moveTo
  , lineTo
  , curveTo
  , relLineTo
  , relCurveTo
  , arc
  , closePath
  , stroke
  , fill
  , fillPreserve
  , transform
  , save
  , restore
  , gsave
  , grestore
  , saveMatrix
  , restoreMatrix
  , translate
  , scale
  , rotate
  , strokeColor
  , strokeColorCMYK
  , fillColor
  , fillColorCMYK
  , lineWidth
  , lineCap
  , lineJoin
  , miterLimit
  , setDash
  , showText
  , showTextCentered
  , showTextAlign
  , showTextInBox
  , clip

  , FontSlant(..)
  , FontWeight(..)
  , face, slant, weight, size, isLocal

  , fillRule, font

  , CMYK(..), cyan, magenta, yellow, blacK
  ) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
import           Data.Monoid                (mconcat, mempty)
#endif
import           Control.Lens               (Lens', makeLenses, use, (%=), (.=))
import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.ByteString.Builder    as B
import           Data.Char                  (isPrint, ord)
import           Data.List                  (intersperse)
import           Data.Semigroup             (Semigroup (..))
import           Data.String                (fromString)
import           Diagrams.Attributes        (Color (..), LineCap (..),
                                             LineJoin (..), SomeColor (..),
                                             colorToSRGBA)
import           Diagrams.TwoD.Attributes   (Texture (..))
import           Diagrams.TwoD.Path         hiding (fillRule, stroke)
import           Numeric                    (showIntAtBase)
import           System.IO                  (IOMode (..), withFile)

data CMYK = CMYK
    { _cyan    :: Double
    , _magenta :: Double
    , _yellow  :: Double
    , _blacK   :: Double
    }
    deriving (Show, Eq)

makeLenses ''CMYK

data FontSlant = FontSlantNormal
               | FontSlantItalic
               | FontSlantOblique
               | FontSlant Double
            deriving (Show, Eq)

data FontWeight = FontWeightNormal
                | FontWeightBold
            deriving (Show, Eq)

data PostscriptFont = PostscriptFont
    { _face    :: String
    , _slant   :: FontSlant
    , _weight  :: FontWeight
    , _size    :: Double
    , _isLocal :: Bool
    } deriving (Eq, Show)

makeLenses '' PostscriptFont

defaultFont :: PostscriptFont
defaultFont = PostscriptFont "Helvetica" FontSlantNormal FontWeightNormal 1 True

-- Here we want to mirror the state of side-effecting calls
-- that we have emitted into the postscript file (at least
-- ones that we do not protect in other ways).
data DrawState = DS
                 { _fillRule :: FillRule
                 , _font     :: PostscriptFont
                 } deriving (Eq)

makeLenses ''DrawState

-- This reflects the defaults from the standard.
emptyDS :: DrawState
emptyDS = DS Winding defaultFont

data RenderState = RS
                   { _drawState :: !DrawState   -- The current state.
                   , _saved     :: ![DrawState] -- A stack of passed states pushed by save and poped with restore.
                   }

makeLenses ''RenderState

emptyRS :: RenderState
emptyRS = RS emptyDS []

--
-- | Type for a monad that writes Postscript using the commands we will define later.

-- | Type for a monad that writes Postscript using the commands we will define later.
newtype PSWriter m = PSWriter { runPSWriter :: State B.Builder m }
  deriving (Functor, Applicative, Monad, MonadState B.Builder)

tell' :: (MonadState s m, Semigroup s) => s -> m ()
#if MIN_VERSION_mtl(2,2,0)
tell' x = modify' (<> x)
#else
tell' x = do
  s' <- get
  put $! s' <> x
#endif

-- | Type of the monad that tracks the state from side-effecting commands.
newtype Render m = Render { runRender :: StateT RenderState PSWriter m }
  deriving (Functor, Applicative, Monad, MonadState RenderState)

-- | Abstraction of the drawing surface details.
data Surface = Surface { header :: Int -> B.Builder, footer :: Int -> B.Builder, _width :: Int, _height :: Int, fileName :: String }

doRender :: Render a -> PSWriter a
doRender r = evalStateT (runRender r) emptyRS

-- | Handles opening and closing the file associated with the
--   passed 'Surface' and renders the commands built up in the
--   'Render' argument.
renderWith :: MonadIO m => Surface -> Render a -> m a
renderWith s r = liftIO $ withFile (fileName s) WriteMode $ \h -> do
    B.hPutBuilder h b
    return v
  where
    (b, v) = renderBuilder s r

-- | Pure variant of 'renderWith'
renderBuilder :: Surface -> Render a -> (B.Builder, a)
renderBuilder s r =
    let (v, ss) = flip runState mempty . runPSWriter . doRender $ r
    in (header s 1 <> ss <> footer s 1, v)

-- | Renders multiple pages given as a list of 'Render' actions
--   to the file associated with the 'Surface' argument.
renderPagesWith :: MonadIO m => Surface -> [Render a] -> m [a]
renderPagesWith s rs = liftIO $ withFile (fileName s) WriteMode $ \h -> do
    B.hPutBuilder h b
    return v
  where
    (b, v) = renderPagesBuilder s rs

-- | Pure variant of 'renderPagesWith'
renderPagesBuilder :: Surface -> [Render a] -> (B.Builder, [a])
renderPagesBuilder s rs =
    let (vs, sss) = unzip . map (uncurry page) $ zip rs [1..]
    in (header s (length rs) <> mconcat sss, vs)
 where
   page r i =
      let (v, ss) = flip runState mempty . runPSWriter . doRender $  r
      in (v, ss <> footer s i)

-- | Builds a surface and performs an action on that surface.
withEPSSurface :: String -> Int -> Int -> (Surface -> r) -> r
withEPSSurface file w h f = f s
  where s = Surface (epsHeader w h) epsFooter w h file

renderPS :: B.Builder -> Render ()
renderPS b = Render . lift . tell' $ b <> "\n"

renderWordsPS :: (a -> B.Builder) -> [a] -> Render ()
renderWordsPS f = renderPS  . mconcat . intersperse " " . map f

-- | Clip with the current path.
clip :: Render ()
clip = renderPS "clip"

mkPSCall :: (a -> B.Builder) -> B.Builder -> [a] -> Render ()
mkPSCall f n vs = mkPSCall' n (map f vs)

mkPSCall' :: B.Builder -> [B.Builder] -> Render()
mkPSCall' n vs = renderPS $ mconcat (intersperse " " vs) <> " " <> n

-- | Start a new path.
newPath :: Render ()
newPath = renderPS "newpath"

-- | Close the current path.
closePath :: Render ()
closePath = renderPS "closepath"

-- | Draw an arc given a center, radius, start, and end angle.
arc :: Double -- ^ x-coordinate of center.
    -> Double -- ^ y-coordiante of center.
    -> Double -- ^ raidus.
    -> Double -- ^ start angle in radians.
    -> Double -- ^ end angle in radians.
    -> Render ()
arc a b c d e = mkPSCall B.doubleDec "arc" [a,b,c, d * 180 / pi, e* 180 / pi]

-- | Move the current point.
moveTo :: Double -> Double -> Render ()
moveTo x y = mkPSCall B.doubleDec "moveto" [x,y]

-- | Add a line to the current path from the current point to the given point.
--   The current point is also moved with this command.
lineTo :: Double -> Double -> Render ()
lineTo x y = mkPSCall B.doubleDec "lineto" [x,y]

-- | Add a cubic Bézier curve segment to the current path from the current point.
--   The current point is also moved with this command.
curveTo :: Double -> Double -> Double -> Double -> Double -> Double -> Render ()
curveTo ax ay bx by cx cy = mkPSCall B.doubleDec "curveto" [ax,ay,bx,by,cx,cy]

-- | Add a line segment to the current path using relative coordinates.
relLineTo :: Double -> Double -> Render ()
relLineTo x y = mkPSCall B.doubleDec "rlineto" [x,y]

-- | Add a cubic Bézier curve segment to the current path from the current point
--   using relative coordinates.
relCurveTo :: Double -> Double -> Double -> Double -> Double -> Double -> Render ()
relCurveTo ax ay bx by cx cy = mkPSCall B.doubleDec "rcurveto" [ax,ay,bx,by,cx,cy]

-- | Stroke the current path.
stroke :: Render ()
stroke = renderPS "s"

fill :: Render ()
fill = do
    rule <- use $ drawState . fillRule
    case rule of
      Winding -> renderPS "fill"
      EvenOdd -> renderPS "eofill"

-- | Fill the current path without affecting the graphics state.
fillPreserve :: Render ()
fillPreserve = do
        gsave
        fill
        grestore

-- | Draw a string at the current point.
showText :: String -> Render ()
showText s = do
    renderFont
    stringPS s
    renderPS " show"

-- | Draw a string by first measuring the width then offseting by half.
showTextCentered :: String -> Render ()
showTextCentered s = do
    renderFont
    stringPS s
    renderPS " showcentered"

-- | Draw a string uniformally scaling to fit within a bounding box.
showTextInBox :: (Double,Double) -> (Double,Double) -> String -> Render ()
showTextInBox (a,b) (c,d) s = do
    renderFont
    renderWordsPS B.doubleDec $ [a,b,c,d]
    stringPS s
    renderPS " showinbox"

-- | Draw a string with offset factors from center relative to the width and height.
showTextAlign :: Double -> Double -> String -> Render ()
showTextAlign xt yt s = do
    renderFont
    renderPS " "
    renderWordsPS B.doubleDec [xt, yt]
    stringPS s
    renderPS " showalign"

-- | Apply a transform matrix to the current transform.
transform :: Double -> Double -> Double -> Double -> Double -> Double -> Render ()
transform ax ay bx by tx ty = when (vs /= [1.0,0.0,0.0,1.0,0.0,0.0]) $
      renderPS (matrixPS vs <> " concat")
    where vs  = [ax,ay,bx,by,tx,ty]

matrixPS :: Show a => [a] -> B.Builder
matrixPS vs = fromString $ unwords ("[" : map show vs ++ ["]"])

-- | Push the current state of the renderer onto the state stack.
save :: Render ()
save = do
    renderPS "save"
    d <- use drawState
    saved %= (d:)

-- | Replace the current state by popping the state stack.
restore :: Render ()
restore = do
    renderPS "restore"
    s <- use saved
    case s of
      []     -> do saved .= []
      (x:xs) -> do
        drawState .= x
        saved     .= xs


-- | Push the current graphics state.
gsave :: Render ()
gsave = do
    renderPS "gsave"
    d <- use drawState
    saved %= (d:)

-- | Pop the current graphics state.
grestore :: Render ()
grestore = do
    renderPS "grestore"
    s <- use saved
    case s of
      []     -> do saved .= []
      (x:xs) -> do
          drawState .= x
          saved     .= xs

-- | Push the current transform matrix onto the execution stack.
saveMatrix :: Render ()
saveMatrix = renderPS "matrix currentmatrix"

-- | Set the current transform matrix to be the matrix found by popping
--   the execution stack.
restoreMatrix :: Render ()
restoreMatrix = renderPS "setmatrix"

-- RGB colors
colorPS :: Color c => c -> [Double]
colorPS c = [ r, g, b ]
  where (r,g,b,_) = colorToSRGBA c

-- | Set the color of the stroke.  Ignore gradients.
strokeColor :: Texture n -> Render ()
strokeColor (SC (SomeColor c)) = mkPSCall B.doubleDec "setrgbcolor" (colorPS c)
strokeColor _ = return ()

-- | Set the color of the fill.  Ignore gradients.
fillColor :: Texture n -> Render ()
fillColor (SC (SomeColor c)) = mkPSCall B.doubleDec "setrgbcolor" (colorPS c)
fillColor _                  = return ()

-- CMYK colors
colorCMYK :: CMYK -> [Double]
colorCMYK (CMYK c m y k) = [c,m,y,k]

-- | Set the color of the stroke.
strokeColorCMYK :: CMYK -> Render ()
strokeColorCMYK c = mkPSCall B.doubleDec "setcmykcolor" (colorCMYK c)

-- | Set the color of the fill.
fillColorCMYK :: CMYK -> Render ()
fillColorCMYK c = mkPSCall B.doubleDec "setcmykcolor" (colorCMYK c)

-- | Set the line width.
lineWidth :: Double -> Render ()
lineWidth w = mkPSCall B.doubleDec "setlinewidth" [w]

-- | Set the line cap style.
lineCap :: LineCap -> Render ()
lineCap lc = mkPSCall B.intDec "setlinecap" [fromLineCap lc]

-- | Set the line join method.
lineJoin :: LineJoin -> Render ()
lineJoin lj = mkPSCall B.intDec "setlinejoin" [fromLineJoin lj]

-- | Set the miter limit.
miterLimit :: Double -> Render ()
miterLimit ml = mkPSCall B.doubleDec "setmiterlimit" [ml]

-- | Set the dash style.
setDash :: [Double] -- ^ Dash pattern (even indices are "on").
        -> Double   -- ^ Offset.
        -> Render ()
setDash as offset = mkPSCall' "setdash" [showArray B.doubleDec as, B.doubleDec offset]

showArray :: (a -> B.Builder) -> [a] -> B.Builder
showArray f as = "[" <> mconcat (intersperse " " (map f as)) <> "]"

fromLineCap :: LineCap -> Int
fromLineCap LineCapRound  = 1
fromLineCap LineCapSquare = 2
fromLineCap _             = 0

fromLineJoin :: LineJoin -> Int
fromLineJoin LineJoinRound = 1
fromLineJoin LineJoinBevel = 2
fromLineJoin _             = 0

-- | Translate the current transform matrix.
translate :: Double -> Double -> Render ()
translate x y = mkPSCall B.doubleDec "translate" [x,y]

-- | Scale the current transform matrix.
scale :: Double -> Double -> Render ()
scale x y = mkPSCall B.doubleDec "scale" [x,y]

-- | Rotate the current transform matrix.
rotate :: Double -> Render ()
rotate t = mkPSCall B.doubleDec "rotate" [t]

stringPS :: String -> Render ()
stringPS ss = Render $ lift $ do
    tell' "("
    tell' (fromString $ concatMap escape ss)
    tell' ")"
  where escape '\n' = "\\n"
        escape '\r' = "\\r"
        escape '\t' = "\\t"
        escape '\b' = "\\b"
        escape '\f' = "\\f"
        escape '\\' = "\\"
        escape '('  = "\\("
        escape ')'  = "\\)"
        escape c | isPrint c = [c]
                 | otherwise = '\\' : showIntAtBase 7 ("01234567"!!) (ord c) ""

epsHeader :: Int -> Int -> Int -> B.Builder
epsHeader w h pages = mconcat
          [ "%!PS-Adobe-3.0", if pages == 1 then " EPSF-3.0\n" else "\n"
          , "%%Creator: diagrams-postscript 0.1\n"
          , "%%BoundingBox: 0 0 ", B.intDec w, " ", B.intDec h, "\n"
          , "%%Pages: ", B.intDec pages, "\n"
          , "%%EndComments\n\n"
          , "%%BeginProlog\n"
          , "%%BeginResource: procset diagrams-postscript 0 0\n"
          , "/s { 0.0 currentlinewidth ne { stroke } if } bind def\n"
          , "/nvhalf { 2 div neg exch 2 div neg exch } bind def\n"
          , "/showcentered { dup stringwidth nvhalf moveto show } bind def\n"
          , "/stringbbox { 0 0 moveto true charpath flattenpath pathbbox } bind def\n"
          , "/wh { 1 index 4 index sub 1 index 4 index sub } bind def\n"
          , "/showinbox { gsave dup stringbbox wh 11 7 roll mark 11 1 roll "
          , "wh dup 7 index div 2 index 9 index div 1 index 1 index lt "
          , "{ pop dup 9 index mul neg 3 index add 2 div 7 index add "
          , " 6 index 13 index abs add } "
          , "{ exch pop 6 index 12 index abs 2 index mul 7 index add } "
          , "ifelse 17 3 roll cleartomark 4 1 roll translate dup scale "
          , "0 0 moveto show grestore } bind def\n"
          , "/showalign { dup mark exch stringbbox wh 10 -1 roll exch 10 1 roll mul "
          , "neg 9 -2 roll mul 4 index add neg 8 2 roll cleartomark 3 1 roll moveto "
          , "show } bind def\n"
          , "%%EndResource\n"
          , "%%EndProlog\n"
          , "%%BeginSetup\n"
          , "%%EndSetup\n"
          , "%%Page: 1 1\n"
          ]

epsFooter :: Int -> B.Builder
epsFooter page = mconcat
          [ "showpage\n"
          , "%%PageTrailer\n"
          , "%%EndPage: ", B.intDec page, "\n"
          ]

---------------------------
-- Font

renderFont :: Render ()
renderFont = do
    n <- fontFromName <$> f face <*> f slant <*> f weight
    s <- f size
    renderPS $ mconcat ["/", fromString n, " ", B.doubleDec s, " selectfont"]
  where
    f :: Lens' PostscriptFont a -> Render a
    f x = use $ drawState . font . x

-- This is a little hacky.  I'm not sure there are good options.
fontFromName :: String -> FontSlant -> FontWeight -> String
fontFromName n s w = fontName ++ bold w ++ italic s
  where
    fontName = map f n
    f ' ' = '-'
    f c   = c

    bold FontWeightNormal = ""
    bold FontWeightBold   = "Bold"

    italic FontSlantNormal  = ""
    italic FontSlantItalic  = "Italic"
    italic FontSlantOblique = "Oblique"
    italic _                = ""
