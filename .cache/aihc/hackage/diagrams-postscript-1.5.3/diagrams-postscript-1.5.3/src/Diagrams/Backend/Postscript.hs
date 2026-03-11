{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE ViewPatterns              #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Postscript
-- Copyright   :  (c) 2013 Diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A Postscript rendering backend for diagrams.
--
-- To build diagrams for Postscript rendering use the @Postscript@
-- type in the diagram type construction
--
-- > d :: Diagram Postscript
-- > d = square 10
--
-- and render giving the @Postscript@ token
--
-- > renderDia' d (PostscriptOptions "file.eps" (mkWidth 400) EPS)
--
-- This IO action will write the specified file. If you prefer to generate
-- an in-memory 'ByteString', use 'renderDia'.
--
-- Check "Diagrams.Backend.Postscript.CmdLine" for the command-line
-- interface documentation (a great way to start using this package).
-----------------------------------------------------------------------------
module Diagrams.Backend.Postscript

  ( -- * Backend token
    Postscript(..)
  , B

    -- * Postscript-specific options
    -- | #postscript-options#
    -- $PostscriptOptions

  , Options(..), psfileName, psSizeSpec, psOutputFormat

    -- * Postscript-supported output formats

  , OutputFormat(..)

  , renderDias
  , renderDia'
  , renderDias'
  ) where

import           Diagrams.Backend.Postscript.CMYK
import           Diagrams.Core.Compile
import qualified Graphics.Rendering.Postscript    as C

import           Diagrams.Prelude                 hiding (fillColor, view)

import           Diagrams.TwoD.Adjust             (adjustDia2D)
import           Diagrams.TwoD.Path               (Clip (Clip), getFillRule)
import           Diagrams.TwoD.Text

import           Control.Lens                     hiding (transform)
import           Control.Monad                    (when)
import qualified Control.Monad.StateStack         as SS
import           Control.Monad.Trans              (lift)
import           Data.Maybe                       (catMaybes, isJust)

import qualified Data.ByteString.Builder          as B
import           System.IO                        (IOMode (..), withFile)

import qualified Data.Foldable                    as F
import           Data.Hashable                    (Hashable (..))
import           Data.Tree
import           Data.Typeable
import           GHC.Generics                     (Generic)

-- | This data declaration is simply used as a token to distinguish this rendering engine.
data Postscript = Postscript
    deriving (Eq,Ord,Read,Show,Typeable)

type B = Postscript

type instance V Postscript = V2
type instance N Postscript = Double

-- | Postscript only supports EPS style output at the moment.  Future formats would each
--   have their own associated properties that affect the output.
data OutputFormat = EPS -- ^ Encapsulated Postscript output.
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Typeable, Generic)

instance Hashable OutputFormat

data PostscriptState
  = PostscriptState { _accumStyle :: Style V2 Double
                      -- ^ The current accumulated style.
                    , _ignoreFill :: Bool
                      -- ^ Whether or not we saw any lines in the most
                      --   recent path (as opposed to loops).  If we did,
                      --   we should ignore any fill attribute.
                      --   diagrams-lib separates lines and loops into
                      --   separate path primitives so we don't have to
                      --   worry about seeing them together in the same
                      --   path.
                    }

$(makeLenses ''PostscriptState)

instance Default PostscriptState where
   def = PostscriptState
         { _accumStyle = mempty
         , _ignoreFill = False
         }

type RenderM a = SS.StateStackT PostscriptState C.Render a

liftC :: C.Render a -> RenderM a
liftC = lift

runRenderM :: RenderM a -> C.Render a
runRenderM = flip SS.evalStateStackT def

save :: RenderM ()
save = SS.save >> liftC C.save

restore :: RenderM ()
restore = liftC C.restore >> SS.restore

instance Semigroup (Render Postscript V2 Double) where
  C x <> C y = C (x >> y)

instance Monoid (Render Postscript V2 Double) where
  mempty  = C $ return ()
#if !MIN_VERSION_base(4,11,0)
  mappend = (<>)
#endif

instance Backend Postscript V2 Double where
  data Render  Postscript V2 Double = C (RenderM ())
  type Result  Postscript V2 Double = B.Builder
  data Options Postscript V2 Double = PostscriptOptions
          { _psfileName     :: String       -- ^ the name of the file you want generated
          , _psSizeSpec     :: SizeSpec V2 Double   -- ^ the requested size of the output
          , _psOutputFormat :: OutputFormat -- ^ the output format and associated options
          }
    deriving (Show, Eq)

  renderRTree _ opts t =
    let surfaceF surface = fst (C.renderBuilder surface r)
        V2 w h = specToSize 100 (opts^.psSizeSpec)
        r = runRenderM . runC . toRender $ t
    in case opts^.psOutputFormat of
         EPS -> C.withEPSSurface (opts^.psfileName) (round w) (round h) surfaceF

  adjustDia = adjustDia2D psSizeSpec

runC :: Render Postscript V2 Double -> RenderM ()
runC (C r) = r

-- | Get an accumulated style attribute from the render monad state.
getStyleAttrib :: AttributeClass a => (a -> b) -> RenderM (Maybe b)
getStyleAttrib f = (fmap f . getAttr) <$> use accumStyle

toRender :: RTree Postscript V2 Double a -> Render Postscript V2 Double
toRender (Node (RPrim p) _) = render Postscript p
toRender (Node (RStyle sty) rs) = C $ do
  save
  postscriptStyle sty
  accumStyle %= (<> sty)
  runC $ F.foldMap toRender rs
  restore
toRender (Node _ rs) = F.foldMap toRender rs

instance Hashable (Options Postscript V2 Double) where
  hashWithSalt s (PostscriptOptions fn sz out) =
    s `hashWithSalt` fn
      `hashWithSalt` sz
      `hashWithSalt` out

psfileName :: Lens' (Options Postscript V2 Double) String
psfileName = lens (\(PostscriptOptions {_psfileName = f}) -> f)
                     (\o f -> o {_psfileName = f})

psSizeSpec :: Lens' (Options Postscript V2 Double) (SizeSpec V2 Double)
psSizeSpec = lens (\(PostscriptOptions {_psSizeSpec = s}) -> s)
                     (\o s -> o {_psSizeSpec = s})

psOutputFormat :: Lens' (Options Postscript V2 Double) OutputFormat
psOutputFormat = lens (\(PostscriptOptions {_psOutputFormat = t}) -> t)
                     (\o t -> o {_psOutputFormat = t})

renderDias :: (Semigroup m, Monoid m) =>
               Options Postscript V2 Double -> [QDiagram Postscript V2 Double m] -> IO [()]
renderDias opts ds = case opts^.psOutputFormat of
  EPS -> C.withEPSSurface (opts^.psfileName) (round w) (round h) surfaceF
    where
      surfaceF surface  = C.renderPagesWith surface rs
      dropMid (x, _, z) = (x,z)
      optsdss = map (dropMid . adjustDia Postscript opts) ds
      g2o     = scaling (sqrt (w * h))
      rs      = map (runRenderM . runC . toRender . toRTree g2o . snd) optsdss
      sizes   = map (specToSize 1 . view psSizeSpec . fst) optsdss
      V2 w h  = foldBy (liftA2 max) zero sizes

-- | Check [Postscript-specific options](#postscript-options) for a
-- description of the available options.
renderDia' :: QDiagram Postscript V2 Double Any -> Options Postscript V2 Double -> IO ()
renderDia' d o = do
  let b = renderDia Postscript o d
  withFile (o ^. psfileName) WriteMode $ \h -> B.hPutBuilder h b

-- | Check [Postscript-specific options](#postscript-options) for a
-- description of the available options.
renderDias' :: [QDiagram Postscript V2 Double Any] -> Options Postscript V2 Double -> IO ()
renderDias' ds o = renderDias o ds >> return ()

renderC :: (Renderable a Postscript, V a ~ V2, N a ~ Double) => a -> RenderM ()
renderC = runC . render Postscript

-- | Handle those style attributes for which we can immediately emit
--   postscript instructions as we encounter them in the tree (clip, font
--   size, fill rule, line width, cap, join, and dashing).  Other
--   attributes (font face, slant, weight; fill color, stroke color,
--   opacity) must be accumulated.
postscriptStyle :: Style v Double -> RenderM ()
postscriptStyle s =
  sequence_
  . catMaybes $ [ handle clip
                , handle lFillRule
                , handle lWidth
                , handle lJoin
                , handle lMiter
                , handle lCap
                , handle lDashing
                ]
  where
    handle :: AttributeClass a => (a -> RenderM ()) -> Maybe (RenderM ())
    handle f = f `fmap` getAttr s
    clip     = mapM_ (\p -> postscriptPath p >> liftC C.clip) . op Clip
    lFillRule = liftC . assign (C.drawState . C.fillRule) . getFillRule
    lWidth = liftC . C.lineWidth . getLineWidth
    lCap = liftC . C.lineCap . getLineCap
    lJoin = liftC . C.lineJoin . getLineJoin
    lMiter = liftC . C.miterLimit . getLineMiterLimit
    lDashing (getDashing -> Dashing ds offs) = liftC $ C.setDash ds offs

fromFontSlant :: FontSlant -> C.FontSlant
fromFontSlant FontSlantNormal   = C.FontSlantNormal
fromFontSlant FontSlantItalic   = C.FontSlantItalic
fromFontSlant FontSlantOblique  = C.FontSlantOblique

fromFontWeight :: FontWeight -> C.FontWeight
fromFontWeight FontWeightNormal = C.FontWeightNormal
fromFontWeight FontWeightBold   = C.FontWeightBold
fromFontWeight _                = C.FontWeightNormal

postscriptTransf :: Transformation V2 Double -> C.Render ()
postscriptTransf t = C.transform a1 a2 b1 b2 c1 c2
  where (V2 a1 a2) = apply t unitX
        (V2 b1 b2) = apply t unitY
        (V2 c1 c2) = transl t

instance Renderable (Segment Closed V2 Double) Postscript where
  render _ (Linear (OffsetClosed v)) = C . liftC $ uncurry C.relLineTo (unr2 v)
  render _ (Cubic (unr2 -> (x1, y1))
                  (unr2 -> (x2, y2))
                  (OffsetClosed (unr2 -> (x3, y3))))
    = C . liftC $ C.relCurveTo x1 y1 x2 y2 x3 y3

instance Renderable (Trail V2 Double) Postscript where
  render _ = withTrail renderLine renderLoop
    where
      renderLine ln = C $ do
        mapM_ renderC (lineSegments ln)

        -- remember that we saw a Line, so we will ignore fill attribute
        ignoreFill .= True

      renderLoop lp = C $ do
        case loopSegments lp of
          -- let closePath handle the last segment if it is linear
          (segs, Linear _) -> mapM_ renderC segs

          -- otherwise we have to draw it explicitly
          _ -> mapM_ renderC (lineSegments . cutLoop $ lp)

        liftC C.closePath

instance Renderable (Path V2 Double) Postscript where
  render _ p = C $ do
      postscriptPath p
      f <- getStyleAttrib getFillTexture
      s <- getStyleAttrib getLineTexture
      fk <- getStyleAttrib getFillColorCMYK
      sk <- getStyleAttrib getLineColorCMYK
      ign <- use ignoreFill
      setFillColor f fk
      when ((isJust f || isJust fk) && not ign) $ liftC C.fillPreserve
      setStrokeColor s sk
      liftC C.stroke

setFillColor :: Maybe (Texture Double) -> Maybe (CMYK) -> RenderM ()
setFillColor c cmyk = do
    liftC $ maybe (return ()) C.fillColor c
    liftC $ maybe (return ()) C.fillColorCMYK cmyk

setStrokeColor :: Maybe (Texture Double) -> Maybe (CMYK) -> RenderM ()
setStrokeColor c cmyk = do
    liftC $ maybe (return ()) C.strokeColor c
    liftC $ maybe (return ()) C.strokeColorCMYK cmyk

postscriptPath :: Path V2 Double -> RenderM ()
postscriptPath (Path trs) = do
      liftC C.newPath
      ignoreFill .= False
      F.mapM_ renderTrail trs
    where renderTrail (viewLoc -> (unp2 -> pt, tr)) = do
            liftC $ uncurry C.moveTo pt
            renderC tr

if' :: Monad m => (a -> m ()) -> Maybe a -> m ()
if' = maybe (return ())

instance Renderable (Text Double) Postscript where
  render _ (Text tr al str) = C $ do
      ff <- getStyleAttrib getFont
      fs <- getStyleAttrib (fromFontSlant . getFontSlant)
      fw <- getStyleAttrib (fromFontWeight . getFontWeight)
      size' <- getStyleAttrib getFontSize
      f <- getStyleAttrib getFillTexture
      fk <- getStyleAttrib getFillColorCMYK
      save
      setFillColor f fk
      liftC $ do
        if' (assign (C.drawState . C.font . C.size))   size'
        if' (assign (C.drawState . C.font . C.face))   ff
        if' (assign (C.drawState . C.font . C.slant))  fs
        if' (assign (C.drawState . C.font . C.weight)) fw
      when (isJust f || isJust fk) $ liftC C.fillPreserve
      liftC $ postscriptTransf tr
      case al of
        BoxAlignedText xt yt -> liftC $ C.showTextAlign xt yt str
        BaselineText         -> liftC $ C.moveTo 0 0 >> C.showText str
      restore

-- $PostscriptOptions
--
-- Unfortunately, Haddock does not yet support documentation for
-- associated data families, so we must just provide it manually.
-- This module defines
--
-- > data family Options Postscript V2 Double = PostscriptOptions
-- >           { _psfileName     :: String             -- ^ the name of the file you want generated
-- >           , _psSizeSpec     :: SizeSpec V2 Double -- ^ the requested size of the output
-- >           , _psOutputFormat :: OutputFormat        -- ^ the output format and associated options
-- >           }
