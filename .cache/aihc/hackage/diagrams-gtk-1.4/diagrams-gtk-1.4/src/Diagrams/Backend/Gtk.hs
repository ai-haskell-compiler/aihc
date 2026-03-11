{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Gtk
-- Copyright   :  (c) 2011 Diagrams-cairo team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Convenient interface to rendering diagrams directly
-- on Gtk widgets using the Cairo backend.
--
-----------------------------------------------------------------------------

module Diagrams.Backend.Gtk
       ( defaultRender
       , toGtkCoords
       , renderToGtk
       ) where

import           Diagrams.Backend.Cairo          as Cairo
import           Diagrams.Prelude                hiding (height, width)

-- Below hack is needed because GHC 7.0.x has a bug regarding export
-- of data family constructors; see comments in Diagrams.Backend.Cairo
#if __GLASGOW_HASKELL__ < 702 || __GLASGOW_HASKELL__ >= 704
import           Diagrams.Backend.Cairo.Internal
#endif

import qualified Graphics.Rendering.Cairo        as CG
import           Graphics.UI.Gtk

-- | Convert a Diagram to the backend coordinates.
--
-- Provided to Query the diagram with coordinates from a mouse click
-- event.
--
-- > widget `on` buttonPressEvent $ tryEvent $ do
-- >   click <- eventClick
-- >   (x,y) <- eventCoordinates
-- >   let result = runQuery (query $ toGtkCoords myDiagram) (x ^& y)
-- >   do_something_with result
--
-- `toGtkCoords` does no rescaling of the diagram, however it is centered in
-- the window.
toGtkCoords :: Monoid' m => QDiagram Cairo V2 Double m -> QDiagram Cairo V2 Double m
toGtkCoords d = (\(_,_,d') -> d') $
  adjustDia Cairo
            (CairoOptions "" absolute RenderOnly False)
            d

-- | Render a diagram to a DrawingArea with double buffering,
--   rescaling to fit the full area.
defaultRender :: Monoid' m => DrawingArea -> QDiagram Cairo V2 Double m -> IO ()
defaultRender drawingarea diagram = do
  drawWindow <- (widgetGetDrawWindow drawingarea)
  renderDoubleBuffered drawWindow opts diagram
    where opts w h = (CairoOptions
              { _cairoFileName     = ""
              , _cairoSizeSpec     = dims (V2 (fromIntegral w) (fromIntegral h))
              , _cairoOutputType   = RenderOnly
              , _cairoBypassAdjust = False
              }
           )

-- | Render a diagram to a 'DrawableClass' with double buffering.  No
--   rescaling or transformations will be performed.
--
--   Typically the diagram will already have been transformed by
--   'toGtkCoords'.
renderToGtk ::
  (DrawableClass dc, Monoid' m)
  => dc                     -- ^ widget to render onto
  -> QDiagram Cairo V2 Double m  -- ^ Diagram
  -> IO ()
renderToGtk drawable = do renderDoubleBuffered drawable opts
  where opts _ _ = (CairoOptions
                    { _cairoFileName     = ""
                    , _cairoSizeSpec     = absolute
                    , _cairoOutputType   = RenderOnly
                    , _cairoBypassAdjust = True
                    }
                   )


-- | Render a diagram onto a 'DrawableClass' using the given CairoOptions.
--
--   This uses cairo double-buffering.
renderDoubleBuffered ::
  (Monoid' m, DrawableClass dc) =>
  dc -- ^ drawable to render onto
  -> (Int -> Int -> Options Cairo V2 Double) -- ^ options, depending on drawable width and height
  -> QDiagram Cairo V2 Double m -- ^ Diagram
  -> IO ()
renderDoubleBuffered drawable renderOpts diagram = do
  (w,h) <- drawableGetSize drawable
  let opts = renderOpts w h
      renderAction = delete w h >> snd (renderDia Cairo opts diagram)
  renderWithDrawable drawable (doubleBuffer renderAction)


-- | White rectangle of size (w,h).
--
--   Used to clear canvas when using double buffering.
delete :: Int -> Int -> CG.Render ()
delete w h = do
  CG.setSourceRGB 1 1 1
  CG.rectangle 0 0 (fromIntegral w) (fromIntegral h)
  CG.fill


-- | Wrap the given render action in double buffering.
doubleBuffer :: CG.Render () -> CG.Render ()
doubleBuffer renderAction = do
  CG.pushGroup
  renderAction
  CG.popGroupToSource
  CG.paint
