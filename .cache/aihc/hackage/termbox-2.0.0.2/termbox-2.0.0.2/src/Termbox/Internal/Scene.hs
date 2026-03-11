module Termbox.Internal.Scene
  ( Scene,
    render,
    image,
    fill,
    cursor,
  )
where

import Termbox.Bindings.Hs hiding (bg)
import Termbox.Internal.Color (Color, MaybeColor, justColor, nothingColor, unMaybeColor)
import Termbox.Internal.Image (Image (..))
import Termbox.Internal.Pos (Pos (..))
import qualified Termbox.Internal.Style as Style

-- | A scene, which contains an image, an optional background fill color, and an optional cursor.
--
-- * Create a scene with 'image'.
-- * Set a scene\'s background fill color with 'fill'.
-- * Set a scene\'s cursor position with 'cursor'.
data Scene = Scene
  { sceneDraw :: !(MaybeColor -> IO ()),
    sceneFill :: {-# UNPACK #-} !MaybeColor
  }

-- | Render a scene.
render :: Scene -> IO ()
render Scene {sceneFill, sceneDraw} = do
  tb_set_cursor Nothing
  tb_set_clear_attributes _TB_DEFAULT (unMaybeColor sceneFill)
  tb_clear
  sceneDraw sceneFill
  tb_present

-- | Create a scene from an image.
image :: Image -> Scene
image (Image draw) =
  Scene
    { sceneDraw = draw mempty . Style.maybeFill,
      sceneFill = nothingColor
    }

-- | Set a scene's background fill color.
fill :: Color -> Scene -> Scene
fill color scene =
  scene {sceneFill = justColor color}

-- | Set a scene's cursor position.
cursor :: Pos -> Scene -> Scene
cursor Pos {col, row} scene =
  scene
    { sceneDraw = \background -> do
        sceneDraw scene background
        tb_set_cursor (Just (col, row))
    }
