{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Postscript.CmdLine
-- Copyright   :  (c) 2013 Diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Convenient creation of command-line-driven executables for
-- rendering diagrams using the Postscript backend.
--
-- * 'defaultMain' creates an executable which can render a single
--   diagram at various options.
--
-- * 'multiMain' is like 'defaultMain' but allows for a list of
--   diagrams from which the user can choose one to render.
--
-- * 'pagesMain' is like 'defaultMain' but renders a list of
--   diagrams as pages in a single file.
--
-- * 'animMain' renders an animation at a given frame rate
--   into separate files with an index number.
--
-- * 'mainWith' is a generic form that does all of the above but with
--   a slightly scarier type.  See "Diagrams.Backend.CmdLine".  This
--   form can also take a function type that has a subtable final result
--   (any of arguments to the above types) and 'Parseable' arguments.
--
-- If you want to generate diagrams programmatically---/i.e./ if you
-- want to do anything more complex than what the below functions
-- provide---you have several options.
--
-- * Use a function with 'mainWith'.  This may require making
--   'Parseable' instances for custom argument types.
--
-- * Make a new 'Mainable' instance.  This may require a newtype
--   wrapper on your diagram type to avoid the existing instances.
--   This gives you more control over argument parsing, intervening
--   steps, and diagram creation.
--
-- * Build option records and pass them along with a diagram to 'mainRender'
--   from "Diagrams.Backend.CmdLine".
--
-- * An even more flexible approach is to directly call 'renderDia'; see
--   "Diagrams.Backend.Postscript" for more information.
--
-- For a tutorial on command-line diagram creation see
-- <https://diagrams.github.io/doc/cmdline.html>.
--
-----------------------------------------------------------------------------

module Diagrams.Backend.Postscript.CmdLine
       (
         -- * General form of @main@
         -- $mainwith

         mainWith

         -- * Supported forms of @main@

       , defaultMain
       , multiMain
       , pagesMain
       , animMain

         -- * Backend tokens

       , Postscript
       , B

      ) where

import qualified Data.ByteString.Builder     as B

import           Diagrams.Backend.CmdLine
import           Diagrams.Backend.Postscript
import           Diagrams.Prelude            hiding (height, interval, option,
                                              output, value, width, (<>))

import           Data.List.Split

-- $mainwith
-- The 'mainWith' method unifies all of the other forms of @main@ and is
-- now the recommended way to build a command-line diagrams program.  It
-- works as a direct replacement for 'defaultMain', 'multiMain', 'pagesMain',
-- or 'animMain' as well as allowing more general arguments.  For example,
-- given a function that produces a diagram when given an @Int@ and a @'Colour'
-- Double@, 'mainWith' will produce a program that looks for additional number
-- and color arguments.
--
-- > ... definitions ...
-- > f :: Int -> Colour Double -> QDiagram Postscript V2 Double Any
-- > f i c = ...
-- >
-- > main = mainWith f
--
-- We can run this program as follows:
--
-- > $ ghc --make MyDiagram
-- >
-- > # output image.eps built by `f 20 red`
-- > $ ./MyDiagram -o image.eps -w 200 20 red



-- | This is the simplest way to render diagrams, and is intended to
--   be used like so:
--
-- > ... other definitions ...
-- > myDiagram = ...
-- >
-- > main = defaultMain myDiagram
--
--   Compiling a source file like the above example will result in an
--   executable which takes command-line options for setting the size,
--   output file, and so on, and renders @myDiagram@ with the
--   specified options.
--
--   Pass @--help@ to the generated executable to see all available
--   options.  Currently it looks something like
--
-- @
-- ./Program
--
-- Usage: ./Program [-w|--width WIDTH] [-h|--height HEIGHT] [-o|--output OUTPUT]
--   Command-line diagram generation.
--
-- Available options:
--   -?,--help                Show this help text
--   -w,--width WIDTH         Desired WIDTH of the output image
--   -h,--height HEIGHT       Desired HEIGHT of the output image
--   -o,--output OUTPUT       OUTPUT file
-- @
--
--   For example, a common scenario is
--
-- @
-- $ ghc --make MyDiagram
--
--   # output image.eps with a width of 400pt (and auto-determined height)
-- $ ./MyDiagram -o image.eps -w 400
-- @

defaultMain :: QDiagram Postscript V2 Double Any -> IO ()
defaultMain = mainWith

instance Mainable (QDiagram Postscript V2 Double Any) where
    type MainOpts (QDiagram Postscript V2 Double Any) = DiagramOpts

    mainRender opts d = chooseRender opts (renderDia' d)

chooseRender :: DiagramOpts -> (Options Postscript V2 Double -> IO ()) -> IO ()
chooseRender opts renderer =
  case splitOn "." (opts^.output) of
    [""] -> putStrLn "No output file given."
    ps |  last ps `elem` ["eps"]
       || last ps `elem` ["ps"] -> do
           let outfmt   = EPS
               sizeSpec = fromIntegral <$> mkSizeSpec2D (opts^.width) (opts^.height)

           renderer (PostscriptOptions (opts^.output) sizeSpec outfmt)
       | otherwise -> putStrLn $ "Unknown file type: " ++ last ps

-- | @multiMain@ is like 'defaultMain', except instead of a single
--   diagram it takes a list of diagrams paired with names as input.
--   The generated executable then takes a @--selection@ option
--   specifying the name of the diagram that should be rendered.  The
--   list of available diagrams may also be printed by passing the
--   option @--list@.
--
--   Example usage:
--
-- @
-- $ ghc --make MultiTest
-- [1 of 1] Compiling Main             ( MultiTest.hs, MultiTest.o )
-- Linking MultiTest ...
-- $ ./MultiTest --list
-- Available diagrams:
--   foo bar
-- $ ./MultiTest --selection bar -o Bar.eps -w 200
-- @

multiMain :: [(String, QDiagram Postscript V2 Double Any)] -> IO ()
multiMain = mainWith

instance Mainable [(String,QDiagram Postscript V2 Double Any)] where
    type MainOpts [(String,QDiagram Postscript V2 Double Any)] = (DiagramOpts, DiagramMultiOpts)

    mainRender = defaultMultiMainRender

-- | @pagesMain@ is like 'defaultMain', except instead of a single
--   diagram it takes a list of diagrams and each will be rendered as a page
--   in the Postscript file.
--
--   Example usage:
--
-- @
-- $ ghc --make MultiPage
-- [1 of 1] Compiling Main             ( MultiPage.hs, MultiPage.o )
-- Linking MultiPage ...
-- $ ./MultiPage -o Pages.ps -w 200
-- @

pagesMain :: [QDiagram Postscript V2 Double Any] -> IO ()
pagesMain = mainWith

instance Mainable [QDiagram Postscript V2 Double Any] where
    type MainOpts [QDiagram Postscript V2 Double Any] = DiagramOpts

    mainRender opts ds = chooseRender opts (renderDias' ds)

-- | @animMain@ is like 'defaultMain', but renders an animation
-- instead of a diagram.  It takes as input an animation and produces
-- a command-line program which will crudely \"render\" the animation
-- by rendering one image for each frame, named by extending the given
-- output file name by consecutive integers.  For example if the given
-- output file name is @foo\/blah.eps@, the frames will be saved in
-- @foo\/blah001.eps@, @foo\/blah002.eps@, and so on (the number of
-- padding digits used depends on the total number of frames).  It is
-- up to the user to take these images and stitch them together into
-- an actual animation format (using, /e.g./ @ffmpeg@).
--
--   Of course, this is a rather crude method of rendering animations;
--   more sophisticated methods will likely be added in the future.
--
-- The @--fpu@ option can be used to control how many frames will be
-- output for each second (unit time) of animation.
animMain :: Animation Postscript V2 Double -> IO ()
animMain = mainWith

instance Mainable (Animation Postscript V2 Double) where
    type MainOpts (Animation Postscript V2 Double) = (DiagramOpts, DiagramAnimOpts)

    mainRender = defaultAnimMainRender mainRender output
