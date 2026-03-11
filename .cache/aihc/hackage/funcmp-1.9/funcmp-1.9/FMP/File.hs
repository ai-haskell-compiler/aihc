{- |
   Module      :  FMP.File
   Copyright   :  (c) 2003-2010 Peter Simons
                  (c) 2002-2003 Ferenc Wágner
                  (c) 2002-2003 Meik Hellmund
                  (c) 1998-2002 Ralf Hinze
                  (c) 1998-2002 Joachim Korittky
                  (c) 1998-2002 Marco Kuhlmann
   License     :  GPLv3
   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  portable
 -}
{-
  This program is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation, either version 3 of the License, or (at your option) any later
  version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  this program. If not, see <http://www.gnu.org/licenses/>.
 -}

module FMP.File (
      Parameters(..),
      fileExists, clearFile, getParameters
      ) where

import System.IO
import Control.Exception

data Parameters               =  Parameters{  mpBin           :: String,
                                              funcmpBin,
                                              funcmpRTS       :: String,
                                              defaultDX,
                                              defaultDY,
                                              textDX,
                                              textDY          :: Double,
                                              newmp           :: Bool,
                                              prolog,
                                              epilog          :: String}
                                 deriving (Eq, Read, Show)

stdParameters                 :: Parameters
stdParameters                 =  Parameters{  mpBin           = "virmp",
                                              funcmpBin       = "FuncMP",
                                              funcmpRTS       = "+RTS -H10m -RTS",    ---
                                              defaultDX       = 3,
                                              defaultDY       = 3,
                                              textDX          = 2,
                                              textDY          = 2,
                                              newmp           = False,
                                              prolog          = prolog',
                                              epilog          = "\\end"}


-- Existiert die Datei |file|, f"uhre |t| aus, sonst |f|.

fileExists                    :: String -> IO a -> (IOError -> IO a) -> IO a
fileExists file t             =  Control.Exception.catch (openFile file ReadMode
                                        >>= \h1 -> hClose h1
                                        >> t)

-- L"osche eine Datei

clearFile                     :: String -> IO a -> (IOError -> IO a) -> IO a
clearFile file t              =  Control.Exception.catch (openFile file WriteMode
                                        >>= \h1 -> hClose h1
                                        >> t)

getParameters                 :: IO Parameters
getParameters                 =  fileExists "fmp.ini"
                                      (readFile "fmp.ini"
                                       >>= \p-> let tok = tokens p
                                                in
                                                return (scanParameters tok stdParameters))
                                      (\_ -> return stdParameters)

tokens                        :: String -> [String]
tokens t                      =  if null a
                                      then []
                                      else a:tokens b
              where (a,b)     =  head (lex t)

changeParameters              :: Parameters -> String -> String -> Parameters
changeParameters p "prolog" c =  p{ prolog    = read c }
changeParameters p "epilog" c =  p{ epilog    = read c }
changeParameters p "mp_bin" c =  p{ mpBin     = read c }
changeParameters p "defaultDX" c
                              =  p{ defaultDX = read c }
changeParameters p "defaultDY" c
                              =  p{ defaultDY = read c }
changeParameters p "textDX" c =  p{ textDX    = read c }
changeParameters p "textDY" c =  p{ textDY    = read c }
changeParameters p "newmp" c  =  p{ newmp     = read c }
changeParameters p  "funcmp_rts" c
                              =  p{ funcmpRTS = read c }
changeParameters p  "funcmp_bin" c
                              =  p{ funcmpBin = read c }
changeParameters p _ _        =  p

scanParameters                :: [String] -> Parameters -> Parameters
scanParameters [] a           =  a
scanParameters (a:b:c:r) p    =  if b == "="
                                  then scanParameters r (changeParameters p a c)
                                  else scanParameters (b:c:r) p

prolog'                       :: String
prolog'                       =  "verbatimtex\n\
                                 \\\documentclass[11pt]{report}\n\
                                 \\\begin{document}\n\
                                 \etex\n\n\
                                 \input boxes\n\
                                 \input FuncMP"
