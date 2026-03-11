{- |
   Module      :  Main
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

module Main where
import System
import IO
import FMP.PP
import FMP.Picture    hiding (text, empty, space)
import FMP.Syntax
import FMP.File
import FMP

main                          :: IO ()
main                          =  getArgs
                              >>= \args
                              -> let filePath = head args in
                              let num         = head (tail args) in
                              readFile (filePath ++ "." ++ num ++ ".cache")
                              >>= \file
                              -> let fileName         = filePath ++ "."
                                                      ++ num ++ ".mp" in
                                      do param <- getParameters
                                         writeFile fileName
                                              (show (mpDoc num file param))
                                         err   <- system (mpBin param ++ " "
                                                              ++ fileName
                                                              ++ " >> /dev/null")
                                         if err == ExitSuccess
                                              then putStr ("\\includegraphics{"
                                                      ++ filePath ++ "." ++ num ++ "}")
                                                >> return ()
                                              else putStr "Error"
                                                >> return ()
      where
      mpDoc num file param    =  case [x | (x, t) <- reads file, ("", "") <- lex t] of
                      [x]     -> emit (metaPost (read num) x param)
                      _       -> text "end"
