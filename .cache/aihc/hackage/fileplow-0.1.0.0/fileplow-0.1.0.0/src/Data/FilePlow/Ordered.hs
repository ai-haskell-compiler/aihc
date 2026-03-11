module Data.FilePlow.Ordered
    ( SeekTarget(..), seekTo )
where

import Data.FilePlow

import Numeric.Search

data SeekTarget b
    = StLargerThan !b
      -- ^ put the handle to the first entry that is larger than the provided value.
      -- Useful for files with entries in ASCending order
    | StSmallerThan !b
      -- ^ put the handle to the first entry that is smaller than the provided value.
      -- Useful for files with entries in DESCending order
    deriving (Show, Eq)

-- ^ place the file handling to a specific 'SeekTarget' provided a function that
-- extracts a value/entry at a specific position
seekTo ::
    (Ord b, Eq b, PlowHandle hdl)
    => hdl
    -> SeekTarget b
    -> (hdl -> IO b)
    -> IO Bool
seekTo hdl st extract =
    do s <- pFileSize hdl
       ranges <- searchM (fromTo 0 (s - 1)) divForever $ \pos ->
           do case st of
                StLargerThan _ -> pSeek hdl AbsoluteSeek pos
                StSmallerThan _ -> pSeek hdl AbsoluteSeek (s - 1 - pos)
              val <- extract hdl
              pure $
                  case st of
                    StLargerThan x -> (val >= x)
                    StSmallerThan x -> (val <= x)
       let tgtVal =
               case st of
                 StSmallerThan _ -> largest True ranges
                 StLargerThan _ -> smallest True ranges
       case tgtVal of
         Just pos ->
             case st of
               StLargerThan _ -> pSeek hdl AbsoluteSeek pos >> pure True
               StSmallerThan _ -> pSeek hdl AbsoluteSeek (s - 1 - pos) >> pure True
         Nothing -> pure False
