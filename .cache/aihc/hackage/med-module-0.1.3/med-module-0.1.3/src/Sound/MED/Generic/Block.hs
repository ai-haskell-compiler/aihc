module Sound.MED.Generic.Block where

import qualified Sound.MED.Raw.MMD0Block as MMD0Block
import qualified Sound.MED.Raw.MMD0NoteData as MMD0NoteData
import qualified Sound.MED.Raw.MMD1Block as MMD1Block
import qualified Sound.MED.Raw.MMD1NoteData as MMD1NoteData
import qualified Sound.MED.Raw.BlockInfo as BlockInfo
import qualified Sound.MED.Raw.BlockCmdPageTable as BlockCmdPageTable
import qualified Sound.MED.Raw.CmdPageData as CmdPageData

import Sound.MED.Basic.Human(Human(human),bold)
import Sound.MED.Basic.Utility(stringFromBytes)

import Text.Printf(printf)
import Control.Monad(liftM2)
import Data.Bits(shiftR, (.&.))
import Data.Maybe(catMaybes)

type Note      = Int
type Inst      = Int
type Cmd       = Int
type Val       = Int
type Highlight = Bool
type Line      = ( Maybe Highlight, [ ( Note, Inst, [ ( Cmd, Val ) ] ) ])

data MEDBlock = MEDBlock
  { name    :: Maybe String
  , tracks  :: Int
  , lines   :: Int
  , pages   :: Int
  , seqdata :: [ Line ]
  }

medblock0 :: MMD0Block.MMD0Block -> MEDBlock
medblock0 b =
  let name'       = Nothing
      tracks'     = fromIntegral $ MMD0Block.numtracks b
      lines'      = fromIntegral $ MMD0Block.lines     b + 1
      pages'      = 1
      highlights' = repeat Nothing
      f           = fromIntegral
      g (MMD0NoteData.MMD0NoteData n i c v) = (f n, f i, [(f c, f v)])
      notedata'   = map (map g) (MMD0Block.notedata b)
      seqdata'    = zip highlights' notedata'
  in MEDBlock name' tracks' lines' pages' seqdata'


medblock1 :: MMD1Block.MMD1Block -> MEDBlock
medblock1 b =
  let i           = MMD1Block.info b
      name'       = fmap stringFromBytes $ BlockInfo.blockname =<< i
      tracks'     = fromIntegral $ MMD1Block.numtracks b
      lines'      = fromIntegral $ MMD1Block.lines     b + 1
      pages'      = case BlockInfo.pagetable =<< i of
        Nothing -> 1
        Just pt -> 1 + (length . catMaybes $ BlockCmdPageTable.pages pt)
      hlbit h bpos = ((h `shiftR` bpos) .&. 1) == 1
      hlbits h    = map (hlbit h) [0..31]
      highlights' = case BlockInfo.hlmask =<< i of
        Nothing -> repeat Nothing
        Just hl -> map Just $ concatMap hlbits $ hl
      fI          = fromIntegral
      nd (MMD1NoteData.MMD1NoteData n j c v) = (fI n, fI j, [(fI c, fI v)])
      notedata'   = map (map nd) (MMD1Block.notedata b)
      cv (CmdPageData.CmdPageData c v) = (fI c, fI v)
      cmddata'    = case BlockInfo.pagetable =<< i of
        Nothing -> []
        Just pt -> map (map (map cv)) (catMaybes (BlockCmdPageTable.pages pt))
      p (c,v) (n,j,cvs) = (n, j, cvs ++ [(c,v)])
      ncdata'     = foldr (zipWith (zipWith p)) notedata' cmddata'
      seqdata'    = zip highlights' ncdata'
  in MEDBlock name' tracks' lines' pages' seqdata'


instance Human MEDBlock where
  human b =
    let name' = maybe "" (' ':) $ name b
        blocklines = Sound.MED.Generic.Block.lines
        dim'  = printf "%d*%d*%d" (blocklines b) (tracks b) (pages b)
        seq'  = unlines (zipWith highlightLine [0..] (seqdata b))
    in dim' ++ name' ++ "\n" ++ seq'

highlightLine :: Int -> Line -> String
highlightLine i (highlight, ds) =
  let bLine = humanLine i ds
  in if highlight == Just True then bold bLine else bLine

humanLine :: Int -> [ ( Note, Inst, [ ( Cmd, Val ) ] ) ] -> String
humanLine i ds =
  let mapWords fmt = concatMap (' ':) . map fmt
      hCV (c, v) = printf "%02X%02X" c v
      hTrack (n, j, cvs) = printf "%s %02X%s" (notes!!n) j (mapWords hCV cvs)
  in  printf "%04X:%s" i (mapWords hTrack ds)

notes :: [String]
notes =
  "---" :
  liftM2 (flip (printf "%s%1X"))
    [(1::Int) ..]
    ["C-","C#","D-","D#","E-","F-","F#","G-","G#","A-","A#","B-"]
