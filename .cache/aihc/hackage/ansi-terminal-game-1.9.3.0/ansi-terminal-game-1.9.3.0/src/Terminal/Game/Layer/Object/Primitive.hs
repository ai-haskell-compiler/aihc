{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Terminal.Game.Layer.Object.Primitive where

import Terminal.Game.Plane

import qualified Control.Monad.Catch as MC
import qualified GHC.Generics as G
import qualified Data.ByteString as BS
import qualified Data.Serialize as Z
import qualified Data.Sequence as S
import qualified Test.QuickCheck as Q

-------------------------------------------------------------------------------
-- Assorted API types

-- | The number of 'Tick's fed each second to the logic function;
-- constant on every machine. /Frames/ per second might be lower
-- (depending on drawing function onerousness, terminal refresh rate,
-- etc.).
type TPS = Integer

-- | The number of frames blit to terminal per second. Frames might be
-- dropped, but game speed will remain constant. Check @balls@
-- (@cabal run -f examples balls@) to see how to display FPS.
-- For obvious reasons (blits would be wasted) @max FPS = TPS@.
type FPS = Integer

-- | An @Event@ is a 'Tick' (time passes) or a 'KeyPress'.
--
-- Note that all @Keypress@es are recorded and fed to your game-logic
-- function. This means you will not lose a single character, no matter
-- how fast your player is at typing or how low you set 'FPS' to be.
--
-- Example: in a game where you are controlling a hot-air baloon and have
-- @direction@ and @position@ variables, you most likely want @direction@
-- to change at every @KeyPress@, while having @position@ only change at
-- @Tick@s.
data Event = Tick
           | KeyPress Char
              -- ↑↓→← do not work on Windows (are handled by the app,
              -- not passed to the program) both on cmd.exe and
              -- PowerShell.
           deriving (Show, Eq, G.Generic)
instance Z.Serialize Event where

instance Q.Arbitrary Event where
  arbitrary = Q.oneof [ pure Tick,
                        KeyPress <$> Q.arbitrary ]

-- | Game environment with current terminal dimensions and current display
-- rate.
data GEnv = GEnv { eTermDims :: Dimensions,
                        -- ^ Current terminal dimensions.
                   eFPS :: FPS
                        -- ^ Current blitting rate.
                       }
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- GRec record/replay game typs

-- | Opaque data type with recorded game input, for testing purposes.
data GRec = GRec { aPolled :: S.Seq [Event],
                                -- Seq. of polled events
                   aTermSize :: S.Seq (Maybe Dimensions) }
                                -- Seq. of polled termdims
        deriving (Show, Eq, G.Generic)
instance Z.Serialize GRec where

igrec :: GRec
igrec = GRec S.Empty S.Empty

addDims :: Maybe Dimensions -> GRec -> GRec
addDims mds (GRec p s) = GRec p (mds S.<| s)

getDims :: GRec -> (Maybe Dimensions, GRec)
getDims (GRec p (ds S.:|> d)) = (d, GRec p ds)
getDims _ = error "getDims: empty Seq"
    -- Have to use _ or “non exhaustive patterns” warning

addPolled :: [Event] -> GRec -> GRec
addPolled es (GRec p s) = GRec (es S.<| p) s

getPolled :: GRec -> ([Event], GRec)
getPolled (GRec (ps S.:|> p) d) = (p, GRec ps d)
getPolled _ = error "getPolled: empty Seq"

isOver :: GRec -> Bool
isOver (GRec S.Empty _) = True
isOver _ = False

-- | Reads a file containing a recorded session. Throws
-- 'MalformedGRec' on failure.
readRecord :: FilePath -> IO GRec
readRecord fp = Z.decode <$> BS.readFile fp >>= \case
                  Left e  -> MC.throwM (MalformedGRec e)
                  Right r -> return r

-- | Convenience function to create a 'GRec' from screen size (constant) plus a list of events. Useful with 'setupGame'.
createGRec :: Dimensions -> [Event] -> GRec
createGRec ds es = let l = length es * 2 in
                   GRec (S.fromList [es])
                        (S.fromList . replicate l $ Just ds)

-------------------------------------------------------------------------------
-- Exceptions

-- | @ATGException@s are thrown synchronously for easier catching.
data ATGException = CannotGetDisplaySize
                  | DisplayTooSmall Dimensions Dimensions
                        -- ^ Required and actual dimensions.
                  | MalformedGRec String
        deriving (Eq)

instance Show ATGException where
    show CannotGetDisplaySize = "CannotGetDisplaySize"
    show (DisplayTooSmall (sw, sh) tds) =
      let colS ww = ww < sw
          rowS wh = wh < sh

          smallMsg :: Dimensions -> String
          smallMsg (ww, wh) =
                let cm = show ww ++ " columns"
                    rm = show wh ++ " rows"
                    em | colS ww && rowS wh = cm ++ " and " ++ rm
                       | colS ww = cm
                       | rowS wh = rm
                       | otherwise = "smallMsg: passed correct term size!"
                in
                  "This games requires a display of " ++ show sw ++
                  " columns and " ++ show sh ++ " rows.\n" ++
                  "Yours only has " ++ em ++ "!\n\n" ++
                  "Please resize your terminal and restart the game.\n"
      in "DisplayTooSmall.\n" ++ smallMsg tds
    show (MalformedGRec e) = "MalformedGRec: " ++ e

instance MC.Exception ATGException where
