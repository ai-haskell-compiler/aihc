-------------------------------------------------------------------------------
-- Layer 1 (imperative), as per
-- https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html
-- 2019 Francesco Ariis GPLv3
-------------------------------------------------------------------------------

{-# Language ScopedTypeVariables #-}

module Terminal.Game.Layer.Imperative where

import Terminal.Game.Draw
import Terminal.Game.Layer.Object

import qualified Control.Concurrent as CC
import qualified Control.Exception as E
import qualified Control.Monad as CM
import qualified Data.Bool as B
import qualified Data.Either as ET
import qualified Data.List as D
import qualified System.IO as SI

import Terminal.Game.Plane

-- | Game definition datatype, parametrised on:
--
-- * your gamestate @s@; and
-- * a result when the game is finished @r@. Simple games do not need this,
--   just fill @r@ with @()@.
--
-- The two most important elements are the function dealing with logic and
-- the drawing one. Check @alone@ demo (@cabal run -f examples alone@) to
-- see a basic game in action.
data Game s r = Game {
        gTPS           :: TPS,
            -- ^ Game speed in ticks per second. You do not
            -- need high values, since the 2D canvas is coarse
            -- (e.g. 13 TPS is enough for action games).
        gInitState     :: s,   -- ^ Initial state of the game.
        gLogicFunction :: GEnv -> s -> Event -> Either r s,
            -- ^ Logic function.  If `gLogicFunction` returns @Right s@
            -- the game will continue with state @s@; if it returns @Left@
            -- the game is over (quit condition).
            --
            -- Curious to see how @r@ can be useful? Check
            -- @cabal run -f examples balls@ and
            -- @example/MainBalls.hs@.
        gDrawFunction  :: GEnv -> s -> Plane
            -- ^ Draw function. Just want to blit your game
            -- in the middle? Check 'centerFull'.
    }

-- | A blank plane as big as the terminal.
blankPlaneFull :: GEnv -> Plane
blankPlaneFull e = uncurry blankPlane (eTermDims e)

-- | Blits plane in the middle of terminal.
--
-- @
--   draw :: GEnv -> MyState -> Plane
--   draw ev s =
--       centerFull ev $
--         ⁝
-- @
centerFull :: GEnv -> Plane -> Plane
centerFull e p = blankPlaneFull e *** p

-- | Entry point for the game execution, should be called in @main@.
--
-- You __must__ compile your programs with @-threaded@; if you do not do
-- this the game will crash, at start-up. Just add:
--
-- @
-- ghc-options:      -threaded
-- @
--
-- in your @.cabal@ file and you will be fine!
playGame :: Game s r -> IO r
playGame g = either id (error "`Right` in playGame") <$>
               runGIO (runGameGeneral g)

-- | As 'playGame', but ignore the result @r@.
playGame_ :: Game s r -> IO ()
playGame_ g = () <$ playGame g

-- | Tests a game in a /pure/ environment. Aims to accurately emulate 'GEnv'
-- changes (screen size, FPS) too. Returns a result @r@ or a state @s@ in
-- case the Event stream is exhausted before the game exits.
--
-- A useful trick is to call 'recordGame' and press /Ctrl-C/ while playing
-- (instead of quitting properly). This way @testGame@ will return
-- @Left s@, a state that you can then inspect.
testGame :: Game s r -> GRec -> Either r s
testGame g ts =
        case runTest (runGameGeneral g) ts of
            (Nothing, l) -> error $ "testGame, exception called: " ++
                                    show l
                -- it is fine to use error here since in the end
                -- hspec can deal with it gracefully and we give
                -- more infos on a failed test
            (Just s, _) -> s

-- | As 'testGame', but returns 'Game' instead of result/state.
-- Useful to fast-forward (e.g.: skip menus) before invoking 'playGame'.
setupGame :: Game s r -> GRec -> Game s r
setupGame g ts = let s' = testGame g ts
                 in case s' of
                      -- If the game is already over, return a mock logic
                      -- function which simply ends the game.
                      Left r -> g { gLogicFunction = \_ _ _ -> Left r }
                      Right s -> g { gInitState = s }

-- | Similar to 'testGame', runs the game given a 'GRec'. Unlike
-- 'testGame', the playthrough will be displayed on screen. Useful when a
-- test fails and you want to see how.
--
-- See this in action with  @cabal run -f examples alone-playback@.
--
-- Notice that 'GEnv' will be provided at /run-time/, and not
-- record-time; this can make emulation slightly inaccurate if — e.g. —
-- you replay the game on a smaller terminal than the one you recorded
-- the session on.
narrateGame :: Game s r -> GRec -> IO ()
narrateGame g e = () <$ runReplay (runGameGeneral g) e

-- | Play as in 'playGame' and write the session (input stream, etc.) to
-- @file@. Then you can use this with 'testGame' and 'narrateGame'. Session
-- will be recorded even if an exception happens while playing.
recordGame :: Game s r -> FilePath -> IO ()
recordGame g fp =
        E.bracket
          (CC.newMVar igrec)
          (\ve -> writeRec fp ve)
          (\ve -> () <$ runRecord (runGameGeneral g) ve)

data Config = Config { cMEvents :: CC.MVar [Event],
                       cTPS     :: TPS }

runGameGeneral :: forall s r m. MonadGameIO m =>
                  Game s r -> m (Either r s)
runGameGeneral (Game tps s lf df) =
            -- init
            setupDisplay    >>
            startEvents tps >>= \(InputHandle ve ts) ->
            displaySizeErr  >>= \ds ->

            -- do it!
            let c = Config ve tps in
            cleanUpErr (game c ds)
                            -- this under will be run regardless
                       (stopEvents ts >>
                        shutdownDisplay  )
    where
          game :: MonadGameIO m => Config -> Dimensions -> m (Either r s)
          game c wds = gameLoop c (Right s) lf df
                                Nothing wds
                                (creaFPSCalc tps)

-- | Wraps an @IO@ computation so that any 'ATGException' or 'error' gets
-- displayed along with a @\<press any key to quit\>@ prompt.
-- Some terminals shut-down immediately upon program end; adding
-- @errorPress@ to 'playGame' makes it easier to beta-test games on those
-- terminals.
errorPress :: IO a -> IO a
errorPress m = E.catches m [E.Handler errorDisplay,
                            E.Handler atgDisplay]
    where
          errorDisplay :: E.ErrorCall -> IO a
          errorDisplay (E.ErrorCallWithLocation cs l) = report $
              putStrLn (cs ++ "\n\n")        >>
              putStrLn "Stack trace info:\n" >>
              putStrLn l

          atgDisplay :: ATGException -> IO a
          atgDisplay e = report $ print e

          report :: IO () -> IO a
          report wm =
              putStrLn "ERROR REPORT\n"                >>
              wm                                       >>
              putStrLn "\n\n <Press any key to quit>"  >>
              SI.hSetBuffering SI.stdin SI.NoBuffering >>
              getChar                                  >>
              errorWithoutStackTrace "errorPress"


-----------
-- LOGIC --
-----------

-- from http://www.loomsoft.net/resources/alltut/alltut_lesson6.htm
gameLoop :: MonadGameIO m     =>
            Config            ->  -- event source
            Either r s        ->  -- state
            (GEnv ->
              s -> Event ->
              Either r s)     ->  -- logic function
            (GEnv ->
             s -> Plane)      ->  -- draw function
            Maybe Plane       ->  -- last blitted screen
            Dimensions        ->  -- Term dimensions
            FPSCalc           ->  -- calculate fps
            m (Either r s)
gameLoop c s lf df opln td fps =

        -- Quit?
        areEventsOver >>= \qb ->
            -- We will quit in case input stream (events) is exhausted.
            -- This might happen during test/narrate.
        if ET.isLeft s || qb
          then return s
        else

        -- Fetch events (if any).
        -- This is safe as we checked for `areEventsOver` above.
        pollEvents (cMEvents c) >>= \es ->

        -- no events? skip everything
        if null es
          then sleepABit (cTPS c)               >>
               gameLoop c s lf df opln td fps
        else

        displaySizeErr          >>= \td' ->

        -- logic
        let ge = GEnv td' (calcFPS fps)
            (i, s') = stepsLogic s (lf ge) es in

        -- no `Tick` events? You do not need to blit, just update state
        if i == 0
          then gameLoop c s' lf df opln td fps
        else

        -- FPS calc
        let fps' = addFPS i fps in

        -- clear screen if resolution changed
        let resc = td /= td' in
        CM.when resc clearDisplay >>

        -- draw
        let
            opln' | resc = Nothing -- res changed? restart double buffering
                  | otherwise = opln
            npln = case s' of
                    (Right rs) -> df ge rs
                    (Left _) -> uncurry blankPlane td'
                    -- In case the logic function came to an end
                    -- (Left), just print a blank plane.
        in

        blitPlane opln' npln >>

        gameLoop c s' lf df (Just npln) td' fps'

-- Int = number of `Tick` events
stepsLogic :: Either r s -> (s -> Event -> Either r s) -> [Event] ->
              (Integer, Either r s)
stepsLogic s lf es = let ies = D.genericLength . filter isTick $ es
                     in (ies, logicFold lf s es)
    where
          isTick Tick = True
          isTick _    = False

          logicFold :: (s -> Event -> Either r s) ->
                       Either r s -> [Event] -> Either r s
          logicFold _ (Left r) _ = Left r
          logicFold wlf (Right ws) wes = CM.foldM wlf ws wes


-------------------------------------------------------------------------------
-- Frame per Seconds

data FPSCalc = FPSCalc [Integer] TPS
    -- list with number of `Ticks` processed at each blit and expected
    -- FPS (i.e. TPS)

-- the size of moving average will be TPS (that simplifies calculations)
creaFPSCalc :: TPS -> FPSCalc
creaFPSCalc tps = FPSCalc (D.genericReplicate tps {- (tps*2) -} 1) tps
    -- tps*1: size of thw window in **blit actions** (not tick actions!)
    --        so keeping it small should be responsive and non flickery
    --        at the same time!

-- add ticks
addFPS :: Integer -> FPSCalc -> FPSCalc
addFPS nt (FPSCalc (_:fps) tps) = FPSCalc (fps ++ [nt]) tps
addFPS _ (FPSCalc [] _) = error "addFPS: empty list."

calcFPS :: FPSCalc -> Integer
calcFPS (FPSCalc fps tps) =
        let ts = sum fps
            ds = D.genericLength fps
        in roundQuot (tps * ds) ts
    where
          roundQuot :: Integer -> Integer -> Integer
          roundQuot a b = let (q, r) = quotRem a b
                          in q + B.bool 0 1 (r > div b 2)
