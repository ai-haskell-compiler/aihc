module Main where

import Alone ( aloneInARoom, sizeCheck )

import Terminal.Game

-- Hot reloading is a handy feature while writing a game. Here I will
-- show you various ways to do that with ansi-terminal-game.
--
-- Hot reloading makes use of `entr` (install it from your repos) and
-- some additional scaffolding, provided by `tmux` or your plain terminal.
-- Read below to see two ideas in action.


{- === HOT RELOAD WITH ENTR AND TMUX ===

	1. Install `entr` and `tmux`.

    2. open a tmux window, in the bottom-right plane launch the game
       in an infinite loop, e.g.

         while true; do cabal run -f examples alone; done

       Remember, the pane has to be the bottom-right one, like this:

            +----------------------------------------------+
            |                        |                     |
            |                        |                     |
            |                        |                     |
            |                        |                     |
            |                        |                     |
            |                        |                     |
            |                        |---------------------|
            |                        |                     |
            |                        |                     |
            |                        |                     |
            |                        |       G A M E       |
            |                        |                     |
            |                        |                     |
            |                        |                     |
            +----------------------------------------------+

     3. in another pane launch `entr` in this fashion:

          find src example/ -name *.hs | entr tmux send-keys -t {bottom-right} q

     4. Now whenever you modify a source file in example/ , the game
        will be reloaded!  -}


{- === HOT RELOAD WITH ENTR AND PLAIN TERMINAL ===

    1. If your terminal has a `command` option, it is even easier.
       We will use `urxvt` for this example

          find src example/*.hs | entr -r urxvt -e cabal run -f examples alone

    2. Every time you save a source file, a terminal will be spawn with
       your game running in it.  -}


{-  === HOT RELOAD REPLAYS WITH ENTR ===

    `entr` by itself *cannot* autoreload a game in the same window, as it
    cannot handle interactive programs. But if you are just displaying a
    replay, this can come handy

        find example/*.hs | entr -cr cabal run -f examples hot-reload

    This is very useful to incrementally build NPCs’ behaviour,
    iron out mechanics bugs etc.

    Remember that you can use `recordGame` to record a session.  -}


-- If you you need something fancier for your game (e.g. hot reloading user
-- maps), `venzone` [1] (module Watcher) has a builtin /watch mode/ you can
-- take inspiration from.
--
-- [1] https://hackage.haskell.org/package/venzone

main :: IO ()
main = do
        sizeCheck
        gr <- readRecord "test/records/alone-record-test.gr"
                -- check `readRecord
        () <$ narrateGame aloneInARoom gr
