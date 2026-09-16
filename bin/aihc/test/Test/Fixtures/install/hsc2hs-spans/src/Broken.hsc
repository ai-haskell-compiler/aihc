module Broken (answer, oops) where

#include "broken.h"

-- Padding, so that this file and the module hsc2hs generates from it
-- disagree about which line anything is on: the #include above leaves
-- one line behind, and the constant below makes hsc2hs mark the rest
-- of the module with a line pragma. An error reported at the physical
-- line of the generated module would land on one of these comments.

answer :: Int
answer = #{const BROKEN_ANSWER}

oops :: Int
oops = deliberatelyUnbound
