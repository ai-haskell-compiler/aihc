{-# LANGUAGE OverloadedStrings #-}

--
-- |
-- Maintainer: Andrew Cowie
-- Stability: Experimental
--
-- /Background/
--
-- We had a need for identifiers that could be used by humans.
--
-- The requirement to be able to say these over the phone complicates matters.
-- Most people have approached this problem by using a phonetic alphabet. The
-- trouble comes when you hear people saying stuff like \"A as in ... uh,
-- Apple?\" (should be Alpha, of course) and \"U as in ... um, what's a word
-- that starts with U?\" It gets worse. Ever been to a GPG keysigning? Listen
-- to people attempt to read out the digits of their key fingerprints. ...C 3 E
-- D  0 0 0 0  0 0 0 2 B D B D... \"Did you say \'C\' or \'D\'?\" and \"how
-- many zeros was that?\" Brutal.
--
-- So what we need is a symbol set where each digit is unambigious and doesn't
-- collide with the phonetics of another symbol. This package provides
-- 'English16', a set of 16 letters and numbers that, when spoken in /English/,
-- have unique pronounciation and have been very successful in verbal
-- communications over noisy links.
--
-- Ironically, however, when used in written applications the English16 set is
-- a bit restrictive. When /looking/ at them they don't have much variety (it
-- turned out they're very blocky—so much so you have to squint). If the
-- application is transcription or identification visually then the criteria is
-- shapes that are distinct, rather than their sound. For these uses we provide
-- 'Latin25', a set of 25 symbols useful for identifiers in automated systems
-- that nevertheless have to be operated or debugged by humans.
--
-- Finally, also included is code to work in base 62, which is simply
-- @[\'0\'@-@\'9\'@, @\'A\'@-@\'Z\'@, and @\'a\'@-@\'z\']@. These are
-- frequently used to express short codes in URL redirectors; you may find them
-- a more useful encoding for expressing numbers than base 16 hexidecimal.
--
module Data.Locator
(
    -- * English16: locators humans can exchange
    -- | This was somewhat inspired by the record locators used by the civilian
    -- air travel industry, but with the restriction that the symbol set is
    -- carefully chosen (aviation locators do heroic things like excluding
    -- \'I\' but not much else) and, in the case of Locator16a, to not repeat
    -- symbols. They're not a reversable encoding, but assuming you're just
    -- generating identifiers and storing them somewhere, they're quite handy.
    --
    -- @TODO@ /link to paper with pronunciation study when published./
    --
    Locator(..)
  , English16(..)
  , fromEnglish16
  , toEnglish16
  , toEnglish16a
  , hashStringToEnglish16a

    -- * Latin25: a visually distinct character set
    -- An althernate character set chosen for visual distinctiveness (rather
    -- than the aural distinctiveness goal of "English16").
  , Latin25(..)
  , fromLatin25
  , toLatin25
  , hashStringToLatin25

    -- * Base62: binary without punctuation
  , toBase62
  , fromBase62
  , padWithZeros
  , hashStringToBase62

    -- * Deprecated functions
  , fromLocator16
  , toLocator16
  , toLocator16a
  , hashStringToLocator16a
) where

import Data.Locator.Common
import Data.Locator.Hashes
import Data.Locator.English16
import Data.Locator.Latin25
