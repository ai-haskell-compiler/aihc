-- |
-- This module expors single polymorphic function 'once', that allows you
-- to memoize IO actions and functions, evaluating them at most once.
-- Here is example:
--
-- >>> let mkStamp = (putStrLn "stamping" >> writeFile "/tmp/stamp" "") :: IO ()
-- >>> -- onceStamp :: IO ()
-- >>> onceStamp <- once mkStamp
-- >>> -- onceStamp actually evaluates mkStamp it wraps first time.
-- >>> onceStamp
-- stamping
-- >>> -- but second time result `()' is memoized, no action is performed.
-- >>> onceStamp
-- >>> -- we can memoize functions too
-- >>> foo <- once $ \x -> print "foo" >> print (x :: Int)
-- >>> -- action will be performed once for every distinct argument
-- >>> foo 10
-- foo
-- 10
-- >>> foo 10
-- 10
-- >>> foo 4
-- foo
-- 4

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

module Control.Once (once) where
import Control.Once.Internal
import Control.Once.Class
import Control.Once.TH
import Prelude hiding (lookup)

instance Once (IO a) where once = once0
$(fmap concat $ mapM deriveOnce [1..7])
