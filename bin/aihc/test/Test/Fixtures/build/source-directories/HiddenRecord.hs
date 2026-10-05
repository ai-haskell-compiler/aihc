module HiddenRecord (Options (limit, verbose), defaultOptions) where

data Options = Options {limit :: Int, verbose :: Bool}

defaultOptions :: Options
defaultOptions = Options 1 True
