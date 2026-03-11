{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Text.Show.ToolsYj (ShowIO(..), printIO) where

class ShowIO a where
	showIO :: a -> IO String

instance {-# OVERLAPPABLE #-} Show a => ShowIO a where
	showIO = pure . show

printIO :: ShowIO a => a -> IO ()
printIO = (putStrLn =<<) . showIO
