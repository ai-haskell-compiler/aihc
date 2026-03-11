{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module HaskellWorks.Data.Model.Lens where

import Control.Lens
import HaskellWorks.Data.Model.Type

makeFields ''Storage
makeFields ''Mount
