{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Main.Vendor
Description : Details about the application.
Copyright   : (c) 2020-2022 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Module providing info about this application.
-}

module Main.Vendor
  ( buildVersion
  , productDesc
  , productInfo
  , productName
  , webRepo
  )
where

import           Data.String                    ( IsString(..) )
import           Data.Version                   ( showVersion )
import           Paths_vcs_ignore               ( version )


-- | Product version.
buildVersion :: IsString a => a
buildVersion = fromString . showVersion $ version


productDesc :: IsString a => a
productDesc = "library for handling files ignored by VCS systems"


-- | Product info.
productInfo :: (IsString a, Monoid a) => a
productInfo = mconcat [productName, ", v", buildVersion, " :: ", webRepo]


-- | Product full name.
productName :: IsString a => a
productName = "vcs-ignore"


-- | Product source code repository.
webRepo :: IsString a => a
webRepo = "https://github.com/vaclavsvejcar/vcs-ignore"
