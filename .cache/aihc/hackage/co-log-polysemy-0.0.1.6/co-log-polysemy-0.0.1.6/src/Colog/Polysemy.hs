{- |
Module                  : Colog.Polysemy
Copyright               : (c) 2019-2020 Kowainik
                          (c) 2021-2025 Co-Log
SPDX-License-Identifier : MPL-2.0
Maintainer              : Co-Log <xrom.xkov@gmail.com>
Stability               : Provisional
Portability             : Portable

This package contains extensible effects implementation of composable, contravariant and
comonadic logging based on @co-log-core@ and @polysemy@.
-}

module Colog.Polysemy
       ( module Poly
       ) where

import Colog.Polysemy.Effect as Poly
