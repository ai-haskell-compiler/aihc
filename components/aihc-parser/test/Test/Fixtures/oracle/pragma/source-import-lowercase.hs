{- ORACLE_TEST xfail importDeclSource is a Bool so the pretty-printer always emits {-# SOURCE #-} -}
module SourceImportLowercase where

import {-# source #-} SourceLoopA
