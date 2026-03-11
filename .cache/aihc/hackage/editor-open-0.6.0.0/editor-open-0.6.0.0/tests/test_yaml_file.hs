-- Copyright 2015 Peter Harpending
-- 
-- Licensed under the Apache License, Version 2.0 (the "License"); you
-- may not use this file except in compliance with the License.  You
-- may obtain a copy of the License at
-- 
--     http://www.apache.org/licenses/LICENSE-2.0
-- 
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
-- implied.  See the License for the specific language governing
-- permissions and limitations under the License.


-- | 
-- Module      : Main
-- Description : Test of editor-open
-- Copyright   : Copyright 2015 Peter Harpending
-- License     : Apache-2.0
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : POSIX
-- 

module Main where

import qualified Data.ByteString as B
import           Paths_editor_open
import           System.IO
import           Text.Editor

main :: IO ()
main =
  getDataFileName "res/bug-schema.yaml" >>=
  runUserEditorDWIMFile yamlTemplate >>=
  B.hPut stdout
