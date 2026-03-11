-- |
-- Module      : Test.LeanCheck.WithInstances
-- Copyright   : (c) 2018-2022 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- "Test.LeanCheck" with additional 'Listable' instances
-- from "Test.LeanCheck.Instances".
--
-- Importhing this is the same as:
--
-- > import Test.LeanCheck
-- > import Test.LeanCheck.Instances
module Test.LeanCheck.WithInstances (module Test.LeanCheck) where

import Test.LeanCheck
import Test.LeanCheck.Instances
