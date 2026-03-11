-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Test.Utils where

import Data.Time (TimeZone)

cst :: TimeZone
cst = read "CST"

cdt :: TimeZone
cdt = read "CDT"
