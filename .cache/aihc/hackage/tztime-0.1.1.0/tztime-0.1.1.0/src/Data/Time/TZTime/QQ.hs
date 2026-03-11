-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

{-# LANGUAGE CPP #-}

module Data.Time.TZTime.QQ
  ( tz
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Time.TZTime.Internal as I
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Quote
import Text.ParserCombinators.ReadP qualified as P

#if MIN_VERSION_template_haskell(2,17,0)
import Language.Haskell.TH.Syntax (unTypeCode)
#else
import Language.Haskell.TH.Syntax (unType)
#endif

{- |
Quasiquoter for parsing a `TZTime` at compile-time in the format:
@yyyy-mm-dd hh:mm:ss[.sss] [±hh:mm] [time zone]@.

The offset is optional, except when the local time is ambiguous
(i.e. when the clocks are set back around that time in that time zone).

The offset can also be expressed using [military time zone abbreviations](https://www.timeanddate.com/time/zones/military),
and these time zones abbreviations as per RFC 822 section 5:
\"UTC\", \"UT\", \"GMT\", \"EST\", \"EDT\", \"CST\", \"CDT\", \"MST\", \"MDT\", \"PST\", \"PDT\".

Note: the time zone's rules are loaded from the embedded database
using `Data.Time.TZInfo.fromIdentifier`.

>>> [tz|2022-03-04 10:15:40.123 [Europe/Rome]|]
2022-03-04 10:15:40.123 +01:00 [Europe/Rome]

>>> [tz|2022-11-06 01:30:00 [America/Winnipeg]|]
...
    • Ambiguous time: please specify an offset.
      Did you mean any of the following?
      - 2022-11-06 01:30:00 -05:00 [America/Winnipeg]
      - 2022-11-06 01:30:00 -06:00 [America/Winnipeg]
...

-}
tz :: QuasiQuoter
tz = QuasiQuoter
  { quoteExp = qexp
  , quotePat = \_ -> fail "Cannot use 'tz' as a pattern."
  , quoteType = \_ -> fail "Cannot use 'tz' as a type."
  , quoteDec = \_ ->fail "Cannot use 'tz' as a declaration."
  }
  where
    qexp :: String -> Q Exp
    qexp input = do
      (lt, offsetMaybe, ident) <- I.readP_to_Q input (I.readComponentsP <* P.skipSpaces)
      I.getValidTZTimes lt ident >>= I.checkOffset offsetMaybe >>= \case
        tzt :| [] -> toExp tzt
        tzts -> fail $ "Ambiguous time: please specify an offset.\n" <> I.mkSuggestions tzts

    toExp :: TZTime -> Q Exp
    toExp tzt =
#if MIN_VERSION_template_haskell(2,17,0)
      unTypeCode $ I.liftTZTime tzt
#else
      unType <$> I.liftTZTime tzt
#endif
