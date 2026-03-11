module Language.Haskell.TH.Now (nowQ)

where

import Data.Time.Clock
import Data.Time.Format
import Language.Haskell.TH

nowQ :: Q Exp
nowQ = runIO $ LitE . StringL <$> formattedTime
  where
    formattedTime =
      formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S%z")) <$>
      getCurrentTime
