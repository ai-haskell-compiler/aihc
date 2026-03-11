-- | This library provides parser builders akin to what optparse-applicative
-- offers for 'String' out of the Box, but for 'Data.Text'.
module Options.Applicative.Text
  ( text
  , textOption
  , textArgument ) where

import           Control.Applicative       ((<$>))
import qualified Data.Text                 as T
import qualified Options.Applicative       as Opt
import qualified Options.Applicative.Types as Opt

-- | Text 'Option' reader.
text :: Opt.ReadM T.Text
text = T.pack <$> Opt.readerAsk

-- | Builder for an option taking a 'Data.Text' argument.
textOption :: Opt.Mod Opt.OptionFields T.Text -> Opt.Parser T.Text
textOption = Opt.option text

-- | Builder for a 'Text' argument.
textArgument :: Opt.Mod Opt.ArgumentFields T.Text -> Opt.Parser T.Text
textArgument = Opt.argument text
