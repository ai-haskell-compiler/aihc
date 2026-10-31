module Consumer where

import Original (Token)
import qualified "origin" Original as Source
import qualified "facade" Original as Facade
import qualified Alias
import qualified Renamed

fromAlias :: Token -> Alias.Token
fromAlias = Alias.identity

fromChain :: Alias.Token -> Renamed.Token
fromChain = Renamed.identity

fromFacade :: Source.Token -> Facade.Token
fromFacade = Facade.identity
