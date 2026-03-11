{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Language.Haskell.TH.TestUtils.QState (
  QState (..),
  ReifyInfo (..),
  loadNames,
  unmockedState,
) where

import Language.Haskell.TH
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Syntax (Lift)

import Language.Haskell.TH.TestUtils.QMode (MockedMode (..), QMode (..))

-- | State information for mocking Q functionality.
data QState (mode :: MockedMode) = QState
  { mode :: QMode mode
  , knownNames :: [(String, Name)]
  -- ^ Names that can be looked up with 'lookupTypeName' or 'lookupValueName'
  , reifyInfo :: [(Name, ReifyInfo)]
  -- ^ Reification information for Names to return when 'reify' is called.
  }
  deriving (Show, Lift)

data ReifyInfo = ReifyInfo
  { reifyInfoInfo :: Info
  , reifyInfoFixity :: Maybe Fixity
  , reifyInfoRoles :: Maybe [Role]
  , reifyInfoType :: Type
  }
  deriving (Show, Lift)

-- | A helper for loading names for 'reifyInfo'
--
--  Usage:
--
--  > QState
--  >   { reifyInfo = $(loadNames [''Int, ''Maybe])
--  >   , ...
--  >   }
loadNames :: [Name] -> ExpQ
loadNames names = listE $ flip map names $ \name -> do
  info <- reify name
  fixity <- reifyFixity name
  roles <- recover (pure Nothing) $ Just <$> reifyRoles name
  infoType <- reifyType name

  [|(name, ReifyInfo info fixity roles infoType)|]

-- | A shortcut for defining an unmocked Q.
unmockedState :: QState 'NotMocked
unmockedState =
  QState
    { mode = AllowQ
    , knownNames = []
    , reifyInfo = []
    }
