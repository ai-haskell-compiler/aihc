{-# LANGUAGE OverloadedStrings #-}

-- | Profiles as defined by various sources
module Text.StringPrep.Profiles
  ( namePrepProfile
  , saslPrepProfile
  ) where


import qualified Data.Set as Set
import           Data.Text (Text, singleton)
import           Text.StringPrep

-- | Nameprep profile (RFC 3491)
namePrepProfile :: Bool -> StringPrepProfile
namePrepProfile allowUnassigned =
	Profile {
		maps = [b1,b2],
		shouldNormalize = True,
		prohibited = (if allowUnassigned then id else (a1:))
                               [c12,c22,c3,c4,c5,c6,c7,c8,c9],
		shouldCheckBidi = True
	}

nonAsciiSpaces :: Set.Set Char
nonAsciiSpaces = Set.fromList [ '\x00A0', '\x1680', '\x2000', '\x2001', '\x2002'
                              , '\x2003', '\x2004', '\x2005', '\x2006', '\x2007'
                              , '\x2008', '\x2009', '\x200A', '\x200B', '\x202F'
                              , '\x205F', '\x3000'
                              ]


toSpace :: Char -> Text
toSpace x = if x `Set.member` nonAsciiSpaces then " " else singleton x

-- | SASLPrep profile (RFC 4013). The parameter determines whether unassigned
-- charater are allowed (query behaviour) or disallowed (store)
saslPrepProfile :: Bool -> StringPrepProfile
saslPrepProfile allowUnassigned = Profile
    { maps = [b1, toSpace]
    , shouldNormalize = True
    , prohibited = (if allowUnassigned then id else (a1:))
                      [c12, c21, c22, c3, c4, c5, c6, c7, c8, c9]
    , shouldCheckBidi = True
    }
