{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Arbitrary.Xmpp where

import           Control.Applicative ((<$>), (<*>))
import           Data.Char
import           Data.Maybe
import qualified Data.Text as Text
import           Network.Xmpp.Internal hiding (elements)
import           Test.QuickCheck
import           Test.QuickCheck.Instances()
import           Test.QuickCheck.Arbitrary.Generic
import qualified Text.CharRanges as Ranges
import qualified Text.StringPrep as SP
import qualified Text.StringPrep.Profiles as SP

import           Tests.Arbitrary.Common
import           Tests.Arbitrary.Xml ()


instance Arbitrary NonemptyText where
    arbitrary = Nonempty . Text.pack <$> listOf1
                  (arbitrary `suchThat` (not . isSpace))
    shrink (Nonempty txt) = map Nonempty
                            . filter (not . Text.all isSpace) $ shrink txt

instance Arbitrary Jid where
    arbitrary = do
        ~(Just jid) <- tryJid `suchThat` isJust
        return jid
      where
        tryJid = jidFromTexts <$> maybeGen (genString nodeprepProfile False)
                              <*> genString (SP.namePrepProfile False) False
                              <*> maybeGen (genString resourceprepProfile True)

        genString profile node = Text.pack . take 1024 <$> listOf1 genChar
          where
            genChar = arbitrary `suchThat` (not . isProhibited)
            prohibited = Ranges.toSet $ concat (SP.prohibited profile)
            isProhibited x = Ranges.member x prohibited
                             || if node
                                then False
                                else x `elem` ['@','/', '＠', '／']

    shrink (Jid lp dp rp) = [ Jid lp' dp  rp  | lp' <- shrinkMaybe shrink lp]
                         ++ [ Jid lp  dp' rp  | dp' <- shrink dp]
                         ++ [ Jid lp  dp  rp' | rp' <- shrinkMaybe shrink rp]


-- string :: SP.StringPrepProfile -> Gen [Char]
-- string profile = take 1024 <$> listOf1 genChar
--   where
--     genChar = arbitrary `suchThat` (not . isProhibited)
--     prohibited = Ranges.toSet $ concat (SP.prohibited profile)
--     isProhibited x = Ranges.member x prohibited
--                      || x `elem` "@/"

instance Arbitrary LangTag where
    arbitrary = LangTag <$> genTag <*> listOf genTag
        where genTag = fmap Text.pack . listOf1 . elements $ ['a'..'z'] ++ ['A'..'Z']
    shrink (LangTag lt lts) = [LangTag lt' lts | lt' <- shrinkText1 lt] ++
                              [LangTag lt lts' | lts' <- filter (not . Text.null)
                                                         <$> shrink lts]

instance Arbitrary XmppFailure where
  arbitrary = elements [StreamEndFailure, TcpConnectionFailure, XmppIllegalTcpDetails, TlsNoServerSupport, XmppNoStream, TlsStreamSecured, XmppOtherFailure]

-- Auto-derive trivial instances
instance Arbitrary StanzaErrorType where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary StanzaErrorCondition where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary StanzaError where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary StreamErrorInfo where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary IQRequestType where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary IQRequest where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary IQResult where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary IQError where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary MessageType where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Message where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary MessageError where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary PresenceType where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Presence where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary PresenceError where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Stanza where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary SaslError where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary SaslFailure where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary StreamErrorCondition where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary AuthFailure where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Version where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary ConnectionState where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary TlsBehaviour where
  arbitrary = genericArbitrary
  shrink = genericShrink
