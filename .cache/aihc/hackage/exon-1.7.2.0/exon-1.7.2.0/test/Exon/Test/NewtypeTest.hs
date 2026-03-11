module Exon.Test.NewtypeTest where

import Hedgehog (TestT, (===))

import Exon.Class.Exon (exonUse)
import Exon.Quote (exon, exonws, exun)
import Exon.SkipWs (skipWs)

newtype Nt =
  Nt Text
  deriving stock (Eq, Show, Generic)

newtype NtR =
  NtR { field :: Text }
  deriving stock (Eq, Show, Generic)

newtype Str =
  Str Text
  deriving stock (Eq, Show)
  deriving newtype (IsString, Semigroup, Monoid)

newtype StrGen =
  StrGen Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString)

newtype BS =
  BS ByteString
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString)

newtype StrUse =
  StrUse StrGen
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString)

instance Semigroup StrUse where
  StrUse (StrGen l) <> StrUse (StrGen r) =
    StrUse (StrGen (l <> "|" <> r))

instance Monoid StrUse where
  mempty =
    StrUse (StrGen "")

nt :: Nt
nt = Nt "nt"

ntr :: NtR
ntr = NtR "ntr"

test_newtype :: TestT IO ()
test_newtype = do
  ("pre nt Text" :: Text) === [exon|pre ##{nt} #{"Text" :: Text}|]
  ("pre ntr Text" :: Text) === [exon|pre ##{ntr} #{"Text" :: Text}|]
  ("pre nt Nt \"nt\"" :: Text) === [exon|pre ##{nt} #{show nt}|]
  ("pre nt Nt \"nt\" IsString" :: Text) === [exon|pre ##{nt} #{show nt} #{"IsString"}|]
  ("pre nt Nt \"nt\" IsString Text" :: Text) === [exon|pre ##{nt} #{show nt} #{"IsString"} #{"Text" :: Text}|]
  ("pre nt Nt \"nt\" Text Str" :: Str) === [exon|pre ##{nt} #{show nt} ##{"Text" :: Text} #{"Str" :: Str}|]
  ("pre nt Nt \"nt\" Text Str" :: StrGen) === [exon|pre ##{nt} #{show nt} ##{"Text" :: Text} #{"Str" :: StrGen}|]
  ("pre BS IsString ByteString" :: ByteString) === [exon|pre ##{"BS" :: BS} #{"IsString"} #{"ByteString" :: ByteString}|]
  ("pre BS IsString LByteString" :: Str) === [exon|pre ##{"BS" :: BS} #{"IsString"} ##{"LByteString" :: LByteString}|]
  ("pre BS IsString LByteString" :: Str) === [exun|pre #{"BS" :: BS} #{"IsString"} #{"LByteString" :: LByteString}|]
  ("pre|post" :: StrUse) === exonUse (skipWs [exonws|pre ##{"post" :: Text}|])
  ("prepost" :: StrUse) === skipWs [exonws|pre ##{"post" :: Text}|]
  ("pre post" :: Str) === [exon|pre ##{StrUse "post"}|]
  ("one #two" :: Text) === [exon|one ###{Nt "two"}|]
