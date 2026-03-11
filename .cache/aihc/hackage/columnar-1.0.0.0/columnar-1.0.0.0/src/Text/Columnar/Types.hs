{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Text.Columnar.Types
  ( Columnar(..)
  , ColumnOptions(..)
  , IsNamed(..)
  , WithColHeader(..)
  , RowNo(..)
  , headerRowNo
  , firstRowNo
  , rowNoSupply
  , defaultPositionalColumnOptions
  , defaultColumnOptions
  -- IsRecord
  , IsRecord(..)
  , IsMonoidalRecord(..)
  , FieldMethods(..)
  , MonoidalFieldMethods(..)
  , Records(..)
  -- isRecordIntMap and friends
  , IsRecordIntMap(..)
  , encodeRecordMap
  , decodeRecordMap
  , memptyRecordMap
  , mappendRecordMap
  , summarizeMap
  -- IsRecordHashMap and friends
  , IsRecordHashMap(..)
  , encodeRecordHashMap
  , decodeRecordHashMap
  , memptyRecordHashMap
  , mappendRecordHashMap
  , summarizeHashMap
  -- Rows, Row
  , Row(..)
  , Rows(..)
  , mkRows
  , mkRowsWith
  , getRows
  , mkRow
  -- ColumnarIx
  , ColumnarIx(..)
  , listColumns
  -- construction toolkit: mkFM, mkMFM, etc
  , mkFieldMethods
  , mkMonoidalFieldMethods
  , convertMonoidalFieldMethodsToFieldMethods
  , constructRecordMappend
  -- encoding with Haskell
  , haskellRecords
  -- mkRecords, mkRecordsWith
  , mkRecords
  , mkRecordsWith
  -- en/decoding Records
  , buildRecord
  , encodeRecordsT
  , encodeRecords
  , decodeRecordsT
  , decodeRecords
  , parseRecord
  , decodeRecordsWithT
  , decodeRecordsWith
  -- converting between Records and Rows
  , recordsToRows
  , recordToRow
  , rowsToRecords
  , rowToRecord
  -- en/decoding Columnar
  , encodeColumnarCsv
  , csvHeader
  , decodeColumnarCsv
  , decodeColumnarCsvWith
  -- misc tools
  , recordFields
  , listRecords
  -- utilities
  , mkNamedRecord
  , mkIntMap
  -- for convenience
  , module Text.Enum.Text
  ) where

import qualified Control.Lens                   as L
import qualified Data.ByteString.Char8          as B
import qualified Data.ByteString.Lazy.Char8     as LBS
import           Data.Coerce
import qualified Data.Csv                       as CSV
import qualified Data.HashMap.Strict            as HM
import qualified Data.IntMap.Strict             as IM
import qualified Data.Map                       as Map
import           Data.Maybe
import           Data.Possibly
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE
import qualified Data.Text.Encoding.Error       as TE
import qualified Data.Vector                    as V
import           Fmt
import           Text.Enum.Text
import           Text.Show.Functions()


type TextParser a = T.Text -> Possibly a


-- A toolkit for encoding and decoding CSV Records, and writing out
-- plain-text tables. It builds up the abstraction provided by cassava
-- and adds text decoders. Every type in this framework can automatically
-- work with named fields (a CSV with a header) or unlabeled fields
-- (no header). To generate tables that line up the function assigning
-- a formatter to each column must be supplied (otherwise default formatter
-- that places a space to the right of each column will be used).

-- While cassava is ByteString oriented, this API is somewhat more Text
-- text oriented, with each field being Buildable and TextParsable.

-- Support for auto-building Monoidal Records from Monoidal fields is also
-- supported.

-- The quickest way to see how this qworks is to look at it in actions,
-- for example, "Nike.Server.Types.JobTransition".


-------------------------------------------------------------------------------
-- Columnar
-------------------------------------------------------------------------------

-- | a columnar type enumerates the columns of a CSV/table; it is just
-- an EnumText with options for CSV codecs and the table generators
class EnumText c => Columnar c where
  columnOptions :: ColumnOptions c
  columnOptions = defaultColumnOptions

data ColumnOptions c =
  ColumnOptions
    { _co_csv_nmd :: IsNamed                          -- ^ encoding CSV with headers?
    , _co_csv_eos :: CSV.EncodeOptions                -- ^ Cassava encode options
    , _co_csv_dos :: CSV.DecodeOptions                -- ^ Cassava decode options
    , _co_col_hdr :: WithColHeader                    -- ^ teabular: include a header line
    , _co_col_fmt :: RowNo -> c -> Builder -> Builder -- ^ teabular: field formatters
    }
  deriving (Show)

-- | are we generating/expecting a header for CSVs
data IsNamed
  = Named                     -- ^ we are generating and expecting headers
  | Positional CSV.HasHeader  -- ^ we are not generating headers but possibly
                              -- skipping them on read

-- | are we generating/expecting a header for tabular data
data WithColHeader
  = WithColHeader
  | WithoutColHeader
  deriving (Eq,Ord,Show)

-- | which Row is being formatted
newtype RowNo =
  RowNo
    { _RowNo :: Int
    }
  deriving (Enum,Eq,Num,Ord,Show)

-- | the header is row 0, first data row is 1
headerRowNo, firstRowNo :: RowNo
headerRowNo = 0
firstRowNo  = 1

-- | the list of valid RowNo, starting with the first row (NOT the header)
rowNoSupply :: [RowNo]
rowNoSupply = [1..]

instance Show IsNamed where
  show Named                      = "Named"
  show (Positional CSV.HasHeader) = "(Positional HasHeader)"
  show (Positional CSV.NoHeader ) = "(Positional NoHeader)"

defaultPositionalColumnOptions :: ColumnOptions c
defaultPositionalColumnOptions =
    defaultColumnOptions
      { _co_csv_nmd = Positional CSV.NoHeader
      }

defaultColumnOptions :: ColumnOptions c
defaultColumnOptions =
  ColumnOptions
    { _co_csv_nmd = Named
    , _co_csv_eos = CSV.defaultEncodeOptions
    , _co_csv_dos = CSV.defaultDecodeOptions
    , _co_col_hdr = WithColHeader
    , _co_col_fmt = const $ const id
    }


-------------------------------------------------------------------------------
-- IsRecordIntMap
-------------------------------------------------------------------------------

-- often Columnar records will be organized as an IntMap of records
class ( IsRecord r c
      , Coercible m (IM.IntMap r)
      )  => IsRecordIntMap r c m | r->c, c->r, r->m, m->r where
  recordKey :: r -> Int

-- | decoding into an Intmap
decodeRecordMap :: forall r c m . IsRecordIntMap r c m => LBS.ByteString -> Possibly m
decodeRecordMap = fmap mk . dec
  where
    dec :: LBS.ByteString -> Possibly (Records r c)
    dec = decodeRecords

    mk  =
      coerce
        . IM.fromList
        . mkAssocList recordKey
        . V.toList
        . _recs_vector

-- | encoding to an Intmap
encodeRecordMap :: forall r c m . IsRecordIntMap r c m => m -> LBS.ByteString
encodeRecordMap = enc . mkRecords . IM.elems . coerce
  where
    enc :: Records r c -> LBS.ByteString
    enc = encodeRecords

-- | often we will want to construct a Monoid Intmap from Monoid records
memptyRecordMap :: forall r c m . (Monoid r, IsRecordIntMap r c m) => m
memptyRecordMap = coerce (IM.empty :: IM.IntMap r)

-- | often we will want to construct a Monoid Intmap from Monoid records
mappendRecordMap :: forall r c m . (Monoid r, IsRecordIntMap r c m) => m -> m -> m
mappendRecordMap x y =
    coerce $ (IM.unionWith (<>) (coerce x) (coerce y) :: IM.IntMap r)

-- | provide evidence that Map is well formed
summarizeMap :: forall r c m . IsRecordIntMap r c m => m -> T.Text
summarizeMap m = fmt $ "IntMap with "+|IM.size im|+" records"
  where
    im :: IM.IntMap r
    im = coerce m


-------------------------------------------------------------------------------
-- IsRecordHashMap
-------------------------------------------------------------------------------

-- sometimes Columnar records will be organized as an HashMap of records
class ( IsRecord r c
      , Coercible m (HM.HashMap T.Text r)
      )  => IsRecordHashMap r c m | r->c, c->r, r->m, m->r where
  recordTextKey :: r -> T.Text

-- | decoding into an Intmap
decodeRecordHashMap :: forall r c m . IsRecordHashMap r c m
                    => LBS.ByteString
                    -> Possibly m
decodeRecordHashMap = fmap mk . dec
  where
    dec :: LBS.ByteString -> Possibly (Records r c)
    dec = decodeRecords

    mk  =
      coerce
        . HM.fromList
        . mkAssocList recordTextKey
        . V.toList
        . _recs_vector

-- | encoding to an Intmap
encodeRecordHashMap :: forall r c m . IsRecordHashMap r c m
                    => m
                    -> LBS.ByteString
encodeRecordHashMap = enc . mkRecords . HM.elems . coerce
  where
    enc :: Records r c -> LBS.ByteString
    enc = encodeRecords

-- | often we will want to construct a Monoid HashMap from Monoid records
memptyRecordHashMap :: forall r c m . (Monoid r, IsRecordHashMap r c m) => m
memptyRecordHashMap = coerce (HM.empty :: HM.HashMap T.Text r)

-- | often we will want to construct a Monoid HashMap from Monoid records
mappendRecordHashMap :: forall r c m . (Monoid r, IsRecordHashMap r c m)
                     => m
                     -> m
                     -> m
mappendRecordHashMap x y =
    coerce $ (HM.unionWith (<>) (coerce x) (coerce y) :: HM.HashMap T.Text r)

-- | provide evidence that HashMap is well formed
summarizeHashMap :: forall r c m . IsRecordHashMap r c m => m -> T.Text
summarizeHashMap m = fmt $ "HashMap with "+|HM.size hm|+" records"
  where
    hm :: HM.HashMap T.Text r
    hm = coerce m


-------------------------------------------------------------------------------
-- IsRecord
-------------------------------------------------------------------------------

-- | IsRecord combines the column type with the record type, each record
-- type determining the column type and vice versa
class Columnar c => IsRecord r c | r->c, c->r where
  zeroRecord   :: r
  fieldMethods :: c -> FieldMethods r c

-- | IsRecord combines the column type with the record type, each record
-- type determining the column type and vice versa
class (Monoid r,IsRecord r c) => IsMonoidalRecord r c where
  monoidalFieldMethods :: c -> MonoidalFieldMethods r c

-- | for each column we need a lens the field of the record, which must be
-- Buildable and TextParsable
data FieldMethods r c = forall f . (Buildable f,TextParsable f) =>
  FieldMethods
    { _fm_lens :: L.Lens' r f
    }

-- | if we need monoidal records then the fields must be Monoidal too.
data MonoidalFieldMethods r c =
                  forall f . (Buildable f,TextParsable f,Monoid f) =>
  MonoidalFieldMethods
    { _mfm_lens :: L.Lens' r f
    }

-- | a record set contains the ColumnOptions along with the vector of
-- records
data Records r c =
  Records
    { _recs_options :: ColumnOptions c
    , _recs_vector  :: V.Vector r
    }
  deriving (Show)

instance (IsRecord r c,Buildable (Rows c)) => Buildable (Records r c) where
  build = build . recordsToRows


-------------------------------------------------------------------------------
-- Rows, Row
-------------------------------------------------------------------------------

-- | Rows are used for generating tabular output and do not need access
-- to any record type
data Rows c =
    Rows
      { _rows_options :: ColumnOptions c
      , _rows_vector  :: V.Vector (Row c)
      }
  deriving (Show)

newtype Row c = Row { _Row :: c->Builder }
  deriving (Show)

mkRows :: Columnar c => [Row c] -> Rows c
mkRows = mkRowsWith columnOptions

mkRowsWith :: ColumnOptions c -> [Row c] -> Rows c
mkRowsWith opts rs =
    Rows
      { _rows_options = opts
      , _rows_vector  = V.fromList rs
      }

getRows :: Rows c -> [Row c]
getRows = V.toList . _rows_vector

mkRow :: (c -> Builder) -> Row c
mkRow = coerce

instance Columnar c => Buildable (Rows c) where
  build = build_columnar

instance Columnar c => Eq (Row c) where
  (==) (Row f) (Row g) = test_row_equality f g

instance Columnar c => CSV.ToField (CSV.Only c) where
  toField = toFieldEnumText . CSV.fromOnly

instance Columnar c => CSV.FromField (CSV.Only c) where
  parseField = fmap CSV.Only . fromFieldEnumText

instance Columnar c => CSV.ToRecord (Row c) where
  toRecord row = V.fromList $ map mk $ lst row
    where
      mk  c = TE.encodeUtf8 $ fmt $ coerce row c

      lst _ = [minBound..maxBound :: c]

instance Columnar c => CSV.FromRecord (Row c) where
  parseRecord = parse_record

instance Columnar c => CSV.ToNamedRecord (Row c) where
  toNamedRecord row = HM.fromList $ map mk $ listColumns $ mk_ix row
    where
      mk c =
        ( CSV.toField $ CSV.Only c
        , TE.encodeUtf8 $ fmt $ coerce row c
        )

      mk_ix :: Row c -> ColumnarIx c
      mk_ix = const ColumnarIx

instance Columnar c => CSV.FromNamedRecord (Row c) where
  parseNamedRecord = parse_named_record


-------------------------------------------------------------------------------
-- ColumnarIx
-------------------------------------------------------------------------------

-- | sometimes we just need the column type, as with listColumns
data ColumnarIx c = ColumnarIx
  deriving (Show)

listColumns :: Columnar c => ColumnarIx c -> [c]
listColumns = const [minBound..maxBound]


-------------------------------------------------------------------------------
-- Encoding as Haskell
-------------------------------------------------------------------------------

-- | encode the list of records as a Haskell list of strings,
-- one line per line, indented at two spaces
haskellRecords :: IsRecord r c
               => [r]       -- ^ list of records to encode
               -> Builder   -- ^ Haskell defining LBS for decoding
haskellRecords rs = mconcat $
    [ "  [ "+|show hdr|+"\n"
    ] ++
    [ "  , "+|show rcd|+"\n"
      | rcd <- bdy
      ] ++
    [ "  ]"
    ]
  where
    (hdr,bdy) = case T.lines $ encodeRecordsT $ mkRecords rs of
      []  -> error "haskellRecords: empty encoding"
      h:b -> (h,b)


-------------------------------------------------------------------------------
-- construction toolkit: mkFM, mkMFM, etc
-------------------------------------------------------------------------------

-- | for constructing each field's 'FieldMethods'
mkFieldMethods :: (TextParsable f,Buildable f) => L.Lens' r f -> FieldMethods r c
mkFieldMethods = FieldMethods

-- | for constructing each field's 'MonoidalFieldMethods'
mkMonoidalFieldMethods :: (TextParsable f,Buildable f,Monoid f)
                       => L.Lens' r f
                       -> MonoidalFieldMethods r c
mkMonoidalFieldMethods = MonoidalFieldMethods

convertMonoidalFieldMethodsToFieldMethods
    :: MonoidalFieldMethods r c -> FieldMethods r c
convertMonoidalFieldMethodsToFieldMethods
    MonoidalFieldMethods{..} = FieldMethods _mfm_lens

constructRecordMappend :: (Bounded c,Enum c)
                       => (c->MonoidalFieldMethods r c)
                       -> r -> r -> r
constructRecordMappend mfm x = foldr (.) id $ map ma_f [minBound..maxBound]
  where
    ma_f lb = case mfm lb of
      MonoidalFieldMethods{..} ->
        L.over _mfm_lens (L.view _mfm_lens x<>)


-------------------------------------------------------------------------------
-- mkRecords, mkRecordsWith
-------------------------------------------------------------------------------

mkRecords :: Columnar c => [r] -> Records r c
mkRecords = mkRecordsWith columnOptions

mkRecordsWith :: ColumnOptions c -> [r] -> Records r c
mkRecordsWith co = Records co . V.fromList


-------------------------------------------------------------------------------
-- en/decoding Records
-------------------------------------------------------------------------------

buildRecord :: forall r c . IsRecord r c => r -> Builder
buildRecord r = build $ encodeRecordsT $ mkRecordsWith co [r]
  where
    co = defaultPositionalColumnOptions

encodeRecordsT :: forall r c . IsRecord r c => Records r c -> T.Text
encodeRecordsT = TE.decodeUtf8With TE.lenientDecode . LBS.toStrict . encodeRecords

encodeRecords :: forall r c . IsRecord r c => Records r c -> LBS.ByteString
encodeRecords = encodeColumnarCsv . recordsToRows

decodeRecordsT :: IsRecord r c => T.Text -> Possibly (Records r c)
decodeRecordsT = decodeRecordsWithT columnOptions

decodeRecords :: IsRecord r c => LBS.ByteString -> Possibly (Records r c)
decodeRecords = decodeRecordsWith columnOptions

parseRecord :: IsRecord r c => TextParser r
parseRecord txt = decodeRecordsWithT co txt >>= extr
  where
    extr Records{..} = case V.toList _recs_vector of
      [r] -> Right r
      _   -> Left "parseRecord: expecting a single record"

    co = defaultPositionalColumnOptions

decodeRecordsWithT :: IsRecord r c
                   => ColumnOptions c
                   -> T.Text
                   -> Possibly (Records r c)
decodeRecordsWithT co = decodeRecordsWith co . LBS.fromStrict . TE.encodeUtf8

decodeRecordsWith :: IsRecord r c
                  => ColumnOptions c
                  -> LBS.ByteString
                  -> Possibly (Records r c)
decodeRecordsWith co lbs = decodeColumnarCsvWith co lbs >>= rowsToRecords


-------------------------------------------------------------------------------
-- converting between Records and Rows
-------------------------------------------------------------------------------

recordsToRows :: IsRecord r c => Records r c -> Rows c
recordsToRows Records{..} =
  Rows
    { _rows_options = _recs_options
    , _rows_vector  = V.map recordToRow _recs_vector
    }

recordToRow :: IsRecord r c => r -> Row c
recordToRow r = Row $ \c -> case fieldMethods c of
    FieldMethods{..} -> build $ L.view _fm_lens r

rowsToRecords :: forall r c . IsRecord r c => Rows c -> Possibly (Records r c)
rowsToRecords Rows{..} = do
    recs <- mapM rowToRecord $ V.toList _rows_vector
    return
      Records
        { _recs_options = _rows_options
        , _recs_vector  = V.fromList recs
        }

rowToRecord :: forall r c . IsRecord r c => Row c -> Possibly r
rowToRecord row = foldr op (Right zeroRecord) $ listColumns ix
  where
    op c pr = pr >>= prs (fmt $ _Row row c) c

    ix :: ColumnarIx c
    ix = ColumnarIx

    prs :: T.Text -> c -> r -> Possibly r
    prs txt c r = case fieldMethods c of
      FieldMethods{..} -> do
        f <- parseText txt
        return $ L.set _fm_lens f r


-------------------------------------------------------------------------------
-- en/decoding Columnar with Rows
-------------------------------------------------------------------------------

encodeColumnarCsv :: forall c . Columnar c
                  => Rows c
                  -> LBS.ByteString
encodeColumnarCsv Rows{..} =
    enc $ V.toList _rows_vector
  where
    enc :: [Row c] -> LBS.ByteString
    enc = case _co_csv_nmd _rows_options of
      Named        -> CSV.encodeByNameWith eos $ csvHeader ix
      Positional _ -> CSV.encodeWith       eos

    ix :: ColumnarIx c
    ix = ColumnarIx

    eos = _co_csv_eos _rows_options

csvHeader :: Columnar c => ColumnarIx c -> CSV.Header
csvHeader ci = V.fromList
  [ TE.encodeUtf8 $ fmt $ build c
    | c <- listColumns ci
    ]

decodeColumnarCsv :: Columnar c
                  => LBS.ByteString
                  -> Possibly (Rows c)
decodeColumnarCsv = decodeColumnarCsvWith columnOptions

decodeColumnarCsvWith :: Columnar c
                      => ColumnOptions c
                      -> LBS.ByteString
                      -> Possibly (Rows c)
decodeColumnarCsvWith opts@ColumnOptions{..} = fmap mk . dec
  where
    dec = case _co_csv_nmd of
      Named         -> fmap snd . CSV.decodeByNameWith _co_csv_dos
      Positional hh ->            CSV.decodeWith       _co_csv_dos hh

    mk vec =
      Rows
        { _rows_options = opts
        , _rows_vector  = vec
        }


-------------------------------------------------------------------------------
-- misc tools
-------------------------------------------------------------------------------

recordFields :: forall r c . IsRecord r c => [(B.ByteString,r->B.ByteString)]
recordFields =
    [ (build_bs c, extr c)
      | c<-[minBound..maxBound :: c]
      ]
  where
    extr :: IsRecord r c => c -> r -> B.ByteString
    extr c r = case fieldMethods c of
      FieldMethods{..} -> build_bs $ L.view _fm_lens r

    build_bs :: Buildable a => a -> B.ByteString
    build_bs = TE.encodeUtf8 . fmt . build

listRecords :: Columnar c => Records r c -> [r]
listRecords = V.toList . _recs_vector


-------------------------------------------------------------------------------
-- utilities
-------------------------------------------------------------------------------

mkNamedRecord :: [(B.ByteString,a->B.ByteString)]
              -> a
              -> CSV.NamedRecord
mkNamedRecord al r = HM.fromList [ (lbl,prj r) | (lbl,prj) <- al ]

mkIntMap :: (a->Int) -> [a] -> IM.IntMap a
mkIntMap prj rs = IM.fromList [ (prj r,r) | r <- rs ]


-------------------------------------------------------------------------------
-- internal
-------------------------------------------------------------------------------

build_columnar :: forall c . Columnar c => Rows c -> Builder
build_columnar Rows{..} = mconcat
    [ fmt_row rn row <> "\n"
      | (rn,row) <-
            [ (headerRowNo,hdr) | _co_col_hdr==WithColHeader ] ++
            zip rowNoSupply rows
      ]
  where
    fmt_row :: RowNo -> Row c -> Builder
    fmt_row rn row = mconcat $ map gen $ listColumns ix
      where
        gen c = _co_col_fmt rn c $ coerce row c

    hdr = coerce (buildEnumText :: c -> Builder)

    ix :: ColumnarIx c
    ix = ColumnarIx

    rows :: [Row c]
    rows = V.toList _rows_vector

    ColumnOptions{..} = _rows_options

parse_record :: forall c . Columnar c
             => CSV.Record
             -> CSV.Parser (Row c)
parse_record v = case len == v_len of
      True  -> return $ Row lu
      False -> fail $ unwords
        [ "Columnar.parseRecord: expected"
        , show len
        , "columns but found"
        , show v_len
        , "in the input"
        ]
    where
      lu :: c -> Builder
      lu c = build $ TE.decodeUtf8 $ v V.! (fromEnum c - fromEnum mnb)

      v_len = V.length v
      len   = fromEnum mxb + 1 - fromEnum mnb

      mnb, mxb :: c
      mnb = minBound
      mxb = maxBound

parse_named_record :: forall c . Columnar c
                   => CSV.NamedRecord
                   -> CSV.Parser (Row c)
parse_named_record hm = do
    mp <- fmap Map.fromList $ mapM prs $ listColumns ix
    return $ Row $ \c -> fromMaybe oops $ Map.lookup c mp
  where
    prs :: c -> CSV.Parser (c,Builder)
    prs c = case HM.lookup bs hm of
        Nothing  -> fail $ "CSV record missing column: "++B.unpack bs
        Just val -> return (c,build $ TE.decodeUtf8 val)
      where
        bs = TE.encodeUtf8 $ renderEnumText c

    ix :: ColumnarIx c
    ix = ColumnarIx

    oops = error "parse_named_record: the impossible happened!"

test_row_equality :: forall c . Columnar c
                  => (c->Builder)
                  -> (c->Builder)
                  -> Bool
test_row_equality f g = all chk $ listColumns ix
  where
    chk c = f c == g c

    ix :: ColumnarIx c
    ix = ColumnarIx

mkAssocList :: (a->k) -> [a] -> [(k,a)]
mkAssocList prj rs = [ (prj r,r) | r<-rs ]

fromFieldEnumText :: EnumText c => B.ByteString -> CSV.Parser c
fromFieldEnumText = either fail return . parseEnumText . TE.decodeUtf8
