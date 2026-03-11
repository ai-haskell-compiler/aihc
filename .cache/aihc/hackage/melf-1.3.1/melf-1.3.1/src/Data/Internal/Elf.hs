{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Internal.Elf where

import Control.Exception.ChainedException
import Data.Elf.Constants
import Data.Elf.Headers hiding (Header)
import qualified Data.Elf.Headers as H
import Data.Interval as I

import Control.Lens.Combinators hiding (contains)
import Control.Lens.Operators
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State as MS
import Data.Binary
import Data.Bits as Bin
import Data.ByteString.Lazy.Char8 as BSL8
import Data.ByteString.Lazy as BSL
import Data.Foldable
import Data.Int
import qualified Data.List as L
import Data.Maybe
import Data.Monoid

-- | @RBuilder@ is an intermediate internal data type that is used by parser.
-- It contains information about layout of the ELF file that can be used
-- by `Data.Elf.PrettyPrint.printLayout`
data RBuilder c
    = RBuilderHeader
        { rbhHeader :: HeaderXX c
        }
    | RBuilderSectionTable
        { rbstHeader :: HeaderXX c
        }
    | RBuilderSegmentTable
        { rbptHeader :: HeaderXX c
        }
    | RBuilderSection
        { rbsHeader :: SectionXX c
        , rbsN      :: ElfSectionIndex
        , rbsName   :: String
        }
    | RBuilderSegment
        { rbpHeader :: SegmentXX c
        , rbpN      :: Word16
        , rbpData   :: [RBuilder c]
        }
    | RBuilderRawData
        { rbrdInterval :: Interval (WordXX c)
        }
    | RBuilderRawAlign
        { rbraOffset :: WordXX c
        , rbraAlign  :: WordXX c
        }

data LZip a = LZip [a] (Maybe a) [a]

instance Foldable LZip where
    foldMap f (LZip l  (Just c) r) = foldMap f $ LZip l Nothing (c : r)
    foldMap f (LZip l  Nothing  r) = foldMap f $ L.reverse l ++ r

-- | t`Elf` is a forrest of trees of type `ElfXX`.
-- Trees are composed of `ElfXX` nodes, `ElfSegment` can contain subtrees
data ElfNodeType = Header | SectionTable | SegmentTable | Section | Segment | RawData | RawAlign

-- | List of ELF nodes.
data ElfListXX c where
    ElfListCons :: ElfXX t c -> ElfListXX c -> ElfListXX c
    ElfListNull :: ElfListXX c

-- | Elf is a sigma type where the first entry defines the type of the second one
data Elf = forall a . Elf (SingElfClass a) (ElfListXX a)

-- | Section data may contain a string table.
-- If a section contains a string table with section names, the data
-- for such a section is generated and `esData` should contain `ElfSectionDataStringTable`
data ElfSectionData c
    = ElfSectionData                -- ^ Regular section data
        { esdData :: BSL.ByteString -- ^ The content of the section
        }
    | ElfSectionDataStringTable     -- ^ Section data will be generated from section names
    | ElfSectionDataNoBits          -- ^ SHT_NOBITS uninitialized section data: section has size but no content
        { esdSize :: WordXX c       -- ^ Size of the section
        }

-- | The type of node that defines Elf structure.
data ElfXX t c where
    ElfHeader ::
        { ehData       :: ElfData    -- ^ Data encoding (big- or little-endian)
        , ehOSABI      :: ElfOSABI   -- ^ OS/ABI identification
        , ehABIVersion :: Word8      -- ^ ABI version
        , ehType       :: ElfType    -- ^ Object file type
        , ehMachine    :: ElfMachine -- ^ Machine type
        , ehEntry      :: WordXX c   -- ^ Entry point address
        , ehFlags      :: Word32     -- ^ Processor-specific flags
        } -> ElfXX 'Header c
    ElfSectionTable :: ElfXX 'SectionTable c
    ElfSegmentTable :: ElfXX 'SegmentTable c
    ElfSection ::
        { esName      :: String         -- ^ Section name (NB: string, not offset in the string table)
        , esType      :: ElfSectionType -- ^ Section type
        , esFlags     :: ElfSectionFlag -- ^ Section attributes
        , esAddr      :: WordXX c       -- ^ Virtual address in memory
        , esAddrAlign :: WordXX c       -- ^ Address alignment boundary
        , esEntSize   :: WordXX c       -- ^ Size of entries, if section has table
        , esN         :: ElfSectionIndex -- ^ Section number
        , esInfo      :: Word32         -- ^ Miscellaneous information
        , esLink      :: Word32         -- ^ Link to other section
        , esData      :: ElfSectionData c -- ^ The content of the section
        } -> ElfXX 'Section c
    ElfSegment ::
        { epType       :: ElfSegmentType -- ^ Type of segment
        , epFlags      :: ElfSegmentFlag -- ^ Segment attributes
        , epVirtAddr   :: WordXX c       -- ^ Virtual address in memory
        , epPhysAddr   :: WordXX c       -- ^ Physical address
        , epAddMemSize :: WordXX c       -- ^ Add this amount of memory after the section when the section is loaded to memory by execution system.
                                         --   Or, in other words this is how much `pMemSize` is bigger than `pFileSize`
        , epAlign      :: WordXX c       -- ^ Alignment of segment
        , epData       :: ElfListXX c    -- ^ Content of the segment
        } -> ElfXX 'Segment c
    -- | Some ELF files (some executables) don't bother to define
    -- sections for linking and have just raw data in segments.
    ElfRawData ::
        { edData :: BSL.ByteString -- ^ Raw data in ELF file
        } -> ElfXX 'RawData c
    -- | Align the next data in the ELF file.
    -- The offset of the next data in the ELF file
    -- will be the minimal @x@ such that
    -- @x mod eaAlign == eaOffset mod eaAlign @
    ElfRawAlign ::
        { eaOffset :: WordXX c -- ^ Align value
        , eaAlign  :: WordXX c -- ^ Align module
        } -> ElfXX 'RawAlign c

data WBuilderData
    = WBuilderDataHeader
    | WBuilderDataByteStream { wbdData :: BSL.ByteString }
    | WBuilderDataSectionTable
    | WBuilderDataSegmentTable

data WBuilderState a =
    WBuilderState
        { _wbsSections         :: [(ElfSectionIndex, SectionXX a)]
        , _wbsSegmentsReversed :: [SegmentXX a]
        , _wbsDataReversed     :: [WBuilderData]
        , _wbsOffset           :: WordXX a
        , _wbsPhOff            :: WordXX a
        , _wbsShOff            :: WordXX a
        , _wbsShStrNdx         :: ElfSectionIndex
        , _wbsNameIndexes      :: [Int64]
        }

makeLenses ''WBuilderState

infixr 9 ~:

-- | Helper for `ElfListCons`
(~:) :: ElfXX t a -> ElfListXX a -> ElfListXX a
(~:) = ElfListCons

foldMapElfList :: Monoid m => (forall t' . (ElfXX t' a -> m)) -> ElfListXX a -> m
foldMapElfList f (ElfListCons v@(ElfSegment { .. }) l) = f v <> foldMapElfList f epData <> foldMapElfList f l
foldMapElfList f (ElfListCons v l)                     = f v <> foldMapElfList f l
foldMapElfList _  ElfListNull                          = mempty

foldMapElfList' :: Monoid m => (forall t' . (ElfXX t' a -> m)) -> ElfListXX a -> m
foldMapElfList' f (ElfListCons v l) = f v <> foldMapElfList' f l
foldMapElfList' _  ElfListNull      = mempty

mapMElfList :: Monad m => (forall t' . (ElfXX t' a -> m b)) -> ElfListXX a -> m [b]
mapMElfList f l = sequence $ foldMapElfList' ((: []) . f) l

headerInterval :: forall a . SingElfClassI a => HeaderXX a -> Interval (WordXX a)
headerInterval _ = I 0 $ headerSize $ fromSingElfClass $ singElfClass @a

sectionTableInterval :: SingElfClassI a => HeaderXX a -> Interval (WordXX a)
sectionTableInterval HeaderXX{..} = I hShOff $ fromIntegral $ hShEntSize * hShNum

segmentTableInterval :: SingElfClassI a => HeaderXX a -> Interval (WordXX a)
segmentTableInterval HeaderXX{..} = I hPhOff $ fromIntegral $ hPhEntSize * hPhNum

sectionInterval :: SingElfClassI a => SectionXX a -> Interval (WordXX a)
sectionInterval SectionXX{..} = I sOffset if sType == SHT_NOBITS then 0 else sSize

segmentInterval :: SingElfClassI a => SegmentXX a -> Interval (WordXX a)
segmentInterval SegmentXX{..} = I pOffset pFileSize

rBuilderInterval :: SingElfClassI a => RBuilder a -> Interval (WordXX a)
rBuilderInterval RBuilderHeader{..}       = headerInterval rbhHeader
rBuilderInterval RBuilderSectionTable{..} = sectionTableInterval rbstHeader
rBuilderInterval RBuilderSegmentTable{..} = segmentTableInterval rbptHeader
rBuilderInterval RBuilderSection{..}      = sectionInterval rbsHeader
rBuilderInterval RBuilderSegment{..}      = segmentInterval rbpHeader
rBuilderInterval RBuilderRawData{..}      = rbrdInterval
rBuilderInterval RBuilderRawAlign{}       = error "Internal error: rBuilderInterval is not defined for RBuilderRawAlign"

findInterval :: (Ord t, Num t) => (a -> Interval t) -> t -> [a] -> LZip a
findInterval f e = findInterval' []
    where
        findInterval' l []                           = LZip l Nothing []
        findInterval' l (x : xs) | e `touches`  f x  = LZip l (Just x) xs
                                 | e < offset  (f x) = LZip l Nothing (x : xs)
                                 | otherwise         = findInterval' (x : l) xs
        touches a i | I.empty i = a == offset i
                    | otherwise = a `member` i

showRBuilder' :: RBuilder a -> String
showRBuilder' RBuilderHeader{}       = "header"
showRBuilder' RBuilderSectionTable{} = "section table"
showRBuilder' RBuilderSegmentTable{} = "segment table"
showRBuilder' RBuilderSection{..}    = "section " ++ show rbsN
showRBuilder' RBuilderSegment{..}    = "segment " ++ show rbpN
showRBuilder' RBuilderRawData{}      = "raw data" -- should not be called
showRBuilder' RBuilderRawAlign{}     = "alignment" -- should not be called

showRBuilder :: SingElfClassI a => RBuilder a -> String
showRBuilder v = showRBuilder' v ++ " (" ++ show (rBuilderInterval v) ++ ")"

-- showERBList :: SingElfClassI a => [RBuilder a] -> String
-- showERBList l = "[" ++ (L.concat $ L.intersperse ", " $ fmap showRBuilder l) ++ "]"

intersectMessage :: SingElfClassI a => RBuilder a -> RBuilder a -> String
intersectMessage x y = showRBuilder x ++ " and " ++ showRBuilder y ++ " intersect"

addRBuilders :: forall a m . (SingElfClassI a, MonadCatch m) => [RBuilder a] -> m [RBuilder a]
addRBuilders newts =
    let
        addRBuilders' f newts' l = foldM (flip f) l newts'

        addRBuilderEmpty :: (SingElfClassI a, MonadCatch m) => RBuilder a -> [RBuilder a] -> m [RBuilder a]
        addRBuilderEmpty t ts =
            -- (unsafePerformIO $ Prelude.putStrLn $ "Add Empty " ++ showRBuilder t ++ " to " ++ showERBList ts) `seq`
            let
                to' = offset $ rBuilderInterval t
                (LZip l c' r) = findInterval rBuilderInterval to' ts

                -- Let `(le, lo)` is the result of `allEmptyStarting a l`.
                -- Then `le` is the initial sublist of `l` each element of which is empty and starts at `a`,
                -- `lo` is the rest of `l`.
                allEmptyStartingAt :: WordXX a -> [RBuilder a] -> ([RBuilder a], [RBuilder a])
                allEmptyStartingAt a ls = f ([], ls)
                    where
                        f (le, []) = (L.reverse le, [])
                        f (le, h : lo) =
                            let
                                hi = rBuilderInterval h
                            in if not (I.empty hi) || (offset hi /= a)
                                then (L.reverse le, h : lo)
                                else f (h : le, lo)
            in case c' of
                Just RBuilderSegment{..} -> do
                    d <- $addContext' $ addRBuilderEmpty t rbpData
                    return $ toList $ LZip l (Just RBuilderSegment{ rbpData = d, .. }) r
                Just c ->
                    if offset (rBuilderInterval c) /= to' then
                        $chainedError $ intersectMessage t c
                    else
                        let
                            (ce, re') = allEmptyStartingAt to' (c : r)
                        in case t of
                            RBuilderSegment{..} ->
                                return $ toList $ LZip l (Just RBuilderSegment{ rbpData = ce, .. }) re'
                            _ ->
                                return $ toList $ LZip l Nothing (ce ++ (t : re'))
                Nothing -> return $ toList $ LZip l (Just t) r

        addRBuilderNonEmpty :: (SingElfClassI a, MonadCatch m) => RBuilder a -> [RBuilder a] -> m [RBuilder a]
        addRBuilderNonEmpty t ts =
            -- (unsafePerformIO $ Prelude.putStrLn $ "Add NonEmpty " ++ showRBuilder t ++ " to " ++ showERBList ts) `seq`
            let
                ti = rBuilderInterval t
                (LZip l c' r) = findInterval rBuilderInterval (offset ti) ts

                addRBuildersNonEmpty :: (SingElfClassI a, MonadCatch m) => [RBuilder a] -> RBuilder a -> m (RBuilder a)
                addRBuildersNonEmpty [] x = return x
                addRBuildersNonEmpty ts' RBuilderSegment{..} = do
                    d <- $addContext' $ addRBuilders' addRBuilderNonEmpty ts' rbpData
                    return RBuilderSegment{ rbpData = d, .. }
                addRBuildersNonEmpty (x:_) y = $chainedError $ intersectMessage x y

            in case c' of

                Just c ->

                    if ti == rBuilderInterval c then

                        case t of

                                -- NB: If a segment A has number greater than segment B and they have same size, then
                                --     segment A contains segment B
                                --     This should be taken into account in the serialization code.
                                RBuilderSegment{..} ->

                                    return $ toList $ LZip l (Just RBuilderSegment{ rbpData = [c], .. }) r

                                _ ->  do

                                    c'' <- $addContext' $ addRBuildersNonEmpty [t] c
                                    return $ toList $ LZip l (Just c'') r

                    else if rBuilderInterval c `contains` ti then do

                        c'' <- $addContext' $ addRBuildersNonEmpty [t] c
                        return $ toList $ LZip l (Just c'') r

                    else if ti `contains` rBuilderInterval c then

                        let

                            tir = offset ti + size ti - 1
                            (LZip l2 c2' r2) = findInterval rBuilderInterval tir r

                        in case c2' of

                            Nothing -> do

                                -- add this:     ......[t__________________________]...................
                                -- to this list: ......[c__]......[l2__]...[l2__].....[________].......
                                -- no need to keep the order of l2 as each member of the list will be placed independently from scratch
                                c'' <- $addContext' $ addRBuildersNonEmpty (c : l2) t
                                return $ toList $ LZip l (Just c'') r2

                            Just c2 ->

                                if ti `contains` rBuilderInterval c2 then do

                                    -- add this:     ......[t______________________]........................
                                    -- to this list: ......[c_________]......[c2___]......[________]........
                                    c'' <- $addContext' $ addRBuildersNonEmpty (c : c2 : l2) t
                                    return $ toList $ LZip l (Just c'') r2
                                else

                                    -- add this:     ......[t_________________].............................
                                    -- to this list: ......[c_________]......[c2___]......[________]........
                                    $chainedError $ intersectMessage t c2

                    else

                        -- add this:     ..........[t________].............................
                        -- to this list: ......[c_________]......[_____]......[________]...
                        $chainedError $ intersectMessage t c

                Nothing ->

                    let
                        tir = offset ti + size ti - 1
                        (LZip l2 c2' r2) = findInterval rBuilderInterval tir r
                    in case c2' of

                        Nothing -> do

                            -- add this:     ....[t___].........................................
                            -- or this:      ....[t_________________________]...................
                            -- to this list: .............[l2__]...[l2__].....[________]........
                            c'' <- $addContext' $ addRBuildersNonEmpty l2 t
                            return $ toList $ LZip l (Just c'') r2

                        Just c2 ->

                            if ti `contains` rBuilderInterval c2 then do

                                -- add this:     ....[t_________________________________]........
                                -- to this list: ..........[l2__]..[l2__].....[c2_______]........
                                c'' <- $addContext' $ addRBuildersNonEmpty (c2 : l2) t
                                return $ toList $ LZip l (Just c'') r2

                            else

                                -- add this:     ....[t_______________________________]..........
                                -- to this list: ..........[l2__]..[l2__].....[c2_______]........
                                $chainedError $ intersectMessage t c2

        (emptyRBs, nonEmptyRBs) = L.partition (I.empty . rBuilderInterval) newts

    in
        addRBuilders' addRBuilderNonEmpty nonEmptyRBs [] >>= addRBuilders' addRBuilderEmpty emptyRBs

-- | Find section with a given number
elfFindSection :: forall a m b . (SingElfClassI a, MonadThrow m, Integral b, Show b)
               => ElfListXX a          -- ^ Structured ELF data
               -> b                    -- ^ Number of the section
               -> m (ElfXX 'Section a) -- ^ The section in question
elfFindSection elfs n = if n == 0
    then $chainedError "no section 0"
    else $maybeAddContext ("no section " ++ show n) maybeSection
        where
            maybeSection = getFirst $ foldMapElfList f elfs
            f :: ElfXX t a -> First (ElfXX 'Section a)
            f s@ElfSection{..} | esN == fromIntegral n = First $ Just s
            f _ = First Nothing

-- | Find section with a given name
elfFindSectionByName :: forall a m . (SingElfClassI a, MonadThrow m)
                     => ElfListXX a          -- ^ Structured ELF data
                     -> String               -- ^ Section name
                     -> m (ElfXX 'Section a) -- ^ The section in question
elfFindSectionByName elfs n = $maybeAddContext ("no section \"" ++ show n ++ "\"") maybeSection
    where
        maybeSection = getFirst $ foldMapElfList f elfs
        f :: ElfXX t a -> First (ElfXX 'Section a)
        f s@ElfSection{..} | esName == n = First $ Just s
        f _ = First Nothing

-- | Find ELF header
elfFindHeader :: forall a m . (SingElfClassI a, MonadThrow m)
              => ElfListXX a         -- ^ Structured ELF data
              -> m (ElfXX 'Header a) -- ^ ELF header
elfFindHeader elfs = $maybeAddContext "no header" maybeHeader
    where
        maybeHeader = getFirst $ foldMapElfList f elfs
        f :: ElfXX t a -> First (ElfXX 'Header a)
        f h@ElfHeader{} = First $ Just h
        f _ = First Nothing

cut :: BSL.ByteString -> Int64 -> Int64 -> BSL.ByteString
cut content offset size = BSL.take size $ BSL.drop offset content

tail' :: [a] -> [a]
tail' [] = []
tail' (_ : xs) = xs

nextOffset :: SingElfClassI a => WordXX a -> WordXX a -> WordXX a -> WordXX a
nextOffset _ 0 a = a
nextOffset t m a | m .&. (m - 1) /= 0 = error $ "align module is not power of two " ++ show m
                 | otherwise          = if a' + t' < a then a' + m + t' else a' + t'
    where
        a' = a .&. complement (m - 1)
        t' = t .&. (m - 1)

addRawData :: forall a . SingElfClassI a => BSL.ByteString -> [RBuilder a] -> [RBuilder a]
addRawData _ [] = []
addRawData bs rBuilders = snd $ addRawData' 0 (lrbie, rBuilders)
    where

        -- e, e', ee and lrbie stand for the first occupied byte after the place being fixed
        -- lrbi: last rBuilder interval (begin, size)
        lrbi@(I lrbib lrbis) = rBuilderInterval $ L.last rBuilders
        lrbie = if I.empty lrbi then lrbib else lrbib + lrbis

        allEmpty :: WordXX a -> WordXX a -> Bool
        allEmpty b s = BSL.all (== 0) bs'
            where
                bs' = cut bs (fromIntegral b) (fromIntegral s)

        addRawData' :: WordXX a -> (WordXX a, [RBuilder a]) -> (WordXX a, [RBuilder a])
        addRawData' alignHint (e, rbs) = L.foldr f (e, []) $ fmap fixRBuilder rbs
            where
                f rb (e', rbs') =
                    let
                        i@(I b s) = rBuilderInterval rb
                        b' = if I.empty i then b else b + s
                        rbs'' = addRaw b' e' rbs'
                    in
                        (b, rb : rbs'')

                fixRBuilder :: RBuilder a -> RBuilder a
                fixRBuilder p | I.empty $ rBuilderInterval p = p
                fixRBuilder p@RBuilderSegment{..} =
                    RBuilderSegment{ rbpData = addRaw b ee' rbs', ..}
                        where
                            (I b s) = rBuilderInterval p
                            ee = b + s
                            alignHint' = max (pAlign rbpHeader) alignHint
                            (ee', rbs') = addRawData' alignHint' (ee, rbpData)
                fixRBuilder x = x

                -- b is the first free byte
                addRaw :: WordXX a -> WordXX a -> [RBuilder a] -> [RBuilder a]
                addRaw b ee rbs' =
                    if b < ee
                        then
                            if not $ allEmpty b s
                                then
                                    RBuilderRawData (I b s) : rbs'
                                else
                                    -- check e' < ee means
                                    -- check if next section/segment was actually placed (ee) with greater offset
                                    -- than is required by alignment rules (e')
                                    if e' < ee && e'' == ee
                                        then
                                            RBuilderRawAlign ee alignHint : rbs'
                                        else
                                            rbs'
                        else
                            rbs'
                    where
                        s = ee - b
                        eAddr = case rbs' of
                            (RBuilderSegment{rbpHeader = SegmentXX{..}} : _) -> pVirtAddr
                            _ -> 0
                        eAddrAlign = case rbs' of
                            (RBuilderSegment{rbpHeader = SegmentXX{..}} : _) -> pAlign
                            (RBuilderSection{rbsHeader = SectionXX{..}} : _) -> sAddrAlign
                            _ -> wordSize $ fromSingElfClass $ singElfClass @a
                        -- e' here is the address of the next section/segment
                        -- according to the regular alignment rules
                        e' = nextOffset eAddr eAddrAlign b
                        e'' = nextOffset ee alignHint b

infix 9 !!?

(!!?) :: (Integral b) => [a] -> b -> Maybe a
(!!?) xs i
    | i < 0     = Nothing
    | otherwise = go i xs
  where
    go :: (Integral b) => b -> [a] -> Maybe a
    go 0 (x:_)  = Just x
    go j (_:ys) = go (j - 1) ys
    go _ []     = Nothing

-- | Parse ELF file and produce [`RBuilder`]
parseRBuilder :: (SingElfClassI a, MonadCatch m)
              => HeaderXX a     -- ^ ELF header
              -> [SectionXX a]  -- ^ Section table
              -> [SegmentXX a]  -- ^ Segment table
              -> BSL.ByteString -- ^ ELF file
              -> m [RBuilder a]
parseRBuilder hdr@HeaderXX{..} ss ps bs = do


    let
        maybeStringSectionData = getSectionData bs <$> (ss !!? hShStrNdx)

        mkRBuilderSection :: (SingElfClassI a, MonadCatch m) => (ElfSectionIndex, SectionXX a) -> m (RBuilder a)
        mkRBuilderSection (n, s@SectionXX{..}) = do
            stringSectionData <- $maybeAddContext "No string table" maybeStringSectionData
            return $ RBuilderSection s n $ getString stringSectionData $ fromIntegral sName

        mkRBuilderSegment :: (SingElfClassI a, MonadCatch m) => (Word16, SegmentXX a) -> m (RBuilder a)
        mkRBuilderSegment (n, p) = return $ RBuilderSegment p n []

    sections <- mapM mkRBuilderSection $ tail' $ Prelude.zip [0 .. ] ss
    segments <- mapM mkRBuilderSegment $         Prelude.zip [0 .. ] ps

    let

        header            = RBuilderHeader hdr
        maybeSectionTable = if hShNum == 0 then Nothing else  Just $ RBuilderSectionTable hdr
        maybeSegmentTable = if hPhNum == 0 then Nothing else  Just $ RBuilderSegmentTable hdr

    rbs <- addRBuilders $ [header] ++ maybeToList maybeSectionTable
                                   ++ maybeToList maybeSegmentTable
                                   ++ segments
                                   ++ sections
    return $ addRawData bs rbs

parseElf' :: forall a m . (SingElfClassI a, MonadCatch m) =>
                                               HeaderXX a ->
                                            [SectionXX a] ->
                                            [SegmentXX a] ->
                                           BSL.ByteString -> m Elf
parseElf' hdr@HeaderXX{..} ss ps bs = do

    rbs <- parseRBuilder hdr ss ps bs

    let
        rBuilderToElf :: RBuilder a -> ElfListXX a -> m (ElfListXX a)
        rBuilderToElf RBuilderHeader{} l =
            return $ ElfListCons ElfHeader
                { ehData       = hData
                , ehOSABI      = hOSABI
                , ehABIVersion = hABIVersion
                , ehType       = hType
                , ehMachine    = hMachine
                , ehEntry      = hEntry
                , ehFlags      = hFlags
                } l
        rBuilderToElf RBuilderSectionTable{} l =
            return $ ElfListCons ElfSectionTable l
        rBuilderToElf RBuilderSegmentTable{} l =
            return $ ElfListCons ElfSegmentTable l
        rBuilderToElf RBuilderSection{ rbsHeader = s@SectionXX{..}, ..} l =
            return $ ElfListCons ElfSection
                { esName      = rbsName
                , esType      = sType
                , esFlags     = fromIntegral sFlags
                , esAddr      = sAddr
                , esAddrAlign = sAddrAlign
                , esEntSize   = sEntSize
                , esN         = rbsN
                , esInfo      = sInfo
                , esLink      = sLink
                , esData      =
                    if rbsN == hShStrNdx
                        then ElfSectionDataStringTable
                        else if sType == SHT_NOBITS
                            then ElfSectionDataNoBits sSize
                            else ElfSectionData $ getSectionData bs s
                } l
        rBuilderToElf RBuilderSegment{ rbpHeader = SegmentXX{..}, ..} l = do
            d <- foldrM rBuilderToElf ElfListNull rbpData
            addMemSize <- if pMemSize /= 0 && pFileSize /= 0 && pMemSize < pFileSize
                then $chainedError "memSize < fileSize"
                else return (pMemSize - pFileSize)
            return $ ElfListCons ElfSegment
                { epType        = pType
                , epFlags       = pFlags
                , epVirtAddr    = pVirtAddr
                , epPhysAddr    = pPhysAddr
                , epAddMemSize  = addMemSize
                , epAlign       = pAlign
                , epData        = d
                } l
        rBuilderToElf RBuilderRawData{ rbrdInterval = I o s } l =
            return $ ElfListCons (ElfRawData $ cut bs (fromIntegral o) (fromIntegral s)) l
        rBuilderToElf RBuilderRawAlign{..} l =
            return $ ElfListCons (ElfRawAlign rbraOffset rbraAlign) l

    el <- foldrM rBuilderToElf ElfListNull rbs --  mapM rBuilderToElf rbs
    return $ Elf singElfClass el

-- | Parse ELF file
parseElf :: MonadCatch m => BSL.ByteString -> m Elf
parseElf bs = do
    Headers classS hdr ss ps <- parseHeaders bs
    withSingElfClassI classS parseElf' hdr ss ps bs

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

wbStateInit :: forall a . SingElfClassI a => WBuilderState a
wbStateInit = WBuilderState
    { _wbsSections         = []
    , _wbsSegmentsReversed = []
    , _wbsDataReversed     = []
    , _wbsOffset           = 0
    , _wbsPhOff            = 0
    , _wbsShOff            = 0
    , _wbsShStrNdx         = 0
    , _wbsNameIndexes      = []
    }

zeroSection :: forall a . SingElfClassI a => SectionXX a
zeroSection = SectionXX 0 0 0 0 0 0 0 0 0 0

neighbours :: [a] -> (a -> a -> b) -> [b]
neighbours          [] _ = []
neighbours x@(_:xtail) f = fmap (uncurry f) $ L.zip x xtail

-- make string table and indexes for it from a list of strings
mkStringTable :: [String] -> (BSL.ByteString, [Int64])
mkStringTable sectionNames = (stringTable, os)
    where

        -- names:
        -- i for indexes of the section entry in section table
        -- n for section name string
        -- o for offset of the string in the string table
        -- in, io -- for pairs
        -- ins, ios -- for lists of pairs
        -- etc

        (ins0, ins) = L.break ((/= "") . snd) $ L.sortOn (L.length . snd) $ L.zip [(1 :: Word32) .. ] sectionNames
        ios0 = fmap f' ins0
            where
                f' (i, _) = (i, 0)

        (stringTable, ios, _) = f (BSL.singleton 0, [], L.reverse ins)

        os = fmap snd $ L.sortOn fst $ ios0 ++ ios

        -- create string table.  If one name is a suffix of another,
        -- allocate only the longest name in string table
        f x@(_, _, []) = x
        f (st, iosf, (i, n) : insf) = f (st', iosf'', insf')

            where

                st' = st <> BSL8.pack n <> BSL.singleton 0
                o = BSL.length st
                iosf'' = (i, o) : iosf' ++ iosf

                (iosf', insf') = ff insf

                -- look if there exists a name that is a suffix for the currently allocated name
                -- in the list of unallocated indexed section names
                ff = L.foldr fff ([], [])
                    where
                        fff (i', n') (iosff, insff) = if n' `L.isSuffixOf` n
                            then
                                let
                                    o' = o + fromIntegral (L.length n - L.length n')
                                in
                                    ((i', o') : iosff, insff)
                            else (iosff, (i', n') : insff)

serializeElf' :: forall a m . (SingElfClassI a, MonadCatch m) => ElfListXX a -> m BSL.ByteString
serializeElf' elfs = do

    -- FIXME: it's better to match constructor here, but there is a bug that prevents to conclude that
    -- the match is irrefutable:
    -- https://stackoverflow.com/questions/72803815/phantom-type-makes-pattern-matching-irrefutable-but-that-seemingly-does-not-wor
    -- https://gitlab.haskell.org/ghc/ghc/-/issues/15681#note_165436
    -- But if I use lazy pattern match, then some other bug comes up that prevents type inference
    -- on GHC 9.0.2
    header' <- $addContext' $ elfFindHeader elfs

    let

        elfClass = fromSingElfClass $ singElfClass @a

        sectionN :: Num b => b
        sectionN = getSum $ foldMapElfList f elfs
            where
                f ElfSection{} = Sum 1
                f _ =  Sum 0

        sectionNames :: [String]
        sectionNames = foldMapElfList f elfs
            where
                f :: ElfXX t a -> [String]
                f ElfSection{..} = [ esName ]
                f _ = []

        (stringTable, nameIndexes) = mkStringTable sectionNames

        segmentN :: Num b => b
        segmentN = getSum $ foldMapElfList f elfs
            where
                f ElfSegment{} = Sum 1
                f _ =  Sum 0

        sectionTable :: Bool
        sectionTable = getAny $ foldMapElfList f elfs
            where
                f ElfSectionTable =  Any True
                f _ = Any False

        align :: (MonadThrow n, MonadState (WBuilderState a) n) => WordXX a -> WordXX a -> n ()
        align _ 0 = return ()
        align _ 1 = return ()
        align t m | m .&. (m - 1) /= 0 = $chainedError $ "align module is not power of two " ++ show m
                  | otherwise = do
            offset  <- use wbsOffset
            wbsOffset .= nextOffset t m offset
            offset' <- use wbsOffset
            wbsDataReversed %= (WBuilderDataByteStream (BSL.replicate (fromIntegral $ offset' - offset) 0) :)

        alignWord :: (MonadThrow n, MonadState (WBuilderState a) n) => n ()
        alignWord = align 0 $ wordSize $ fromSingElfClass $ singElfClass @a

        dataIsEmpty :: ElfSectionData c -> Bool
        dataIsEmpty (ElfSectionData bs)       = BSL.null bs
        dataIsEmpty ElfSectionDataStringTable = BSL.null stringTable
        dataIsEmpty (ElfSectionDataNoBits _)  = True

        lastSection :: ElfListXX a -> (forall t' . (ElfXX t' a -> b)) -> b -> b
        lastSection ElfListNull _ b = b
        lastSection (ElfListCons v ElfListNull) f _ = f v
        lastSection (ElfListCons _ l) f b = lastSection l f b

        lastSectionIsEmpty :: ElfListXX a -> Bool
        lastSectionIsEmpty l = lastSection l f False
            where
                f ElfSection { .. } = dataIsEmpty esData
                f _                 = False

        elf2WBuilder :: (MonadThrow n, MonadState (WBuilderState a) n) => ElfXX t a -> n ()
        elf2WBuilder ElfHeader{} = do
            -- FIXME: add push monad
            wbsDataReversed %= (WBuilderDataHeader :)
            wbsOffset += headerSize elfClass
        elf2WBuilder ElfSectionTable = do
            alignWord
            use wbsOffset >>= assign wbsShOff
            wbsDataReversed %= (WBuilderDataSectionTable :)
            wbsOffset += (sectionN + 1) * sectionTableEntrySize elfClass
        elf2WBuilder ElfSegmentTable = do
            alignWord
            use wbsOffset >>= assign wbsPhOff
            wbsDataReversed %= (WBuilderDataSegmentTable :)
            wbsOffset += segmentN * segmentTableEntrySize elfClass
        elf2WBuilder ElfSection{esFlags = ElfSectionFlag f, ..} = do
            when (f .&. fromIntegral (complement (maxBound @(WordXX a))) /= 0) do
                $chainedError $ "section flags at section " ++ show esN ++ "don't fit"
            -- I don't see any sense in aligning NOBITS section data
            -- still gcc does it for .o files
            when (esType /= SHT_NOBITS || (ehType header') == ET_REL) do
                align 0 esAddrAlign
            (n, ns) <- uses wbsNameIndexes \case
                n' : ns' -> (n', ns')
                _ -> error "internal error: different number of sections in two iterations"
            shStrNdx' <- use wbsShStrNdx
            let
                (d, shStrNdx, sz) = case esData of
                    ElfSectionData { .. } -> (esdData, shStrNdx', fromIntegral $ BSL.length esdData)
                    ElfSectionDataStringTable -> (stringTable, esN, fromIntegral $ BSL.length stringTable)
                    ElfSectionDataNoBits { .. } -> (BSL.empty, shStrNdx', esdSize)
                sName = fromIntegral n                 -- Word32
                sType = esType                         -- ElfSectionType
                sFlags = fromIntegral f
                sAddr = esAddr                         -- WXX c
                sSize = sz                             -- WXX c
                sLink = esLink                         -- Word32
                sInfo = esInfo                         -- Word32
                sAddrAlign = esAddrAlign               -- WXX c
                sEntSize = esEntSize                   -- WXX c
            sOffset <- use wbsOffset                   -- WXX c
            wbsSections %= ((esN, SectionXX { .. }) :)
            wbsDataReversed %= (WBuilderDataByteStream d :)
            wbsOffset += fromIntegral (BSL.length d)
            wbsShStrNdx .= shStrNdx
            wbsNameIndexes .= ns
        elf2WBuilder ElfSegment { .. } = do
            align epVirtAddr epAlign
            offset <- use wbsOffset
            void $ mapMElfList elf2WBuilder epData
            offset' <- use wbsOffset
            let
                -- allocate one more byte in the end of segment if there exists an empty section
                -- at the end so that that empty section will go to the current segment
                add1 = lastSectionIsEmpty epData && offset /= offset'
                pType = epType
                pFlags = epFlags
                pOffset = offset
                pVirtAddr = epVirtAddr
                pPhysAddr = epPhysAddr
                pFileSize = offset' - offset + if add1 then 1 else 0
                pMemSize = pFileSize + epAddMemSize
                pAlign = epAlign
            wbsSegmentsReversed %= (SegmentXX { .. } :)
            when add1 do
                wbsDataReversed %= (WBuilderDataByteStream (BSL.singleton 0) :)
                wbsOffset += 1
        elf2WBuilder ElfRawData { .. } = do
            wbsDataReversed %= (WBuilderDataByteStream edData :)
            wbsOffset += fromIntegral (BSL.length edData)
        elf2WBuilder ElfRawAlign { .. } = align eaOffset eaAlign

        fixSections :: [(ElfSectionIndex, SectionXX a)] -> m [SectionXX a]
        fixSections ss = do
            when (L.length ss /= sectionN) (error "internal error: L.length ss /= sectionN")
            let
                f (ln, _) (rn, _) = ln `compare` rn
                sorted = L.sortBy f ss
                next (ln, _) (rn, _) = ln + 1 == rn
                checkNeibours = and $ neighbours sorted next

            unless checkNeibours ($chainedError "sections are not consistent")
            return $ fmap snd sorted

        wbState2ByteString :: WBuilderState a -> m BSL.ByteString
        wbState2ByteString WBuilderState{..} = do

            sections <- fixSections _wbsSections

            let
                f WBuilderDataHeader =
                    case header' of
                        ElfHeader{..} ->
                            let
                                hData       = ehData
                                hOSABI      = ehOSABI
                                hABIVersion = ehABIVersion
                                hType       = ehType
                                hMachine    = ehMachine
                                hEntry      = ehEntry
                                hPhOff      = _wbsPhOff
                                hShOff      = _wbsShOff
                                hFlags      = ehFlags
                                hPhEntSize  = segmentTableEntrySize elfClass
                                hPhNum      = segmentN :: Word16
                                hShEntSize  = sectionTableEntrySize elfClass
                                hShNum      = (if sectionTable then sectionN + 1 else 0) :: Word16
                                hShStrNdx   = _wbsShStrNdx

                                h :: H.Header
                                h = H.Header (singElfClass @a) HeaderXX{..}
                            in
                                encode h
                f WBuilderDataByteStream {..} = wbdData
                f WBuilderDataSectionTable =
                    serializeBList (ehData header') $ zeroSection : sections
                f WBuilderDataSegmentTable =
                    serializeBList (ehData header') $ L.reverse _wbsSegmentsReversed

            return $ foldMap f $ L.reverse _wbsDataReversed

    execStateT (mapMElfList elf2WBuilder elfs) wbStateInit{ _wbsNameIndexes = nameIndexes } >>= wbState2ByteString

-- | Serialze ELF file
serializeElf :: MonadCatch m => Elf -> m BSL.ByteString
serializeElf (Elf classS ls) = withSingElfClassI classS serializeElf' ls

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

-- FIXME: move this to a separate file

-- | Parsed ELF symbol table entry. NB: This is work in progress
data ElfSymbolXX c =
    ElfSymbolXX
        { steName  :: String           -- ^ Symbol name (NB: String, not string index)
        , steBind  :: ElfSymbolBinding -- ^ Symbol binding attributes
        , steType  :: ElfSymbolType    -- ^ Symbol Type
        , steShNdx :: ElfSectionIndex  -- ^ Section table index
        , steValue :: WordXX c         -- ^ Symbol value
        , steSize  :: WordXX c         -- ^ Size of object
        }

getStringFromData :: BSL.ByteString -> Word32 -> String
getStringFromData stringTable offset = BSL8.unpack $ BSL.takeWhile (/= 0) $ BSL.drop (fromIntegral offset) stringTable

mkElfSymbolTableEntry :: SingElfClassI a => BSL.ByteString -> SymbolXX a -> ElfSymbolXX a
mkElfSymbolTableEntry stringTable SymbolXX{..} =
    let
        steName  = getStringFromData stringTable stName
        steBind  = ElfSymbolBinding $ stInfo `shiftR` 4
        steType  = ElfSymbolType $ stInfo .&. 0x0f
        steShNdx = stShNdx
        steValue = stValue
        steSize  = stSize
    in
        ElfSymbolXX{..}

-- | Parse symbol table
parseSymbolTable :: (MonadThrow m, SingElfClassI a)
                 => ElfData           -- ^ Endianness of the ELF file
                 -> ElfXX 'Section a  -- ^ Parsed section such that @`sectionIsSymbolTable` . `sType`@ is true.
                 -> ElfListXX a       -- ^ Structured ELF data
                 -> m [ElfSymbolXX a] -- ^ Symbol table
parseSymbolTable d symbolTableSection@(ElfSection { .. }) elfs = do

    symbolTable <- case symbolTableSection of
        ElfSection{ esData = ElfSectionData st } -> return st
        _ -> $chainedError "wrong symbol table section data"

    section <- elfFindSection elfs esLink
    stringTable <- case section of
        ElfSection{ esData = ElfSectionData st } -> return st
        _ -> $chainedError "wrong string table section data"

    st <- parseBList d symbolTable
    return (mkElfSymbolTableEntry stringTable <$> st)

mkSymbolTableEntry :: Word32 -> ElfSymbolXX a -> SymbolXX a
mkSymbolTableEntry nameIndex ElfSymbolXX{..} =
    let
        ElfSymbolBinding b = steBind
        ElfSymbolType t = steType

        stName  = nameIndex
        stInfo  = b `shift` 4 .|. t
        stOther = 0 :: Word8
        stShNdx = steShNdx
        stValue = steValue
        stSize  = steSize
    in
        SymbolXX{..}

-- | Serialize symbol table
serializeSymbolTable :: (MonadThrow m, SingElfClassI a)
                     => ElfData                            -- ^ Endianness of the ELF file
                     -> [ElfSymbolXX a]                    -- ^ Symbol table
                     -> m (BSL.ByteString, BSL.ByteString) -- ^ Pair of symbol table section data and string table section data
serializeSymbolTable d ss = do

    let
        (stringTable, stringIndexes) = mkStringTable $ fmap steName ss
        ssWithNameIndexes = L.zip ss stringIndexes

        f :: (ElfSymbolXX a, Int64) -> SymbolXX a
        f (s, n) = mkSymbolTableEntry (fromIntegral n) s

        symbolTable = serializeBList d $ fmap f ssWithNameIndexes

    return (symbolTable, stringTable)
