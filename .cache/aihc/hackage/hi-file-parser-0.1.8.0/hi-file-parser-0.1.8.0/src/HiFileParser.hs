{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant multi-way if" #-}
{-# HLINT ignore "Reduce duplication"     #-}

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module HiFileParser
  ( Interface (..)
  , List (..)
  , Dictionary (..)
  , Module (..)
  , Usage (..)
  , Dependencies (..)
  , getInterface
  , fromFile
  ) where

import           Control.Monad ( replicateM, replicateM_, when )
import           Control.Monad.State
                   ( StateT, evalStateT, get, gets, lift, modify )
import           Data.Binary ( Word32, Word64, Word8)
import qualified Data.Binary.Get as G
                   ( Decoder (..), Get, bytesRead, getByteString, getInt64be
                   , getWord32be, getWord64be, getWord8, lookAhead
                   , runGetIncremental, skip
                   )
import           Data.Bits
                   ( FiniteBits (..), (.|.), clearBit, complement, testBit
                   , unsafeShiftL
                   )
import           Data.Bool ( bool )
import           Data.ByteString.Lazy.Internal ( defaultChunkSize )
import           Data.Char ( chr )
import           Data.Functor ( ($>), void )
import           Data.Maybe ( catMaybes )
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup ( (<>) )
#endif
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as V
import qualified Debug.Trace
import           GHC.IO.IOMode ( IOMode (..) )
import           Numeric ( showHex )
import           RIO ( Generic, Int64, NFData )
import           RIO.ByteString as B ( ByteString, hGetSome, null )
import           System.IO ( withBinaryFile )

newtype IfaceGetState = IfaceGetState
  { useLEB128 :: Bool -- ^ Use LEB128 encoding for numbers
  }

data IfaceVersion
  = V7021
  | V7041
  | V7061
  | V7081
  | V8001
  | V8021
  | V8041
  | V8061
  | V8101
  | V9001
  | V9041
  | V9045
  | V9081
  | V9121
  | V9140
  deriving (Eq, Enum, Ord, Show)
  -- careful, the Ord matters!


type Get a = StateT IfaceGetState G.Get a

-- | Change this to 'True' to enable debugging.
enableDebug :: Bool
enableDebug = False

traceGet :: String -> Get ()
traceGet s
  | enableDebug = Debug.Trace.trace s (return ())
  | otherwise   = return ()

traceShow :: Show a => String -> Get a -> Get a
traceShow s g
  | not enableDebug = g
  | otherwise = do
    a <- g
    traceGet (s ++ " " ++ show a)
    return a

runGetIncremental :: Get a -> G.Decoder a
runGetIncremental g = G.runGetIncremental (evalStateT g emptyState)
 where
  emptyState = IfaceGetState False

getByteString :: Int -> Get ByteString
getByteString i = lift (G.getByteString i)

getWord8 :: Get Word8
getWord8 = lift G.getWord8

-- | Like 'getWord8' but we discard the information.
skipWord8 :: Get ()
skipWord8 = void getWord8

bytesRead :: Get Int64
bytesRead = lift G.bytesRead

skip :: Int -> Get ()
skip = lift . G.skip

uleb :: Get a -> Get a -> Get a
uleb f g = do
  c <- gets useLEB128
  if c then f else g

getWord32be :: Get Word32
getWord32be = uleb getULEB128 (lift G.getWord32be)

getWord64be :: Get Word64
getWord64be = uleb getULEB128 (lift G.getWord64be)

getInt64be :: Get Int64
getInt64be = uleb getSLEB128 (lift G.getInt64be)

lookAhead :: Get b -> Get b
lookAhead g = do
  s <- get
  lift $ G.lookAhead (evalStateT g s)

getPtr :: Get Word32
getPtr = lift G.getWord32be

-- | Like 'getPtr' but we discard the information.
skipPtr :: Get ()
skipPtr = void getPtr

type IsBoot = Bool

type ModuleName = ByteString

newtype List a = List
  { unList :: [a]
  } deriving newtype (NFData, Show)

newtype Dictionary = Dictionary
  { unDictionary :: V.Vector ByteString
  } deriving newtype (NFData, Show)

newtype Module = Module
  { unModule :: ModuleName
  } deriving newtype (NFData, Show)

newtype Usage = Usage
  { unUsage :: FilePath
  } deriving newtype (NFData, Show)

data Dependencies = Dependencies
  { dmods    :: List (ModuleName, IsBoot)
  , dpkgs    :: List (ModuleName, Bool)
  , dorphs   :: List Module
  , dfinsts  :: List Module
  , dplugins :: List ModuleName
  } deriving (Generic, Show)

instance NFData Dependencies

data Interface = Interface
  { deps  :: Dependencies
  , usage :: List Usage
  } deriving (Generic, Show)

instance NFData Interface

-- | The 'String' is for debugging messages. Provided for consistency with
-- 'skipWith'.
with :: Show a => String -> Get a -> Get a
with = traceShow

-- | Like 'with' but we discard the information.
skipWith :: Show a => String -> Get a -> Get ()
skipWith s = void . traceShow s 

-- | Read a block prefixed with its length
withBlockPrefix :: Get a -> Get a
withBlockPrefix f = getPtr *> f

-- | Skip a block prefixed with its length. The 'String' is for debugging
-- messages
skipWithBlockPrefix :: String -> Get ()
skipWithBlockPrefix s = do
  l <- traceShow (s <> ", skipping:") getPtr
  skip (fromIntegral l - 4)

getBool :: Get Bool
getBool = toEnum . fromIntegral <$> getWord8

-- | Like 'getBool' but we discard the information.
skipBool :: Get ()
skipBool = void getBool

getString :: Get String
getString = fmap (chr . fromIntegral) . unList <$> getList getWord32be

getMaybe :: Get a -> Get (Maybe a)
getMaybe f = bool (pure Nothing) (Just <$> f) =<< getBool

-- | Like 'getMaybe' but we discard the information.
skipMaybe :: Get a -> Get ()
skipMaybe = void . getMaybe

getList :: Get a -> Get (List a)
getList f = do
  use_uleb <- gets useLEB128
  if use_uleb
    then do
      l <- (getSLEB128 :: Get Int64)
      List <$> replicateM (fromIntegral l) f
    else do
      i <- getWord8
      l <-
        if i == 0xff
          then getWord32be
          else pure (fromIntegral i :: Word32)
      List <$> replicateM (fromIntegral l) f

-- | Like 'getList' but we discard the information.
skipList :: Get a -> Get ()
skipList = void . getList

getTuple :: Get a -> Get b -> Get (a, b)
getTuple f g = (,) <$> f <*> g

getByteStringSized :: Get ByteString
getByteStringSized = do
  size <- getInt64be
  getByteString (fromIntegral size)

getDictionary :: Int -> Get Dictionary
getDictionary ptr = do
  offset <- bytesRead
  skip $ ptr - fromIntegral offset
  size <- fromIntegral <$> getInt64be
  traceGet ("Dictionary size: " ++ show size)
  dict <- Dictionary <$> V.replicateM size getByteStringSized
  traceGet ("Dictionary: " ++ show dict)
  return dict

-- | Get a FastString
--
-- FastStrings are stored in a global FastString table and only the index (a
-- Word32be) is stored at the expected position.
getCachedBS :: Dictionary -> Get ByteString
getCachedBS d = go =<< with "Dict index:" getWord32be
 where
  go i =
    case unDictionary d V.!? fromIntegral i of
      Just bs -> pure bs
      Nothing -> fail $ "Invalid dictionary index: " <> show i

-- | Get Fingerprint
getFP :: Get String
getFP = do
  x <- getWord64be
  y <- getWord64be
  return (showHex x (showHex y ""))

-- Like 'getFP' but we discard the information.
skipFP :: Get ()
skipFP = void getFP

getInterface721 :: Dictionary -> Get Interface
getInterface721 d = do
  void getModule
  skipBool
  replicateM_ 2 skipFP
  skipBool
  skipBool
  Interface <$> getDependencies <*> getUsage
 where
  getModule = getCachedBS d *> (Module <$> getCachedBS d)
  getDependencies =
    withBlockPrefix $
    Dependencies <$> getList (getTuple (getCachedBS d) getBool) <*>
    getList (getTuple (getCachedBS d) getBool) <*>
    getList getModule <*>
    getList getModule <*>
    pure (List [])
  getUsage = withBlockPrefix $ List . catMaybes . unList <$> getList go
   where
    go :: Get (Maybe Usage)
    go = do
      usageType <- getWord8
      case usageType of
        0 -> getModule *> skipFP*> getBool $> Nothing
        1 ->
             getCachedBS d *> skipFP*> getMaybe skipFP*>
             getList (getTuple (getWord8 *> getCachedBS d) skipFP) *>
             getBool $> Nothing
        _ -> fail $ "Invalid usageType: " <> show usageType

getInterface741 :: Dictionary -> Get Interface
getInterface741 d = do
  void getModule
  skipBool
  replicateM_ 3 skipFP
  skipBool
  skipBool
  Interface <$> getDependencies <*> getUsage
 where
  getModule = getCachedBS d *> (Module <$> getCachedBS d)
  getDependencies =
    withBlockPrefix $
    Dependencies <$> getList (getTuple (getCachedBS d) getBool) <*>
    getList (getTuple (getCachedBS d) getBool) <*>
    getList getModule <*>
    getList getModule <*>
    pure (List [])
  getUsage = withBlockPrefix $ List . catMaybes . unList <$> getList go
   where
    go :: Get (Maybe Usage)
    go = do
      usageType <- getWord8
      case usageType of
        0 -> getModule *> skipFP*> getBool $> Nothing
        1 ->
             getCachedBS d *> skipFP*> getMaybe skipFP*>
             getList (getTuple (getWord8 *> getCachedBS d) skipFP) *>
             getBool $> Nothing
        2 -> Just . Usage <$> getString <* getWord64be <* getWord64be
        _ -> fail $ "Invalid usageType: " <> show usageType

getInterface761 :: Dictionary -> Get Interface
getInterface761 d = do
  void getModule
  skipBool
  replicateM_ 3 skipFP
  skipBool
  skipBool
  Interface <$> getDependencies <*> getUsage
 where
  getModule = getCachedBS d *> (Module <$> getCachedBS d)
  getDependencies =
    withBlockPrefix $
    Dependencies <$> getList (getTuple (getCachedBS d) getBool) <*>
    getList (getTuple (getCachedBS d) getBool) <*>
    getList getModule <*>
    getList getModule <*>
    pure (List [])
  getUsage = withBlockPrefix $ List . catMaybes . unList <$> getList go
   where
    go :: Get (Maybe Usage)
    go = do
      usageType <- getWord8
      case usageType of
        0 -> getModule *> skipFP*> getBool $> Nothing
        1 ->
             getCachedBS d *> skipFP*> getMaybe skipFP*>
             getList (getTuple (getWord8 *> getCachedBS d) skipFP) *>
             getBool $> Nothing
        2 -> Just . Usage <$> getString <* getWord64be <* getWord64be
        _ -> fail $ "Invalid usageType: " <> show usageType

getInterface781 :: Dictionary -> Get Interface
getInterface781 d = do
  void getModule
  skipBool
  replicateM_ 3 skipFP
  skipBool
  skipBool
  Interface <$> getDependencies <*> getUsage
 where
  getModule = getCachedBS d *> (Module <$> getCachedBS d)
  getDependencies =
    withBlockPrefix $
    Dependencies <$> getList (getTuple (getCachedBS d) getBool) <*>
    getList (getTuple (getCachedBS d) getBool) <*>
    getList getModule <*>
    getList getModule <*>
    pure (List [])
  getUsage = withBlockPrefix $ List . catMaybes . unList <$> getList go
   where
    go :: Get (Maybe Usage)
    go = do
      usageType <- getWord8
      case usageType of
        0 -> getModule *> skipFP*> getBool $> Nothing
        1 ->
             getCachedBS d *> skipFP*> getMaybe skipFP*>
             getList (getTuple (getWord8 *> getCachedBS d) skipFP) *>
             getBool $> Nothing
        2 -> Just . Usage <$> getString <* skipFP
        _ -> fail $ "Invalid usageType: " <> show usageType

getInterface801 :: Dictionary -> Get Interface
getInterface801 d = do
  void getModule
  skipWord8
  replicateM_ 3 skipFP
  skipBool
  skipBool
  Interface <$> getDependencies <*> getUsage
 where
  getModule = getCachedBS d *> (Module <$> getCachedBS d)
  getDependencies =
    withBlockPrefix $
    Dependencies <$> getList (getTuple (getCachedBS d) getBool) <*>
    getList (getTuple (getCachedBS d) getBool) <*>
    getList getModule <*>
    getList getModule <*>
    pure (List [])
  getUsage = withBlockPrefix $ List . catMaybes . unList <$> getList go
   where
    go :: Get (Maybe Usage)
    go = do
      usageType <- getWord8
      case usageType of
        0 -> getModule *> skipFP*> getBool $> Nothing
        1 ->
             getCachedBS d *> skipFP*> getMaybe skipFP*>
             getList (getTuple (getWord8 *> getCachedBS d) skipFP) *>
             getBool $> Nothing
        2 -> Just . Usage <$> getString <* skipFP
        3 -> getModule *> skipFP$> Nothing
        _ -> fail $ "Invalid usageType: " <> show usageType

getInterface821 :: Dictionary -> Get Interface
getInterface821 d = do
  void getModule
  skipMaybe getModule
  skipWord8
  replicateM_ 3 skipFP
  skipBool
  skipBool
  Interface <$> getDependencies <*> getUsage
 where
  getModule = do
    idType <- getWord8
    case idType of
      0 -> void $ getCachedBS d
      _ ->
          void $
          getCachedBS d *> getList (getTuple (getCachedBS d) getModule)
    Module <$> getCachedBS d
  getDependencies =
    withBlockPrefix $
    Dependencies <$> getList (getTuple (getCachedBS d) getBool) <*>
    getList (getTuple (getCachedBS d) getBool) <*>
    getList getModule <*>
    getList getModule <*>
    pure (List [])
  getUsage = withBlockPrefix $ List . catMaybes . unList <$> getList go
   where
    go :: Get (Maybe Usage)
    go = do
      usageType <- getWord8
      case usageType of
        0 -> getModule *> skipFP*> getBool $> Nothing
        1 ->
             getCachedBS d *> skipFP*> getMaybe skipFP*>
             getList (getTuple (getWord8 *> getCachedBS d) skipFP) *>
             getBool $> Nothing
        2 -> Just . Usage <$> getString <* skipFP
        3 -> getModule *> skipFP$> Nothing
        _ -> fail $ "Invalid usageType: " <> show usageType

getInterface841 :: Dictionary -> Get Interface
getInterface841 d = do
  void getModule
  skipMaybe getModule
  skipWord8
  replicateM_ 5 skipFP
  skipBool
  skipBool
  Interface <$> getDependencies <*> getUsage
 where
  getModule = do
    idType <- getWord8
    case idType of
      0 -> void $ getCachedBS d
      _ ->
          void $
          getCachedBS d *> getList (getTuple (getCachedBS d) getModule)
    Module <$> getCachedBS d
  getDependencies =
    withBlockPrefix $
    Dependencies <$> getList (getTuple (getCachedBS d) getBool) <*>
    getList (getTuple (getCachedBS d) getBool) <*>
    getList getModule <*>
    getList getModule <*>
    pure (List [])
  getUsage = withBlockPrefix $ List . catMaybes . unList <$> getList go
   where
    go :: Get (Maybe Usage)
    go = do
      usageType <- getWord8
      case usageType of
        0 -> getModule *> skipFP*> getBool $> Nothing
        1 ->
             getCachedBS d *> skipFP*> getMaybe skipFP*>
             getList (getTuple (getWord8 *> getCachedBS d) skipFP) *>
             getBool $> Nothing
        2 -> Just . Usage <$> getString <* skipFP
        3 -> getModule *> skipFP$> Nothing
        _ -> fail $ "Invalid usageType: " <> show usageType

getInterface861 :: Dictionary -> Get Interface
getInterface861 d = do
  void getModule
  skipMaybe getModule
  skipWord8
  replicateM_ 6 skipFP
  skipBool
  skipBool
  Interface <$> getDependencies <*> getUsage
 where
  getModule = do
    idType <- getWord8
    case idType of
      0 -> void $ getCachedBS d
      _ ->
           void $
           getCachedBS d *> getList (getTuple (getCachedBS d) getModule)
    Module <$> getCachedBS d
  getDependencies =
    withBlockPrefix $
    Dependencies <$> getList (getTuple (getCachedBS d) getBool) <*>
    getList (getTuple (getCachedBS d) getBool) <*>
    getList getModule <*>
    getList getModule <*>
    getList (getCachedBS d)
  getUsage = withBlockPrefix $ List . catMaybes . unList <$> getList go
   where
    go :: Get (Maybe Usage)
    go = do
      usageType <- getWord8
      case usageType of
        0 -> getModule *> skipFP*> getBool $> Nothing
        1 ->
             getCachedBS d *> skipFP*> getMaybe skipFP*>
             getList (getTuple (getWord8 *> getCachedBS d) skipFP) *>
             getBool $> Nothing
        2 -> Just . Usage <$> getString <* skipFP
        3 -> getModule *> skipFP$> Nothing
        _ -> fail $ "Invalid usageType: " <> show usageType

getInterfaceRecent :: IfaceVersion -> Dictionary -> Get Interface
getInterfaceRecent version d = do
  if 
    | version >= V9140 -> do
        skipIfaceModInfo -- mi_mod_info_
        skipFP           -- mi_iface_hash_
        ddeps <- withBlockPrefix getDependencies
        skipWithBlockPrefix "mi_public_"
        skipWithBlockPrefix "mi_top_env_"
        skipMaybe (skipWithBlockPrefix "mi_docs_")
        mDusage <- getMaybe (withBlockPrefix getIfaceSelfRecompInfo)
        case mDusage of
          Nothing ->
            -- There are no usages. Is that problematic for Stack?
            pure (Interface ddeps (List []))
          Just dusage -> pure (Interface ddeps dusage)
    | otherwise -> do
        skipWith "Module:" getModule
        skipWith "Sig:" $ getMaybe getModule
        skipWord8 -- hsc_src
        skipFP    -- iface_hash
        skipFP    -- mod_hash
        skipFP    -- flag_hash
        skipFP    -- opt_hash
        skipFP    -- hpc_hash
        skipFP    -- plugin_hash
        skipBool  -- orphan
        skipBool  -- hasFamInsts
        ddeps  <- with "Dependencies:" $ withBlockPrefix getDependencies
        dusage <- with "Usage:" $ withBlockPrefix getFileUsage
        pure (Interface ddeps dusage)
 where
  since v = when (version >= v)

  getFastString = getCachedBS d

  -- Like 'getFastString' but we discard the information.
  skipFastString :: Get ()
  skipFastString = void getFastString

  getModule :: Get Module
  getModule = do
    idType <- with "Unit type:" getWord8
    case idType of
      0 -> skipFastString
      1 -> do
        skipFastString
        skipList (getTuple getFastString getModule)
      _ -> fail $ "Invalid unit type: " <> show idType
    Module <$> getFastString

  -- See `instance Binary Dependencies` in module GHC.Unit.Module.Deps.
  getDependencies =
    if 
      | version >= V9041 -> do
          -- warning: transitive dependencies are no longer stored, only direct 
          -- imports!
          -- Modules are now prefixed with their UnitId (should have been
          -- ModuleWithIsBoot ...)
          direct_mods <- with "direct_mods:" $
            if 
              | version >= V9140 -> getList $ do
                  skipIfaceImportLevel
                  skipFastString
                  getTuple getFastString getBool
              | otherwise -> getList $ do
                  skipFastString
                  getTuple getFastString getBool
          direct_pkgs <-
            if
              | version >= V9140 -> getList $ do
                  skipIfaceImportLevel
                  getFastString
              | otherwise -> getList getFastString
  
          -- plugin packages are now stored separately
          plugin_pkgs <- getList getFastString
          let all_pkgs = unList plugin_pkgs ++ unList direct_pkgs
  
          -- instead of a trust bool for each unit, we have an additional
          -- list of trusted units (transitive)
          trusted_pkgs <- getList getFastString
          let trusted u = u `elem` unList trusted_pkgs
              all_pkgs_trust = List (zip all_pkgs (map trusted all_pkgs))
  
          -- these are new
          skipList getModule -- sig_mods
          skipList $ do -- boot_mods
            skipFastString
            getTuple skipFastString skipBool
  
          dep_orphs  <- getList getModule
          dep_finsts <- getList getModule
  
          -- plugin names are no longer stored here
          let dep_plgins = List []
  
          pure Dependencies
            { dmods    = direct_mods
            , dpkgs    = all_pkgs_trust
            , dorphs   = dep_orphs
            , dfinsts  = dep_finsts
            , dplugins = dep_plgins
            }
      | otherwise -> do
          dep_mods   <- getList (getTuple getFastString getBool)
          dep_pkgs   <- getList (getTuple getFastString getBool)
          dep_orphs  <- getList getModule
          dep_finsts <- getList getModule
          dep_plgins <- getList getFastString
          pure Dependencies
            { dmods    = dep_mods
            , dpkgs    = dep_pkgs
            , dorphs   = dep_orphs
            , dfinsts  = dep_finsts
            , dplugins = dep_plgins
            }

  -- See `newtype IfaceImportLevel` and 
  -- `deriving Binary via EnumBinary ImportLevel` in module 
  -- GHC.Unit.Module.Deps. We discard this information.
  skipIfaceImportLevel :: Get ()
  skipIfaceImportLevel = skipImportLevel

  -- See `data ImportLevel` and 
  -- `deriving via (EnumBinary ImportLevel) instance Binary ImportLevel` in
  -- module GHC.Types.Basic. We discard this information.
  skipImportLevel :: Get ()
  skipImportLevel = void getInt64be

  -- See `data Usage` and `instance Binary Usage` in module 
  -- GHC.Module.Unit.Deps. We discard most of the information, except about the
  -- usage of files.
  getFileUsage = List . catMaybes . unList <$> getList go
   where
    go :: Get (Maybe Usage)
    go = do
      usageType <- with "Usage type:" getWord8
      case usageType of
        0 -> do
          skipWith "Module:" getModule -- usg_mod
          skipFP                       -- usg_mod_hash
          skipBool                     -- usg_safe
          pure Nothing

        1 -> do
          skipWith "Home module:" getFastString -- usg_mod_name
          since V9045 $ void getFastString      -- usg_unit_id
          skipFP                                -- usg_mod_hash
          if                                    -- usg_exports
            | version < V9140 -> skipMaybe skipFP
            | otherwise -> skipMaybe skipHomeModImport
          skipEntitiesList                      -- usg_entities
          skipBool                              -- usg_safe
          pure Nothing

        2 -> do
          -- usg_file_path
          file_path  <- with "File:" $
            if 
              | version >= V9081 -> Text.unpack . Text.decodeUtf8 <$> getFastString
              | otherwise -> getString
          skipWith "FP:" getFP                  -- usg_file_hash
          since V9041 $ skipWith "File label:" (getMaybe getString)-- usg_file_label
          pure (Just (Usage file_path))

        3 -> do
          void getModule -- usg_mod
          skipFP         -- usg_mod_hash
          pure Nothing

        4 | version >= V9041 -> do -- UsageHomeModuleInterface
          skipFastString             -- usg_mod_name
          since V9045 skipFastString -- usg_unit_id
          skipFP                     -- usg_iface_hash
          pure Nothing

        _ -> fail $ "Invalid usageType: " <> show usageType

  -- See `data HomeModImport` and `instance Binary HomeModImport` in module
  -- GHC.Unit.Module.Deps. We discard the information.
  skipHomeModImport :: Get ()
  skipHomeModImport = do
    skipFP
    skipHomeModImportedAvails

  -- See `data HomeModImportedAvails` and
  -- `instance Binary HomeModImportedAvails` in module GHC.Unit.Module.Deps. We 
  -- discard the information.
  skipHomeModImportedAvails :: Get ()
  skipHomeModImportedAvails = do
    homeModImportedAvailsType <- with "HomeModImportedAvails:" getWord8
    case homeModImportedAvailsType of
      0 -> do
        skipDetOrdAvails
        skipList skipName
      1 -> skipFP
      _ -> fail $ "Invalid HomeModImportedAvails type: " <> show homeModImportedAvailsType

  -- See `newtype DetOrdAvails` in module GHC.Types.Avail. We discard the
  -- information.
  skipDetOrdAvails :: Get ()
  skipDetOrdAvails = skipList skipAvailInfo

  -- See `instance Binary AvailInfo` in module GHC.Types.Avail. We discard the
  -- information.
  skipAvailInfo :: Get ()
  skipAvailInfo = do
    availInfoType <- with "AvailInfo type:" getWord8
    case availInfoType of
      0 -> skipName
      1 -> do
        skipName
        skipList skipName
      _ -> fail $ "Invalid AvailInfo type: " <> show availInfoType

  -- See `instance Binary Name` in module GHC.Types.Name. We discard the
  -- information.
  skipName :: Get ()
  skipName = void getInt64be

  -- See `data Usage` and `UsageHomeModule` in module GHC.Unit.Module.Deps. We
  -- discard the information.
  skipEntitiesList :: Get ()
  skipEntitiesList = skipList (getTuple skipOccName skipFP)

  -- See `data OccName` and `instance Binary OccName` in module 
  -- GHC.Types.Name.Occurrence. We discard the information.
  skipOccName :: Get ()
  skipOccName = do
    skipNameSpace
    skipFastString

  -- See `instance Binary NameSpace` in module GHC.Types.Name.Occurrence. We
  -- discard the information.
  skipNameSpace :: Get ()
  skipNameSpace =
    if 
      | version >= V9081 -> do
          nameSpaceType <- getWord8
          case nameSpaceType of
            0 -> pure ()
            1 -> pure ()
            2 -> pure ()
            3 -> pure ()
            4 -> skipFastString
            -- Unlike the original, we test that the byte we have obtained is
            -- valid.
            _ -> fail $ "Invalid NameSpace type: " <> show nameSpaceType
      | otherwise -> skipWord8

  -- See `instance Binary IfaceModInfo` in module GHC.Unit.Module.ModIface. We
  -- discard the information.
  skipIfaceModInfo :: Get ()
  skipIfaceModInfo = do
    skipWith "Module:" getModule         -- mi_mod_info_module
    skipWith "Sig:" $ getMaybe getModule -- mi_mod_info_sig_of
    skipWord8                            -- mi_mod_info_hsc_src

  -- See `data IfaceSelfRecomp` and `instance Binary IfaceSelfRecomp` in module
  -- GHC.Iface.Recomp.Types. We discard most of this information.
  getIfaceSelfRecompInfo :: Get (List Usage)
  getIfaceSelfRecompInfo = do
    skipFP -- sr_src_hash
    dusage <- with "Usage:" (withBlockPrefix getFileUsage) -- sr_usages
    skipFP -- sr_flag_hash
    skipFP -- sr_opt_hash
    skipFP -- sr_hpc_hash
    skipFP -- sr_plugin_hash
    pure dusage

getInterface :: Get Interface
getInterface = do
  let enableLEB128 = modify (\c -> c { useLEB128 = True})
      -- read a relative bin pointer
      getRelPtr = do
        c <- bytesRead
        p <- getPtr
        pure (fromIntegral c + p)

  magic <- lookAhead getWord32be >>= \case
    -- normal magic
    0x1face      -> getWord32be
    0x1face64    -> getWord32be
    m            -> do
      -- GHC 8.10 mistakenly encoded header fields with LEB128
      -- so it gets special treatment
      lookAhead (enableLEB128 >> getWord32be) >>= \case
        0x1face      -> enableLEB128 >> getWord32be
        0x1face64    -> enableLEB128 >> getWord32be
        _            -> fail $ "Invalid magic: " <> showHex m ""

  traceGet ("Magic: " ++ showHex magic "")

  -- empty field (removed in 9.0...)
  case magic of
    0x1face      -> do
      e <- lookAhead getWord32be
      if e == 0
        then void getWord32be
        else enableLEB128 -- > 9.0
    0x1face64    -> do
      e <- lookAhead getWord64be
      if e == 0
        then void getWord64be
        else enableLEB128 -- > 9.0
    _            -> return ()

  -- ghc version
  version <- getString
  traceGet ("Version: " ++ version)

  let !ifaceVersion
        | version >= "9140" = V9140 -- Support GHC 9.14.1-alpha1
        | version >= "9121" = V9121
        | version >= "9081" = V9081
        | version >= "9045" = V9045
        | version >= "9041" = V9041
        | version >= "9001" = V9001
        | version >= "8101" = V8101
        | version >= "8061" = V8061
        | version >= "8041" = V8041
        | version >= "8021" = V8021
        | version >= "8001" = V8001
        | version >= "7081" = V7081
        | version >= "7061" = V7061
        | version >= "7041" = V7041
        | version >= "7021" = V7021
        | otherwise         = error $ "Unsupported version: " <> version

  -- way
  way <- getString
  traceGet ("Ways: " ++ show way)

  -- source hash (GHC >= 9.4 && GHC < 9.14)
  when (ifaceVersion >= V9041 && ifaceVersion < V9140) skipFP

  -- extensible fields (GHC >= 9.0)
  when (ifaceVersion >= V9001) skipPtr

  -- dict_ptr
  dictPtr <- if ifaceVersion >= V9121 -- 9.12 uses relative pointers
    then getRelPtr
    else getPtr
  traceGet ("Dict ptr: " ++ show dictPtr)

  -- dict
  dict <- lookAhead $ getDictionary $ fromIntegral dictPtr

  -- symtable_ptr
  skipPtr

  -- IfaceType table
  when (ifaceVersion >= V9121) skipPtr

  case ifaceVersion of
    V9140 -> getInterfaceRecent ifaceVersion dict
    V9121 -> getInterfaceRecent ifaceVersion dict
    V9081 -> getInterfaceRecent ifaceVersion dict
    V9045 -> getInterfaceRecent ifaceVersion dict
    V9041 -> getInterfaceRecent ifaceVersion dict
    V9001 -> getInterfaceRecent ifaceVersion dict
    V8101 -> getInterfaceRecent ifaceVersion dict
    V8061 -> getInterface861 dict
    V8041 -> getInterface841 dict
    V8021 -> getInterface821 dict
    V8001 -> getInterface801 dict
    V7081 -> getInterface781 dict
    V7061 -> getInterface761 dict
    V7041 -> getInterface741 dict
    V7021 -> getInterface721 dict

fromFile :: FilePath -> IO (Either String Interface)
fromFile fp = withBinaryFile fp ReadMode go
 where
  go h =
    let feed (G.Done _ _ iface) = pure $ Right iface
        feed (G.Fail _ _ msg) = pure $ Left msg
        feed (G.Partial k) = do
          chunk <- hGetSome h defaultChunkSize
          feed $ k $ if B.null chunk then Nothing else Just chunk
    in  feed $ runGetIncremental getInterface

getULEB128 :: forall a. (Integral a, FiniteBits a) => Get a
getULEB128 =
  go 0 0
 where
  go :: Int -> a -> Get a
  go shift w = do
    b <- getWord8
    let !hasMore = testBit b 7
        !val = w .|. (clearBit (fromIntegral b) 7 `unsafeShiftL` shift) :: a
    if hasMore
      then do
        go (shift+7) val
      else
        return $! val

getSLEB128 :: forall a. (Integral a, FiniteBits a) => Get a
getSLEB128 = do
  (val,shift,signed) <- go 0 0
  if signed && (shift < finiteBitSize val )
    then return $! ((complement 0 `unsafeShiftL` shift) .|. val)
    else return val
 where
  go :: Int -> a -> Get (a,Int,Bool)
  go shift val = do
    byte <- getWord8
    let !byteVal = fromIntegral (clearBit byte 7) :: a
        !val' = val .|. (byteVal `unsafeShiftL` shift)
        !more = testBit byte 7
        !shift' = shift+7
    if more
      then go shift' val'
      else do
        let !signed = testBit byte 6
        return (val',shift',signed)
