module System.Path.Internal.PartClass where

import qualified System.Path.Internal.Component as PC
import qualified System.Path.Internal.Part as Part
import System.Path.Internal.System (System(..))
import System.Path.Internal.Component
        (Component(Component), GenComponent)

import Data.Monoid (Endo(Endo), appEndo)
import Data.Ord.HT (comparing)
import Data.Eq.HT (equating)



------------------------------------------------------------------------
-- Type classes and machinery for switching on Part.Abs/Part.Rel and Part.File/Part.Dir

-- | This class provides a way to prevent other modules
--   from making further 'AbsOrRel' or 'FileOrDir'
--   instances
class Private p
instance Private Part.Abs
instance Private Part.Rel
instance Private Part.AbsRel
instance Private Part.File
instance Private Part.Dir
instance Private Part.FileDir


-- | This class allows selective behaviour for absolute and
--   relative paths and is mostly for internal use.
class Private ar => AbsRel ar where
    {- |
    See <https://wiki.haskell.org/Closed_world_instances>
    for the used technique.
    -}
    switchAbsRel :: f Part.Abs -> f Part.Rel -> f Part.AbsRel -> f ar

instance AbsRel Part.Abs    where switchAbsRel f _ _ = f
instance AbsRel Part.Rel    where switchAbsRel _ f _ = f
instance AbsRel Part.AbsRel where switchAbsRel _ _ f = f

class AbsRel ar => AbsOrRel ar where
    switchAbsOrRel :: f Part.Abs -> f Part.Rel -> f ar

instance AbsOrRel Part.Abs where switchAbsOrRel f _ = f
instance AbsOrRel Part.Rel where switchAbsOrRel _ f = f


class AbsOrRel ar => Abs ar where switchAbs :: f Part.Abs -> f ar
instance Abs Part.Abs where switchAbs = id

class AbsOrRel ar => Rel ar where switchRel :: f Part.Rel -> f ar
instance Rel Part.Rel where switchRel = id

relVar :: Rel ar => ar
relVar = unwrapAbsRel $ switchRel $ WrapAbsRel Part.Rel


-- | This class allows selective behaviour for file and
--   directory paths and is mostly for internal use.
class Private fd => FileDir fd where
    switchFileDir :: f Part.File -> f Part.Dir -> f Part.FileDir -> f fd

instance FileDir Part.File    where switchFileDir f _ _ = f
instance FileDir Part.Dir     where switchFileDir _ f _ = f
instance FileDir Part.FileDir where switchFileDir _ _ f = f

class FileDir fd => FileOrDir fd where
    switchFileOrDir :: f Part.File -> f Part.Dir -> f fd

instance FileOrDir Part.File where switchFileOrDir f _ = f
instance FileOrDir Part.Dir  where switchFileOrDir _ f = f


class FileOrDir fd => File fd where switchFile :: f Part.File -> f fd
instance File Part.File where switchFile = id

class FileOrDir fd => Dir fd where switchDir :: f Part.Dir -> f fd
instance Dir Part.Dir where switchDir = id

dirVar :: Dir fd => fd
dirVar = unwrapFileDir $ switchDir $ WrapFileDir Part.Dir


newtype FuncArg b a = FuncArg {runFuncArg :: a -> b}

withAbsRel :: (AbsRel ar) => (String -> a) -> a -> ar -> a
withAbsRel fAbs fRel =
    runFuncArg $
    switchAbsRel
        (FuncArg $ \(Part.Abs (Component drive)) -> fAbs drive)
        (FuncArg $ \Part.Rel -> fRel)
        (FuncArg $ \ar ->
            case ar of
                Part.AbsO (Component drive) -> fAbs drive
                Part.RelO -> fRel)

withFileDir :: (FileDir fd) => (GenComponent -> a) -> a -> a -> fd -> a
withFileDir fFile fDir fFileOrDir =
    runFuncArg $
    switchFileDir (FuncArg $ \(Part.File pc) -> fFile pc)
        (FuncArg $ \Part.Dir -> fDir) (FuncArg $ \Part.FileDir -> fFileOrDir)

withFileOrDir :: (FileOrDir fd) => (GenComponent -> a) -> a -> fd -> a
withFileOrDir fFile fDir =
    runFuncArg $
    switchFileOrDir
        (FuncArg $ \(Part.File pc) -> fFile pc)
        (FuncArg $ \Part.Dir -> fDir)



isAbsolute :: (AbsRel ar) => ar -> Bool
isAbsolute = withAbsRel (const True) False

toAbsRel :: AbsRel ar => ar -> Part.AbsRel
toAbsRel =
    runFuncArg $
    switchAbsRel
        (FuncArg $ \(Part.Abs drive) -> Part.AbsO drive)
        (FuncArg $ \Part.Rel -> Part.RelO)
        (FuncArg id)

fromAbsRel :: AbsRel ar => Part.AbsRel -> Maybe ar
fromAbsRel ar =
    case ar of
        Part.AbsO pc -> switchAbsRel (Just $ Part.Abs pc) Nothing (Just ar)
        Part.RelO -> switchAbsRel Nothing (Just Part.Rel) (Just ar)

fdMap :: (FileDir fd) => (String -> String) -> fd -> fd
fdMap f = appEndo $ switchFileDir (Endo $ Part.fileMap f) (Endo id) (Endo id)


newtype WrapAbsRel os ar = WrapAbsRel {unwrapAbsRel :: ar}

inspectAbsRel ::
    (AbsRel ar) => WrapAbsRel os ar -> Either (Component os) ()
inspectAbsRel =
    withAbsRel (Left . Component) (Right ()) . unwrapAbsRel

instance (System os, AbsRel ar) => Eq (WrapAbsRel os ar) where
    (==) = equating inspectAbsRel

instance (System os, AbsRel ar) => Ord (WrapAbsRel os ar) where
    compare = comparing inspectAbsRel


newtype WrapFileDir os fd = WrapFileDir {unwrapFileDir :: fd}

inspectFileDir ::
    (FileDir ar) => WrapFileDir os ar -> Either (Component os) ()
inspectFileDir =
    withFileDir (Left . PC.retag) (Right ()) (Right ()) . unwrapFileDir

instance (System os, FileDir fd) => Eq (WrapFileDir os fd) where
    (==) = equating inspectFileDir

instance (System os, FileDir fd) => Ord (WrapFileDir os fd) where
    compare = comparing inspectFileDir
