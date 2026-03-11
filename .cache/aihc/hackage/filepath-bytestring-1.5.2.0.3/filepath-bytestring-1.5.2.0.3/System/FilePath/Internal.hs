{-# LANGUAGE OverloadedStrings #-}

-- This template expects CPP definitions for:
--     MODULE_NAME = Posix | Windows
--     IS_WINDOWS  = False | True

-- |
-- Module      :  System.FilePath.MODULE_NAME.ByteString
-- Copyright   :  (c) Neil Mitchell 2005-2014, (c) Joey Hess 2019
-- License     :  BSD3
--
-- Maintainer  :  id@joeyh.name
-- Stability   :  stable
-- Portability :  portable
--
-- A library for 'RawFilePath' manipulations, using MODULE_NAME style paths on
-- all platforms. Importing "System.FilePath.ByteString" is usually better.
--
-- This module is the same as System.FilePath.MODULE_NAME from the filepath
-- library, except it uses 'RawFilePath'.
--
-- Given the example 'RawFilePath': @\/directory\/file.ext@
--
-- We can use the following functions to extract pieces.
--
-- * 'takeFileName' gives @\"file.ext\"@
--
-- * 'takeDirectory' gives @\"\/directory\"@
--
-- * 'takeExtension' gives @\".ext\"@
--
-- * 'dropExtension' gives @\"\/directory\/file\"@
--
-- * 'takeBaseName' gives @\"file\"@
--
-- And we could have built an equivalent path with the following expressions:
--
-- * @\"\/directory\" '</>' \"file.ext\"@.
--
-- * @\"\/directory\/file" '<.>' \"ext\"@.
--
-- * @\"\/directory\/file.txt" '-<.>' \"ext\"@.
--
-- Each function in this module is documented with several examples,
-- which are also used as tests.
--
-- Here are a few examples of using the @filepath@ functions together:
--
-- /Example 1:/ Find the possible locations of a Haskell module @Test@ imported from module @Main@:
--
-- @['replaceFileName' path_to_main \"Test\" '<.>' ext | ext <- [\"hs\",\"lhs\"] ]@
--
-- /Example 2:/ Compile a Haskell file, putting the @.hi@ file under @interface@:
--
-- @'takeDirectory' file '</>' \"interface\" '</>' ('takeFileName' file '-<.>' \"hi\")@
--
-- References:
-- [1] <http://msdn.microsoft.com/en-us/library/windows/desktop/aa365247.aspx Naming Files, Paths and Namespaces> (Microsoft MSDN)
module System.FilePath.MODULE_NAME.ByteString
    (
    -- * Types
    RawFilePath,
    -- * Filename encoding
    -- 
    -- | When using `FilePath`, you do not usually need to care about how
    -- it is encoded, because it is a @[Char]@ and encoding and decoding is
    -- handled by IO actions as needed. Unfortunately the situation is more
    -- complicated when using `RawFilePath`.
    --
    -- It's natural to enable `OverloadedStrings` and use it to construct
    -- a `RawFilePath`, eg @"foo" '</>' "bar"@. A gotcha though is that
    -- any non-ascii characters will be truncated to 8 bits. That is not a
    -- limitation of this library, but of the `IsString` implementation 
    -- of `ByteString`.
    --
    -- Posix filenames do not have any defined encoding. This library
    -- assumes that whatever encoding may be used for a `RawFilePath`,
    -- it is compatable with ASCII. In particular, 0x2F (/) is always
    -- a path separator, and 0x2E (.) is assumed to be an extension
    -- separator. All encodings in common use are compatible with ASCII,
    -- and unix tools have always made similar assumptions,
    -- so this is unlikely to be a problem, unless you are dealing with
    -- EBCDIC or similar historical oddities.
    --
    -- Windows's API expects filenames to be encoded with UTF-16.
    -- This is especially problimatic when using OverloadedStrings
    -- since a ByteString "bar" is not a valid encoding for a
    -- Windows filename (but "b\\0a\\0r\\0" is). To avoid this problem,
    -- and to simplify the implementation,
    -- `RawFilePath` is assumed to be encoded with UTF-8 (not UTF-16)
    -- when this library is used on Windows.
    -- There are not currently any libraries for Windows that use
    -- `RawFilePath`, so you will probably need to convert them back to 
    -- `FilePath` in order to do IO in any case.
    encodeFilePath,
    decodeFilePath,
    -- * Separator predicates
    pathSeparator, pathSeparators, isPathSeparator,
    searchPathSeparator, isSearchPathSeparator,
    extSeparator, isExtSeparator,

    -- * @$PATH@ methods
    splitSearchPath, getSearchPath,

    -- * Extension functions
    splitExtension,
    takeExtension, replaceExtension, (-<.>), dropExtension, addExtension, hasExtension, (<.>),
    splitExtensions, dropExtensions, takeExtensions, replaceExtensions, isExtensionOf,
    stripExtension,

    -- * Filename\/directory functions
    splitFileName,
    takeFileName, replaceFileName, dropFileName,
    takeBaseName, replaceBaseName,
    takeDirectory, replaceDirectory,
    combine, (</>),
    splitPath, joinPath, splitDirectories,

    -- * Drive functions
    splitDrive, joinDrive,
    takeDrive, hasDrive, dropDrive, isDrive,

    -- * Trailing slash functions
    hasTrailingPathSeparator,
    addTrailingPathSeparator,
    dropTrailingPathSeparator,

    -- * File name manipulations
    normalise, equalFilePath,
    makeRelative,
    isRelative, isAbsolute,
    isValid, makeValid
    )
    where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Char(ord, chr, toUpper, toLower, isAsciiLower, isAsciiUpper)
import Data.Maybe(isJust)
import Data.Word(Word8)
#ifdef mingw32_HOST_OS
import qualified Data.ByteString.UTF8 as UTF8
#else
import System.IO.Unsafe (unsafePerformIO)
#endif
import System.Environment (getEnv)
import Data.Semigroup
import Prelude

#ifndef mingw32_HOST_OS
-- Import from unix, rather than redefining, so users who import both
-- will not get an ambiguous occurance of RawFilePath.
import System.Posix.ByteString(RawFilePath)
#else
-- | A literal file path
type RawFilePath = ByteString
#endif


infixr 7  <.>, -<.>
infixr 5  </>


-- | Convert from FilePath to RawFilePath.
--
-- When run on Unix, this applies the filesystem encoding
-- (see `Encoding.getFileSystemEncoding`).
--
-- When run on Windows, this encodes as UTF-8.
-- 
-- the implementation of this function assumes that the
-- filesystem encoding will not be changed while the program is running.
encodeFilePath :: FilePath -> RawFilePath
#ifdef mingw32_HOST_OS
encodeFilePath = UTF8.fromString
#else
encodeFilePath = unsafePerformIO . B.fromFilePath
#endif

-- | Convert from RawFilePath to FilePath
--
-- When run on Unix, this applies the filesystem encoding
-- (see `Encoding.getFileSystemEncoding`).
--
-- When run on Windows, this decodes UTF-8.

-- The implementation of this function assumes that the
-- filesystem encoding will not be changed while the program is running.
decodeFilePath :: RawFilePath -> FilePath
#ifdef mingw32_HOST_OS
decodeFilePath = UTF8.toString
#else
{-# NOINLINE decodeFilePath #-}
decodeFilePath = unsafePerformIO . B.toFilePath
#endif

---------------------------------------------------------------------
-- Platform Abstraction Methods (private)

-- | Is the operating system Unix or Linux like
isPosix :: Bool
isPosix = not isWindows

-- | Is the operating system Windows like
isWindows :: Bool
isWindows = IS_WINDOWS


---------------------------------------------------------------------
-- The basic functions

-- | The character that separates directories. In the case where more than
--   one character is possible, 'pathSeparator' is the \'ideal\' one.
--
-- > Windows: pathSeparator == fromIntegral (ord '\\')
-- > Posix:   pathSeparator == fromIntegral (ord '/')
-- > isPathSeparator pathSeparator
pathSeparator :: Word8
pathSeparator = if isWindows then fromIntegral (ord '\\') else fromIntegral (ord '/')

-- | The list of all possible separators.
--
-- > Windows: pathSeparators == [fromIntegral (ord '\\'), fromIntegral (ord '/')]
-- > Posix:   pathSeparators == [fromIntegral (ord '/')]
-- > pathSeparator `elem` pathSeparators
pathSeparators :: [Word8]
pathSeparators = if isWindows then [fromIntegral (ord '\\'), fromIntegral (ord '/')] else [fromIntegral (ord '/')]

-- | Rather than using @(== 'pathSeparator')@, use this. Test if something
--   is a path separator.
--
-- > isPathSeparator a == (a `elem` pathSeparators)
isPathSeparator :: Word8 -> Bool
isPathSeparator 47 = True
isPathSeparator 92 = isWindows
isPathSeparator _ = False


-- | The character that is used to separate the entries in the $PATH environment variable.
--
-- > Windows: searchPathSeparator == fromIntegral (ord ';')
-- > Posix:   searchPathSeparator == fromIntegral (ord ':')
searchPathSeparator :: Word8
searchPathSeparator = if isWindows then fromIntegral (ord ';') else fromIntegral (ord ':')

-- | Is the character a file separator?
--
-- > isSearchPathSeparator a == (a == searchPathSeparator)
isSearchPathSeparator :: Word8 -> Bool
isSearchPathSeparator = (== searchPathSeparator)


-- | File extension character
--
-- > extSeparator == fromIntegral (ord '.')
extSeparator :: Word8
extSeparator = fromIntegral (ord '.')

-- | Is the character an extension character?
--
-- > isExtSeparator a == (a == extSeparator)
isExtSeparator :: Word8 -> Bool
isExtSeparator = (== extSeparator)


---------------------------------------------------------------------
-- Path methods (environment $PATH)

-- | Take a string, split it on the 'searchPathSeparator' character.
--   Blank items are ignored on Windows, and converted to @.@ on Posix.
--   On Windows path elements are stripped of quotes.
--
--   Follows the recommendations in
--   <http://www.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap08.html>
--
-- > Posix:   splitSearchPath "File1:File2:File3"  == ["File1","File2","File3"]
-- > Posix:   splitSearchPath "File1::File2:File3" == ["File1",".","File2","File3"]
-- > Windows: splitSearchPath "File1;File2;File3"  == ["File1","File2","File3"]
-- > Windows: splitSearchPath "File1;;File2;File3" == ["File1","File2","File3"]
-- > Windows: splitSearchPath "File1;\"File2\";File3" == ["File1","File2","File3"]
splitSearchPath :: ByteString -> [RawFilePath]
splitSearchPath = f
  where
	f x = case B.break isSearchPathSeparator x of
		(pre, post)
			| B.null post -> g pre
			| otherwise -> g pre ++ f (B.drop 1 post)

	g "" = ["." | isPosix]
	g x
		| isWindows = case B.uncons x of
			Just (q, x') | q == quote ->
				case B.unsnoc x' of
					Just (x'', q') | q' == quote -> [x'']
					_ -> [x]
			_  -> [x]
		| otherwise = [x]
	
	quote = fromIntegral (ord '"')


-- | Get a list of 'RawFilePath's in the $PATH variable.
getSearchPath :: IO [RawFilePath]
getSearchPath = fmap (splitSearchPath . encodeFilePath) (getEnv "PATH")


---------------------------------------------------------------------
-- Extension methods

-- | Split on the extension. 'addExtension' is the inverse.
--
-- > splitExtension "/directory/path.ext" == ("/directory/path",".ext")
-- > uncurry (<>) (splitExtension x) == x
-- > Valid x => uncurry addExtension (splitExtension x) == x
-- > splitExtension "file.txt" == ("file",".txt")
-- > splitExtension "file" == ("file","")
-- > splitExtension "file/file.txt" == ("file/file",".txt")
-- > splitExtension "file.txt/boris" == ("file.txt/boris","")
-- > splitExtension "file.txt/boris.ext" == ("file.txt/boris",".ext")
-- > splitExtension "file/path.txt.bob.fred" == ("file/path.txt.bob",".fred")
-- > splitExtension "file/path.txt/" == ("file/path.txt/","")
splitExtension :: RawFilePath -> (ByteString, ByteString)
splitExtension x = if B.null nameDot
	then (x,mempty)
	else (dir <> B.init nameDot, extSeparator `B.cons` ext)
  where
	(dir,file) = splitFileName_ x
	(nameDot,ext) = B.breakEnd isExtSeparator file

-- | Get the extension of a file, returns @\"\"@ for no extension, @.ext@ otherwise.
--
-- > takeExtension "/directory/path.ext" == ".ext"
-- > takeExtension x == snd (splitExtension x)
-- > Valid x => takeExtension (addExtension x "ext") == ".ext"
-- > Valid x => takeExtension (replaceExtension x "ext") == ".ext"
takeExtension :: RawFilePath -> ByteString
takeExtension = snd . splitExtension

-- | Remove the current extension and add another, equivalent to 'replaceExtension'.
--
-- > "/directory/path.txt" -<.> "ext" == "/directory/path.ext"
-- > "/directory/path.txt" -<.> ".ext" == "/directory/path.ext"
-- > "foo.o" -<.> "c" == "foo.c"
(-<.>) :: RawFilePath -> ByteString -> RawFilePath
(-<.>) = replaceExtension

-- | Set the extension of a file, overwriting one if already present, equivalent to '-<.>'.
--
-- > replaceExtension "/directory/path.txt" "ext" == "/directory/path.ext"
-- > replaceExtension "/directory/path.txt" ".ext" == "/directory/path.ext"
-- > replaceExtension "file.txt" ".bob" == "file.bob"
-- > replaceExtension "file.txt" "bob" == "file.bob"
-- > replaceExtension "file" ".bob" == "file.bob"
-- > replaceExtension "file.txt" "" == "file"
-- > replaceExtension "file.fred.bob" "txt" == "file.fred.txt"
-- > replaceExtension x y == addExtension (dropExtension x) y
replaceExtension :: RawFilePath -> ByteString -> RawFilePath
replaceExtension x y = dropExtension x <.> y

-- | Add an extension, even if there is already one there, equivalent to 'addExtension'.
--
-- > "/directory/path" <.> "ext" == "/directory/path.ext"
-- > "/directory/path" <.> ".ext" == "/directory/path.ext"
(<.>) :: RawFilePath -> ByteString -> RawFilePath
(<.>) = addExtension

-- | Remove last extension, and the \".\" preceding it.
--
-- > dropExtension "/directory/path.ext" == "/directory/path"
-- > dropExtension x == fst (splitExtension x)
dropExtension :: RawFilePath -> RawFilePath
dropExtension = fst . splitExtension

-- | Add an extension, even if there is already one there, equivalent to '<.>'.
--
-- > addExtension "/directory/path" "ext" == "/directory/path.ext"
-- > addExtension "file.txt" "bib" == "file.txt.bib"
-- > addExtension "file." ".bib" == "file..bib"
-- > addExtension "file" ".bib" == "file.bib"
-- > addExtension "/" "x" == "/.x"
-- > addExtension x "" == x
-- > Valid x => takeFileName (addExtension (addTrailingPathSeparator x) "ext") == ".ext"
-- > Windows: addExtension "\\\\share" ".txt" == "\\\\share\\.txt"
addExtension :: RawFilePath -> ByteString -> RawFilePath
addExtension file ext = case B.uncons ext of
	Nothing -> file
	Just (x,_xs) -> joinDrive a $
		if isExtSeparator x
			then b <> ext
			else b <> (extSeparator `B.cons` ext)
  where
	(a,b) = splitDrive file

-- | Does the given filename have an extension?
--
-- > hasExtension "/directory/path.ext" == True
-- > hasExtension "/directory/path" == False
-- > null (takeExtension x) == not (hasExtension x)
hasExtension :: RawFilePath -> Bool
hasExtension = B.any isExtSeparator . takeFileName


-- | Does the given filename have the specified extension?
--
-- > "png" `isExtensionOf` "/directory/file.png" == True
-- > ".png" `isExtensionOf` "/directory/file.png" == True
-- > ".tar.gz" `isExtensionOf` "bar/foo.tar.gz" == True
-- > "ar.gz" `isExtensionOf` "bar/foo.tar.gz" == False
-- > "png" `isExtensionOf` "/directory/file.png.jpg" == False
-- > "csv/table.csv" `isExtensionOf` "/data/csv/table.csv" == False
isExtensionOf :: ByteString -> RawFilePath -> Bool
isExtensionOf ext = B.isSuffixOf ext' . takeExtensions
  where
	ext' = case B.uncons ext of
		Just (h, _) | isExtSeparator h -> ext
		_ -> extSeparator `B.cons` ext

-- | Drop the given extension from a FilePath, and the @\".\"@ preceding it.
--   Returns 'Nothing' if the FilePath does not have the given extension, or
--   'Just' and the part before the extension if it does.
--
--   This function can be more predictable than 'dropExtensions', especially if the filename
--   might itself contain @.@ characters.
--
-- > stripExtension "hs.o" "foo.x.hs.o" == Just "foo.x"
-- > stripExtension "hi.o" "foo.x.hs.o" == Nothing
-- > dropExtension x == fromJust (stripExtension (takeExtension x) x)
-- > dropExtensions x == fromJust (stripExtension (takeExtensions x) x)
-- > stripExtension ".c.d" "a.b.c.d"  == Just "a.b"
-- > stripExtension ".c.d" "a.b..c.d" == Just "a.b."
-- > stripExtension "baz"  "foo.bar"  == Nothing
-- > stripExtension "bar"  "foobar"   == Nothing
-- > stripExtension ""     x          == Just x
stripExtension :: ByteString -> RawFilePath -> Maybe RawFilePath
stripExtension ext path = case B.uncons ext of
	Nothing -> Just path
	Just (x, _) -> 
		let dotExt = if isExtSeparator x
			then ext
			else extSeparator `B.cons` ext
		in B.stripSuffix dotExt path


-- | Split on all extensions.
--
-- > splitExtensions "/directory/path.ext" == ("/directory/path",".ext")
-- > splitExtensions "file.tar.gz" == ("file",".tar.gz")
-- > uncurry (<>) (splitExtensions x) == x
-- > Valid x => uncurry addExtension (splitExtensions x) == x
-- > splitExtensions "file.tar.gz" == ("file",".tar.gz")
splitExtensions :: RawFilePath -> (RawFilePath, ByteString)
splitExtensions x = (a <> c, d)
  where
	(a,b) = splitFileName_ x
	(c,d) = B.break isExtSeparator b

-- | Drop all extensions.
--
-- > dropExtensions "/directory/path.ext" == "/directory/path"
-- > dropExtensions "file.tar.gz" == "file"
-- > not $ hasExtension $ dropExtensions x
-- > not $ any isExtSeparator $ takeFileName $ dropExtensions x
dropExtensions :: RawFilePath -> RawFilePath
dropExtensions = fst . splitExtensions

-- | Get all extensions.
--
-- > takeExtensions "/directory/path.ext" == ".ext"
-- > takeExtensions "file.tar.gz" == ".tar.gz"
takeExtensions :: RawFilePath -> ByteString
takeExtensions = snd . splitExtensions


-- | Replace all extensions of a file with a new extension. Note
--   that 'replaceExtension' and 'addExtension' both work for adding
--   multiple extensions, so only required when you need to drop
--   all extensions first.
--
-- > replaceExtensions "file.fred.bob" "txt" == "file.txt"
-- > replaceExtensions "file.fred.bob" "tar.gz" == "file.tar.gz"
replaceExtensions :: RawFilePath -> ByteString -> RawFilePath
replaceExtensions x y = dropExtensions x <.> y



---------------------------------------------------------------------
-- Drive methods

-- | Is the given character a valid drive letter?
-- only a-z and A-Z are letters, not isAlpha which is more unicodey
isLetter :: Word8 -> Bool
isLetter x = isAsciiLower c || isAsciiUpper c
  where c = chr (fromIntegral x)

-- | Split a path into a drive and a path.
--   On Posix, \/ is a Drive.
--
-- > uncurry (<>) (splitDrive x) == x
-- > Windows: splitDrive "file" == ("","file")
-- > Windows: splitDrive "c:/file" == ("c:/","file")
-- > Windows: splitDrive "c:\\file" == ("c:\\","file")
-- > Windows: splitDrive "\\\\shared\\test" == ("\\\\shared\\","test")
-- > Windows: splitDrive "\\\\shared" == ("\\\\shared","")
-- > Windows: splitDrive "\\\\?\\UNC\\shared\\file" == ("\\\\?\\UNC\\shared\\","file")
-- > Windows: splitDrive "\\\\?\\UNCshared\\file" == ("\\\\?\\","UNCshared\\file")
-- > Windows: splitDrive "\\\\?\\d:\\file" == ("\\\\?\\d:\\","file")
-- > Windows: splitDrive "/d" == ("","/d")
-- > Posix:   splitDrive "/test" == ("/","test")
-- > Posix:   splitDrive "//test" == ("//","test")
-- > Posix:   splitDrive "test/file" == ("","test/file")
-- > Posix:   splitDrive "file" == ("","file")
splitDrive :: RawFilePath -> (RawFilePath, RawFilePath)
splitDrive x | isPosix = B.span (== fromIntegral (ord '/')) x
splitDrive x | Just y <- readDriveLetter x = y
splitDrive x | Just y <- readDriveUNC x = y
splitDrive x | Just y <- readDriveShare x = y
splitDrive x = (B.empty,x)

addSlash :: RawFilePath -> RawFilePath -> (RawFilePath, RawFilePath)
addSlash a xs = (a <> c,d)
  where
	(c,d) = B.span isPathSeparator xs

-- See [1].
-- "\\?\D:\<path>" or "\\?\UNC\<server>\<share>"
readDriveUNC :: RawFilePath -> Maybe (RawFilePath, RawFilePath)
readDriveUNC p = do
	(s1, p') <- B.uncons p
	(s2, p'') <- B.uncons p'
	(q, p''') <- B.uncons p''
	(s3, rest) <- B.uncons p'''
	if q == fromIntegral (ord '?') && all isPathSeparator [s1,s2,s3]
		then case breakunc rest of
			Just (unc,r) -> 
				let (a,b) = readDriveShareName r
				in Just (B.pack [s1,s2,q,s3] <> unc <> a, b)
			Nothing -> case readDriveLetter rest of
				-- Extended-length path.
				Just (a,b) -> Just (B.pack [s1,s2,q,s3] <> a, b)
				Nothing -> Nothing
		else Nothing
  where
	breakunc b = do
		(u, b') <- B.uncons b
		(n, b'') <- B.uncons b'
		(c, b''') <- B.uncons b''
		(s, rest) <- B.uncons b'''
		let isup v ch = toUpper (chr (fromIntegral v)) == ch
		if isPathSeparator s && isup u 'U' && isup n 'N' && isup c 'C'
			then Just (B.take 4 b, rest)
			else Nothing

{- c:\ -}
readDriveLetter :: RawFilePath -> Maybe (RawFilePath, RawFilePath)
readDriveLetter p = case B.uncons p of
	Just (x, t) | isLetter x -> case B.uncons t of
		Just (c, t') | c == colon -> case B.uncons t' of
			Just (y, _) | isPathSeparator y ->
				Just $ addSlash (B.pack [x, colon]) t'
			_ -> Just (B.pack [x, colon], t')
		_ -> Nothing
	_ -> Nothing
  where
	colon = fromIntegral (ord ':')

{- \\sharename\ -}
readDriveShare :: ByteString -> Maybe (RawFilePath, RawFilePath)
readDriveShare p = do
	(s1, p') <- B.uncons p
	(s2, p'') <- B.uncons p'
	let (a,b) = readDriveShareName p''
	if isPathSeparator s1 && isPathSeparator s2
		then Just (s1 `B.cons` s2 `B.cons` a,b)
		else Nothing

{- assume you have already seen \\ -}
{- share\bob -> "share\", "bob" -}
readDriveShareName :: ByteString -> (RawFilePath, RawFilePath)
readDriveShareName name = addSlash a b
  where
	(a,b) = B.break isPathSeparator name


-- | Join a drive and the rest of the path.
--
-- > Valid x => uncurry joinDrive (splitDrive x) == x
-- > Windows: joinDrive "C:" "foo" == "C:foo"
-- > Windows: joinDrive "C:\\" "bar" == "C:\\bar"
-- > Windows: joinDrive "\\\\share" "foo" == "\\\\share\\foo"
-- > Windows: joinDrive "/:" "foo" == "/:\\foo"
joinDrive :: RawFilePath -> RawFilePath -> RawFilePath
joinDrive = combineAlways

-- | Get the drive from a filepath.
--
-- > takeDrive x == fst (splitDrive x)
takeDrive :: RawFilePath -> RawFilePath
takeDrive = fst . splitDrive

-- | Delete the drive, if it exists.
--
-- > dropDrive x == snd (splitDrive x)
dropDrive :: RawFilePath -> RawFilePath
dropDrive = snd . splitDrive

-- | Does a path have a drive.
--
-- > not (hasDrive x) == null (takeDrive x)
-- > Posix:   hasDrive "/foo" == True
-- > Windows: hasDrive "C:\\foo" == True
-- > Windows: hasDrive "C:foo" == True
-- >          hasDrive "foo" == False
-- >          hasDrive "" == False
hasDrive :: RawFilePath -> Bool
hasDrive = not . B.null . takeDrive

-- | Is an element a drive
--
-- > Posix:   isDrive "/" == True
-- > Posix:   isDrive "/foo" == False
-- > Windows: isDrive "C:\\" == True
-- > Windows: isDrive "C:\\foo" == False
-- >          isDrive "" == False
isDrive :: RawFilePath -> Bool
isDrive x = not (B.null x) && B.null (dropDrive x)

---------------------------------------------------------------------
-- Operations on a filepath, as a list of directories

-- | Split a filename into directory and file. '</>' is the inverse.
--   The first component will often end with a trailing slash.
--
-- > splitFileName "/directory/file.ext" == ("/directory/","file.ext")
-- > Valid x => uncurry (</>) (splitFileName x) == x || fst (splitFileName x) == "./"
-- > Valid x => isValid (fst (splitFileName x))
-- > splitFileName "file/bob.txt" == ("file/", "bob.txt")
-- > splitFileName "file/" == ("file/", "")
-- > splitFileName "bob" == ("./", "bob")
-- > Posix:   splitFileName "/" == ("/","")
-- > Windows: splitFileName "c:" == ("c:","")
splitFileName :: RawFilePath -> (ByteString, ByteString)
splitFileName x = (if B.null dir then "./" else dir, name)
  where
	(dir, name) = splitFileName_ x

-- version of splitFileName where, if the FilePath has no directory
-- component, the returned directory is "" rather than "./".  This
-- is used in cases where we are going to combine the returned
-- directory to make a valid FilePath, and having a "./" appear would
-- look strange and upset simple equality properties.  See
-- e.g. replaceFileName.
splitFileName_ :: RawFilePath -> (ByteString, ByteString)
splitFileName_ x = (drv <> dir, file)
  where
	(drv,pth) = splitDrive x
	(dir,file) = B.breakEnd isPathSeparator pth

-- | Set the filename.
--
-- > replaceFileName "/directory/other.txt" "file.ext" == "/directory/file.ext"
-- > Valid x => replaceFileName x (takeFileName x) == x
replaceFileName :: RawFilePath -> ByteString -> RawFilePath
replaceFileName x y = a </> y where (a,_) = splitFileName_ x

-- | Drop the filename. Unlike 'takeDirectory', this function will leave
--   a trailing path separator on the directory.
--
-- > dropFileName "/directory/file.ext" == "/directory/"
-- > dropFileName x == fst (splitFileName x)
dropFileName :: RawFilePath -> RawFilePath
dropFileName = fst . splitFileName

-- | Get the file name.
--
-- > takeFileName "/directory/file.ext" == "file.ext"
-- > takeFileName "test/" == ""
-- > takeFileName x `isSuffixOf` x
-- > takeFileName x == snd (splitFileName x)
-- > Valid x => takeFileName (replaceFileName x "fred") == "fred"
-- > Valid x => takeFileName (x </> "fred") == "fred"
-- > Valid x => isRelative (takeFileName x)
takeFileName :: RawFilePath -> RawFilePath
takeFileName = snd . splitFileName

-- | Get the base name, without an extension or path.
--
-- > takeBaseName "/directory/file.ext" == "file"
-- > takeBaseName "file/test.txt" == "test"
-- > takeBaseName "dave.ext" == "dave"
-- > takeBaseName "" == ""
-- > takeBaseName "test" == "test"
-- > takeBaseName (addTrailingPathSeparator x) == ""
-- > takeBaseName "file/file.tar.gz" == "file.tar"
takeBaseName :: RawFilePath -> ByteString
takeBaseName = dropExtension . takeFileName

-- | Set the base name.
--
-- > replaceBaseName "/directory/other.ext" "file" == "/directory/file.ext"
-- > replaceBaseName "file/test.txt" "bob" == "file/bob.txt"
-- > replaceBaseName "fred" "bill" == "bill"
-- > replaceBaseName "/dave/fred/bob.gz.tar" "new" == "/dave/fred/new.tar"
-- > Valid x => replaceBaseName x (takeBaseName x) == x
replaceBaseName :: RawFilePath -> ByteString -> RawFilePath
replaceBaseName pth nam = combineAlways a (nam <.> ext)
  where
	(a,b) = splitFileName_ pth
	ext = takeExtension b

-- | Is an item either a directory or the last character a path separator?
--
-- > hasTrailingPathSeparator "test" == False
-- > hasTrailingPathSeparator "test/" == True
hasTrailingPathSeparator :: RawFilePath -> Bool
hasTrailingPathSeparator x = case B.unsnoc x of
	Nothing -> False
	Just (_i, l) -> isPathSeparator l


hasLeadingPathSeparator :: RawFilePath -> Bool
hasLeadingPathSeparator x = case B.uncons x of
	Nothing -> False
	Just (h,_t) -> isPathSeparator h

-- | Add a trailing file path separator if one is not already present.
--
-- > hasTrailingPathSeparator (addTrailingPathSeparator x)
-- > hasTrailingPathSeparator x ==> addTrailingPathSeparator x == x
-- > Posix:    addTrailingPathSeparator "test/rest" == "test/rest/"
addTrailingPathSeparator :: RawFilePath -> RawFilePath
addTrailingPathSeparator x
	| hasTrailingPathSeparator x = x
	| otherwise = B.snoc x pathSeparator


-- | Remove any trailing path separators
--
-- > dropTrailingPathSeparator "file/test/" == "file/test"
-- >           dropTrailingPathSeparator "/" == "/"
-- > Windows:  dropTrailingPathSeparator "\\" == "\\"
-- > Posix:    not (hasTrailingPathSeparator (dropTrailingPathSeparator x)) || isDrive x
dropTrailingPathSeparator :: RawFilePath -> RawFilePath
dropTrailingPathSeparator x
	| hasTrailingPathSeparator x && not (isDrive x) = 
		let x' = dropWhileEnd isPathSeparator x
		in if B.null x' then B.singleton (B.last x) else x'
	| otherwise = x

-- | Get the directory name, move up one level.
--
-- >           takeDirectory "/directory/other.ext" == "/directory"
-- >           takeDirectory x `isPrefixOf` x || takeDirectory x == "."
-- >           takeDirectory "foo" == "."
-- >           takeDirectory "/" == "/"
-- >           takeDirectory "/foo" == "/"
-- >           takeDirectory "/foo/bar/baz" == "/foo/bar"
-- >           takeDirectory "/foo/bar/baz/" == "/foo/bar/baz"
-- >           takeDirectory "foo/bar/baz" == "foo/bar"
-- > Windows:  takeDirectory "foo\\bar" == "foo"
-- > Windows:  takeDirectory "foo\\bar\\\\" == "foo\\bar"
-- > Windows:  takeDirectory "C:\\" == "C:\\"
takeDirectory :: RawFilePath -> RawFilePath
takeDirectory = dropTrailingPathSeparator . dropFileName

-- | Set the directory, keeping the filename the same.
--
-- > replaceDirectory "root/file.ext" "/directory/" == "/directory/file.ext"
-- > Valid x => replaceDirectory x (takeDirectory x) `equalFilePath` x
replaceDirectory :: RawFilePath -> ByteString -> RawFilePath
replaceDirectory x dir = combineAlways dir (takeFileName x)

{-# INLINE combine #-}
-- | An alias for '</>'.
combine :: RawFilePath -> RawFilePath -> RawFilePath
combine a b
	| hasLeadingPathSeparator b || (not isPosix && hasDrive b) = b
	| otherwise = combineAlways a b

-- | Combine two paths, assuming rhs is NOT absolute.
combineAlways :: RawFilePath -> RawFilePath -> RawFilePath
combineAlways a b
	| B.null a = b
	| B.null b = a
	| hasTrailingPathSeparator a = a <> b
	| isWindows && B.length a == 2
		&& B.last a == fromIntegral (ord ':')
		&& isLetter (B.head a) = a <> b
	| otherwise = a <> B.singleton pathSeparator <> b

-- | Combine two paths with a path separator.
--   If the second path starts with a path separator or a drive letter, then it returns the second.
--   The intention is that @readFile (dir '</>' file)@ will access the same file as
--   @setCurrentDirectory dir; readFile file@.
--
-- > Posix:   "/directory" </> "file.ext" == "/directory/file.ext"
-- > Windows: "/directory" </> "file.ext" == "/directory\\file.ext"
-- >          "directory" </> "/file.ext" == "/file.ext"
-- > Valid x => (takeDirectory x </> takeFileName x) `equalFilePath` x
--
--   Combined:
--
-- > Posix:   "/" </> "test" == "/test"
-- > Posix:   "home" </> "bob" == "home/bob"
-- > Posix:   "x:" </> "foo" == "x:/foo"
-- > Windows: "C:\\foo" </> "bar" == "C:\\foo\\bar"
-- > Windows: "home" </> "bob" == "home\\bob"
--
--   Not combined:
--
-- > Posix:   "home" </> "/bob" == "/bob"
-- > Windows: "home" </> "C:\\bob" == "C:\\bob"
--
--   Not combined (tricky):
--
--   On Windows, if a filepath starts with a single slash, it is relative to the
--   root of the current drive. In [1], this is (confusingly) referred to as an
--   absolute path.
--   The current behavior of '</>' is to never combine these forms.
--
-- > Windows: "home" </> "/bob" == "/bob"
-- > Windows: "home" </> "\\bob" == "\\bob"
-- > Windows: "C:\\home" </> "\\bob" == "\\bob"
--
--   On Windows, from [1]: "If a file name begins with only a disk designator
--   but not the backslash after the colon, it is interpreted as a relative path
--   to the current directory on the drive with the specified letter."
--   The current behavior of '</>' is to never combine these forms.
--
-- > Windows: "D:\\foo" </> "C:bar" == "C:bar"
-- > Windows: "C:\\foo" </> "C:bar" == "C:bar"
(</>) :: RawFilePath -> RawFilePath -> RawFilePath
(</>) = combine

-- | Split a path by the directory separator.
--
-- > splitPath "/directory/file.ext" == ["/","directory/","file.ext"]
-- > mconcat (splitPath x) == x
-- > splitPath "test//item/" == ["test//","item/"]
-- > splitPath "test/item/file" == ["test/","item/","file"]
-- > splitPath "" == []
-- > Windows: splitPath "c:\\test\\path" == ["c:\\","test\\","path"]
-- > Posix:   splitPath "/file/test" == ["/","file/","test"]
splitPath :: RawFilePath -> [RawFilePath]
splitPath x = [drive | not (B.null drive)] ++ f path
  where
	(drive,path) = splitDrive x

	f y
		| B.null y = []
		| otherwise = (a<>c) : f d
	  where
		(a,b) = B.break isPathSeparator y
		(c,d) = B.span  isPathSeparator b

-- | Just as 'splitPath', but don't add the trailing slashes to each element.
--
-- >          splitDirectories "/directory/file.ext" == ["/","directory","file.ext"]
-- >          splitDirectories "test/file" == ["test","file"]
-- >          splitDirectories "/test/file" == ["/","test","file"]
-- > Windows: splitDirectories "C:\\test\\file" == ["C:\\", "test", "file"]
-- >          Valid x => joinPath (splitDirectories x) `equalFilePath` x
-- >          splitDirectories "" == []
-- > Windows: splitDirectories "C:\\test\\\\\\file" == ["C:\\", "test", "file"]
-- >          splitDirectories "/test///file" == ["/","test","file"]
splitDirectories :: RawFilePath -> [RawFilePath]
splitDirectories = map dropTrailingPathSeparator . splitPath


-- | Join path elements back together.
--
-- > joinPath ["/","directory/","file.ext"] == "/directory/file.ext"
-- > Valid x => joinPath (splitPath x) == x
-- > joinPath [] == ""
-- > Posix: joinPath ["test","file","path"] == "test/file/path"
joinPath :: [RawFilePath] -> RawFilePath
-- Note that this definition on c:\\c:\\, join then split will give c:\\.
joinPath = foldr combine mempty

---------------------------------------------------------------------
-- File name manipulators

-- | Equality of two 'FilePath's.
--   If you call @System.Directory.canonicalizePath@
--   first this has a much better chance of working.
--   Note that this doesn't follow symlinks or DOSNAM~1s.
--
-- >          x == y ==> equalFilePath x y
-- >          normalise x == normalise y ==> equalFilePath x y
-- >          equalFilePath "foo" "foo/"
-- >          not (equalFilePath "foo" "/foo")
-- > Posix:   not (equalFilePath "foo" "FOO")
-- > Windows: equalFilePath "foo" "FOO"
-- > Windows: not (equalFilePath "C:" "C:/")
equalFilePath :: RawFilePath -> RawFilePath -> Bool
equalFilePath a b = f a == f b
  where
	f x
		| isWindows = dropTrailingPathSeparator $ encodeFilePath $
			map toLower $ decodeFilePath $ normalise x
		| otherwise = dropTrailingPathSeparator $ normalise x

-- | Contract a filename, based on a relative path. Note that the resulting path
--   will never introduce @..@ paths, as the presence of symlinks means @..\/b@
--   may not reach @a\/b@ if it starts from @a\/c@. For a worked example see
--   <http://neilmitchell.blogspot.co.uk/2015/10/filepaths-are-subtle-symlinks-are-hard.html this blog post>.
--
--   The corresponding @makeAbsolute@ function can be found in
--   @System.Directory@.
--
-- >          makeRelative "/directory" "/directory/file.ext" == "file.ext"
-- >          Valid x => makeRelative (takeDirectory x) x `equalFilePath` takeFileName x
-- >          makeRelative x x == "."
-- >          Valid x y => equalFilePath x y || (isRelative x && makeRelative y x == x) || equalFilePath (y </> makeRelative y x) x
-- > Windows: makeRelative "C:\\Home" "c:\\home\\bob" == "bob"
-- > Windows: makeRelative "C:\\Home" "c:/home/bob" == "bob"
-- > Windows: makeRelative "C:\\Home" "D:\\Home\\Bob" == "D:\\Home\\Bob"
-- > Windows: makeRelative "C:\\Home" "C:Home\\Bob" == "C:Home\\Bob"
-- > Windows: makeRelative "/Home" "/home/bob" == "bob"
-- > Windows: makeRelative "/" "//" == "//"
-- > Posix:   makeRelative "/Home" "/home/bob" == "/home/bob"
-- > Posix:   makeRelative "/home/" "/home/bob/foo/bar" == "bob/foo/bar"
-- > Posix:   makeRelative "/fred" "bob" == "bob"
-- > Posix:   makeRelative "/file/test" "/file/test/fred" == "fred"
-- > Posix:   makeRelative "/file/test" "/file/test/fred/" == "fred/"
-- > Posix:   makeRelative "some/path" "some/path/a/b/c" == "a/b/c"
makeRelative :: RawFilePath -> RawFilePath -> RawFilePath
makeRelative root path
	| equalFilePath root path = "."
	| takeAbs root /= takeAbs path = path
	| otherwise = f (dropAbs root) (dropAbs path)
  where
	f "" y = B.dropWhile isPathSeparator y
	f x y =
		let (x1,x2) = g x
		    (y1,y2) = g y
		in if equalFilePath x1 y1 then f x2 y2 else path

	g x = (B.dropWhile isPathSeparator a, B.dropWhile isPathSeparator b)
	  where
		(a,b) = B.break isPathSeparator $ B.dropWhile isPathSeparator x

	-- on windows, need to drop '/' which is kind of absolute, but not a drive
	dropAbs x | hasLeadingPathSeparator x && not (hasDrive x) = B.tail x
	dropAbs x = dropDrive x

	takeAbs x | hasLeadingPathSeparator x && not (hasDrive x) = B.singleton pathSeparator
	-- A Windows drive letter is an ascii character, so it's safe to
	-- operate on the ByteString containing it using B8.
	takeAbs x = B8.map (\y -> if isPathSeparator (fromIntegral (ord y)) then chr (fromIntegral pathSeparator) else toLower y) $ takeDrive x

-- | Normalise a file
--
-- * \/\/ outside of the drive can be made blank
--
-- * \/ -> 'pathSeparator'
--
-- * .\/ -> \"\"
--
-- > Posix:   normalise "/file/\\test////" == "/file/\\test/"
-- > Posix:   normalise "/file/./test" == "/file/test"
-- > Posix:   normalise "/test/file/../bob/fred/" == "/test/file/../bob/fred/"
-- > Posix:   normalise "../bob/fred/" == "../bob/fred/"
-- > Posix:   normalise "./bob/fred/" == "bob/fred/"
-- > Windows: normalise "c:\\file/bob\\" == "C:\\file\\bob\\"
-- > Windows: normalise "c:\\" == "C:\\"
-- > Windows: normalise "c:\\\\\\\\" == "C:\\"
-- > Windows: normalise "C:.\\" == "C:"
-- > Windows: normalise "\\\\server\\test" == "\\\\server\\test"
-- > Windows: normalise "//server/test" == "\\\\server\\test"
-- > Windows: normalise "c:/file" == "C:\\file"
-- > Windows: normalise "/file" == "\\file"
-- > Windows: normalise "\\" == "\\"
-- > Windows: normalise "/./" == "\\"
-- >          normalise "." == "."
-- > Posix:   normalise "./" == "./"
-- > Posix:   normalise "./." == "./"
-- > Posix:   normalise "/./" == "/"
-- > Posix:   normalise "/" == "/"
-- > Posix:   normalise "bob/fred/." == "bob/fred/"
-- > Posix:   normalise "//home" == "/home"
normalise :: RawFilePath -> RawFilePath
normalise path
	| addPathSeparator = result <> B.singleton pathSeparator
	| otherwise = result
    where
	(drv,pth) = splitDrive path
	result = joinDrive' (normaliseDrive drv) (f pth)

	joinDrive' "" "" = "."
	joinDrive' d p = joinDrive d p

	addPathSeparator = isDirPath pth
		&& not (hasTrailingPathSeparator result)
		&& not (isRelativeDrive drv)

	isDirPath xs = hasTrailingPathSeparator xs
		|| not (B.null xs) && B.last xs == extSeparator
		&& hasTrailingPathSeparator (B.init xs)

	f :: RawFilePath -> RawFilePath
	f = joinPath . dropDots . propSep . splitDirectories

	propSep :: [RawFilePath] -> [RawFilePath]
	propSep (x:xs)
		| B.all isPathSeparator x = B.singleton pathSeparator : xs
		| otherwise = x : xs
	propSep [] = []

	dropDots :: [RawFilePath] -> [RawFilePath]
	dropDots = filter ("." /=)

normaliseDrive :: RawFilePath -> RawFilePath
normaliseDrive "" = ""
normaliseDrive _ | isPosix = B.singleton pathSeparator
normaliseDrive drive =
  case readDriveLetter x2 of
    Nothing -> x2
    Just (drv, _) ->
      case B.unpack drv of
        (x:_:[]) -> B.pack [upcasedriveletter x, colon]
        (x:_) -> B.pack [upcasedriveletter x, colon, pathSeparator]
        _ -> error "impossible"
  where
	x2 = B.map repSlash drive

	repSlash x = if isPathSeparator x then pathSeparator else x

	-- A Windows drive letter is an ascii character, so it's safe to
	-- operate on the ByteString containing it using B8.
	upcasedriveletter = fromIntegral . ord . toUpper . chr . fromIntegral
	colon = fromIntegral (ord ':')

-- Information for validity functions on Windows. See [1].
isBadCharacter :: Word8 -> Bool
isBadCharacter x = x >= 0 && x <= 31 || x `elem` l
  where
	l = map (fromIntegral . ord) ":*?><|\""

badElements :: [FilePath]
badElements =
	["CON","PRN","AUX","NUL","CLOCK$"
	,"COM1","COM2","COM3","COM4","COM5","COM6","COM7","COM8","COM9"
	,"LPT1","LPT2","LPT3","LPT4","LPT5","LPT6","LPT7","LPT8","LPT9"
	]

-- | Is a RawFilePath valid, i.e. could you create a file like it? This function checks for invalid names,
--   and invalid characters, but does not check if length limits are exceeded, as these are typically
--   filesystem dependent.
--
-- >          isValid "" == False
-- >          isValid "\0" == False
-- > Posix:   isValid "/random_ path:*" == True
-- > Posix:   isValid x == (x /= mempty)
-- > Windows: isValid "c:\\test" == True
-- > Windows: isValid "c:\\test:of_test" == False
-- > Windows: isValid "test*" == False
-- > Windows: isValid "c:\\test\\nul" == False
-- > Windows: isValid "c:\\test\\prn.txt" == False
-- > Windows: isValid "c:\\nul\\file" == False
-- > Windows: isValid "\\\\" == False
-- > Windows: isValid "\\\\\\foo" == False
-- > Windows: isValid "\\\\?\\D:file" == False
-- > Windows: isValid "foo\tbar" == False
-- > Windows: isValid "nul .txt" == False
-- > Windows: isValid " nul.txt" == True
isValid :: RawFilePath -> Bool
isValid path
	| B.null path = False
	| B.elem 0 path = False
	| isPosix = True
	| otherwise = 
		not (B.any isBadCharacter x2) &&
		not (any f $ splitDirectories x2) &&
		not (isJust (readDriveShare x1) && B.all isPathSeparator x1) &&
		not (isJust (readDriveUNC x1) && not (hasTrailingPathSeparator x1))
  where
	(x1,x2) = splitDrive path
	f x = map toUpper (decodeFilePath $ dropWhileEnd (== 32) $ dropExtensions x) `elem` badElements

-- | Take a FilePath and make it valid; does not change already valid FilePaths.
--
-- > isValid (makeValid x)
-- > isValid x ==> makeValid x == x
-- > makeValid "" == "_"
-- > makeValid "file\0name" == "file_name"
-- > Windows: makeValid "c:\\already\\/valid" == "c:\\already\\/valid"
-- > Windows: makeValid "c:\\test:of_test" == "c:\\test_of_test"
-- > Windows: makeValid "test*" == "test_"
-- > Windows: makeValid "c:\\test\\nul" == "c:\\test\\nul_"
-- > Windows: makeValid "c:\\test\\prn.txt" == "c:\\test\\prn_.txt"
-- > Windows: makeValid "c:\\test/prn.txt" == "c:\\test/prn_.txt"
-- > Windows: makeValid "c:\\nul\\file" == "c:\\nul_\\file"
-- > Windows: makeValid "\\\\\\foo" == "\\\\drive"
-- > Windows: makeValid "\\\\?\\D:file" == "\\\\?\\D:\\file"
-- > Windows: makeValid "nul .txt" == "nul _.txt"
makeValid :: RawFilePath -> RawFilePath
makeValid "" = "_"
makeValid path
        | isPosix = B.map (\x -> if x == 0 then underscore else x) path
        | isJust (readDriveShare drv) && B.all isPathSeparator drv = B.take 2 drv <> "drive"
        | isJust (readDriveUNC drv) && not (hasTrailingPathSeparator drv) =
            makeValid (drv <> B.singleton pathSeparator <> pth)
        | otherwise = joinDrive drv $ validElements $ validChars pth
    where
        (drv,pth) = splitDrive path

	underscore :: Word8
	underscore = fromIntegral (ord '_')

        validChars = B.map f
        f x = if isBadCharacter x then underscore else x

        validElements x = joinPath $ map g $ splitPath x
        g x = h a <> b
            where (a,b) = B.break isPathSeparator x
        h x = if map toUpper (decodeFilePath $ dropWhileEnd (== 32) a) `elem` badElements then a <> "_" <.> b else x
            where (a,b) = splitExtensions x

-- | Is a path relative, or is it fixed to the root?
--
-- > Windows: isRelative "path\\test" == True
-- > Windows: isRelative "c:\\test" == False
-- > Windows: isRelative "c:test" == True
-- > Windows: isRelative "c:\\" == False
-- > Windows: isRelative "c:/" == False
-- > Windows: isRelative "c:" == True
-- > Windows: isRelative "\\\\foo" == False
-- > Windows: isRelative "\\\\?\\foo" == False
-- > Windows: isRelative "\\\\?\\UNC\\foo" == False
-- > Windows: isRelative "/foo" == True
-- > Windows: isRelative "\\foo" == True
-- > Posix:   isRelative "test/path" == True
-- > Posix:   isRelative "/test" == False
-- > Posix:   isRelative "/" == False
--
-- According to [1]:
--
-- * "A UNC name of any format [is never relative]."
--
-- * "You cannot use the "\\?\" prefix with a relative path."
isRelative :: RawFilePath -> Bool
isRelative x = B.null drive || isRelativeDrive drive
  where
	drive = takeDrive x


{- c:foo -}
-- From [1]: "If a file name begins with only a disk designator but not the
-- backslash after the colon, it is interpreted as a relative path to the
-- current directory on the drive with the specified letter."
isRelativeDrive :: RawFilePath -> Bool
isRelativeDrive x =
	maybe False (not . hasTrailingPathSeparator . fst)
		(readDriveLetter x)

-- | @not . 'isRelative'@
--
-- > isAbsolute x == not (isRelative x)
isAbsolute :: RawFilePath -> Bool
isAbsolute = not . isRelative

-----------------------------------------------------------------------------
-- dropWhileEnd (/= 32) "foo bar" == "foo ")
dropWhileEnd :: (Word8 -> Bool) -> ByteString -> ByteString
dropWhileEnd p = fst . B.spanEnd p

{-

-- takeWhileEnd (>2) [1,2,3,4,1,2,3,4] == [3,4])
takeWhileEnd :: (a -> Bool) -> [a] -> [a]
takeWhileEnd p = reverse . takeWhile p . reverse

-- spanEnd (>2) [1,2,3,4,1,2,3,4] = ([1,2,3,4,1,2], [3,4])
spanEnd :: (a -> Bool) -> [a] -> ([a], [a])
spanEnd p xs = (dropWhileEnd p xs, takeWhileEnd p xs)

-- breakEnd (< 2) [1,2,3,4,1,2,3,4] == ([1,2,3,4,1],[2,3,4])
breakEnd :: (a -> Bool) -> [a] -> ([a], [a])
breakEnd p = spanEnd (not . p)

-- | The stripSuffix function drops the given suffix from a list. It returns
-- Nothing if the list did not end with the suffix given, or Just the list
-- before the suffix, if it does.
stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix xs ys = fmap reverse $ stripPrefix (reverse xs) (reverse ys)

-}
