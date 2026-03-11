{-# CFILES cbits/stub.c #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text..Bibutils
-- Copyright   :  (C) 2008 Andrea Rossato
-- License     :  BSD3
--
-- Maintainer  :  vhaisman@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Here is a simple program using hs-bibutils to conver a MODS
-- collection into a bibtex file, sending the output to stdout:
--
-- > import Text.Bibutils
-- >
-- > main :: IO ()
-- > main = do
-- >   bibl  <- bibl_init
-- >   param <- bibl_initparams mods_in bibtex_out "mods2bibtex"
-- >   setFormatOpts param [bibout_brackets, bibout_uppercase]
-- >   setBOM        param
-- >   setVerbose    param
-- >   bibl_read     param bibl "/path/to/bibtex.bib"
-- >   bibl_write    param bibl "-"
-- >   bibl_free       bibl
-- >   bibl_freeparams param
--
-----------------------------------------------------------------------------

module Text.Bibutils
    ( -- * Basic Functions
      Bibl
    , Param (..)
    , bibl_init
    , bibl_initparams
    , bibl_read
    , bibl_write
    , bibl_readasis
    , bibl_addtoasis
    , bibl_readcorps
    , bibl_addtocorps
    , bibl_free
    , bibl_freeparams
    , bibl_reporterr

    -- * Auxiliary Functions
    , numberOfRefs
    , status
    -- ** Functions for Setting Parameters
    , setParam
    , setFormatOpts
    , setCharsetIn
    , setCharsetOut
    , setBOM
    , unsetBOM
    , setNoSplitTitle
    , unsetNoSplitTitle
    , setLatexOut
    , unsetLatexOut
    , setXmlOut
    , unsetXmlOut
    , setAddcount
    , unsetAddcount
    , setSinglerefperfile
    , unsetSinglerefperfile
    , setOutputRawOpts
    , setVerbose
    , setVerboseLevel
    , unsetVerbose

    -- * Input Formats
    , BiblioIn
    , mods_in
    , bibtex_in
    , ris_in
    , endnote_in
    , copac_in
    , isi_in
    , medline_in
    , biblatex_in
    , endnotexml_in
    , ebi_in
    , word_in
    , nbib_in

    -- * Output Formats
    , BiblioOut
    , mods_out
    , bibtex_out
    , ris_out
    , endnote_out
    , isi_out
    , word2007_out
    , adsab_out
    , nbib_out

    -- * Options for Specific Output Formats
    , FormatOpt
    , bibout_finalcomma
    , bibout_singledash
    , bibout_whitespace
    , bibout_brackets
    , bibout_uppercase
    , bibout_strictkey
    , bibout_shorttitle
    , bibout_dropkey
    , modsout_dropkey

    -- * Charsets
    , Charset
    , bibl_charset_unknown
    , bibl_charset_unicode
    , bibl_charset_gb18030
    , bibl_charset_default
    , bibl_charset_utf8_default
    , bibl_charset_bom_default

    -- * Return Status
    , Status
    , bibl_ok
    , bibl_err_badinput
    , bibl_err_memerr
    , bibl_err_cantopen

    -- * Raw
    , Raw
    , bibl_raw_with_charset_convert
    , bibl_raw_with_make_ref_id

    ) where

import Control.Monad
import Foreign.C
import Foreign

-- | A type for storing the C struct with the bibliography data.
-- Mostly opaque to the Haskell side. See 'numberOfRefs' to retrieve
-- the number of references stored in the struct.
newtype Bibl = Bibl { n :: CLong }

instance Storable Bibl where
    sizeOf    _     = #{size      bibl}
    alignment _     = #{alignment bibl}
    peek p          = #{peek      bibl, n } p >>= return . Bibl
    poke p (Bibl x) = #{poke      bibl, n } p x

-- | Initialize the 'Bibl' C struct. Usually the first function being
-- called.
bibl_init :: IO (ForeignPtr Bibl)
bibl_init
    = alloca $ \p -> do
        c_bibl_init    p
        newForeignPtr_ p

-- | Free the 'Bibl' C struct.
bibl_free :: ForeignPtr Bibl -> IO ()
bibl_free bibl = withForeignPtr bibl c_bibl_free

-- | Retrieve the number of references from a 'Bibl' C struct.
numberOfRefs :: ForeignPtr Bibl -> IO Int
numberOfRefs b
    = withForeignPtr b $ \cb -> peek cb >>= return . fromIntegral . n

-- | A type for storing the Param C struct. It should be accessed with
-- the functions provided, such as 'setCharsetIn', etc.
data Param
    = Param
      { redaformat       :: CInt
      , writeformat      :: CInt
      , charsetin        :: CInt
      , charsetin_src    :: CUChar
      , latexin          :: CUChar
      , utf8in           :: CUChar
      , xmlin            :: CUChar
      , nosplittitle     :: CUChar
      , charsetout       :: CInt
      , charsetout_src   :: CUChar
      , latexout         :: CUChar
      , utf8out          :: CUChar
      , utf8bom          :: CUChar
      , xmlout           :: CUChar
      , format_opts      :: CInt
      , addcount         :: CInt
      , output_raw       :: CUChar
      , verbose          :: CUChar
      , singlerefperfile :: CUChar
      } deriving ( Show )

instance Storable Param where
    sizeOf    _ = #{size param}
    alignment _ = #{alignment param}
    peek p      =  Param
                  `fmap` #{peek param, readformat       } p
                  `ap`   #{peek param, writeformat      } p
                  `ap`   #{peek param, charsetin        } p
                  `ap`   #{peek param, charsetin_src    } p
                  `ap`   #{peek param, latexin          } p
                  `ap`   #{peek param, utf8in           } p
                  `ap`   #{peek param, xmlin            } p
                  `ap`   #{peek param, nosplittitle     } p
                  `ap`   #{peek param, charsetout       } p
                  `ap`   #{peek param, charsetout_src   } p
                  `ap`   #{peek param, latexout         } p
                  `ap`   #{peek param, utf8out          } p
                  `ap`   #{peek param, utf8bom          } p
                  `ap`   #{peek param, xmlout           } p
                  `ap`   #{peek param, format_opts      } p
                  `ap`   #{peek param, addcount         } p
                  `ap`   #{peek param, output_raw       } p
                  `ap`   #{peek param, verbose          } p
                  `ap`   #{peek param, singlerefperfile } p
    poke p (Param rf wf ci csi li ui xi nt co cso lo uo ub xo fo a raw v s) = do
                         #{poke param, readformat       } p rf
                         #{poke param, writeformat      } p wf
                         #{poke param, charsetin        } p ci
                         #{poke param, charsetin_src    } p csi
                         #{poke param, latexin          } p li
                         #{poke param, utf8in           } p ui
                         #{poke param, xmlin            } p xi
                         #{poke param, nosplittitle     } p nt
                         #{poke param, charsetout       } p co
                         #{poke param, charsetout_src   } p cso
                         #{poke param, latexout         } p lo
                         #{poke param, utf8out          } p uo
                         #{poke param, utf8bom          } p ub
                         #{poke param, xmlout           } p xo
                         #{poke param, format_opts      } p fo
                         #{poke param, addcount         } p a
                         #{poke param, output_raw       } p raw
                         #{poke param, verbose          } p v
                         #{poke param, singlerefperfile } p s

-- | Initialize the 'Param' C struct, given the input bibliographic
-- format, the output bibliographic format, and the program name to
-- be used for displaying debugging information.
bibl_initparams :: BiblioIn -> BiblioOut -> String -> IO (ForeignPtr Param)
bibl_initparams i o s
    = alloca $ \p -> withCString s $ \cs  -> do
        c_bibl_initparams p (unBiblioIn i) (unBiblioOut o) cs
        newForeignPtr_ p

-- | Free the 'Param' C struct.
bibl_freeparams :: ForeignPtr Param -> IO ()
bibl_freeparams param = withForeignPtr param c_bibl_freeparams

-- | Set fields of the 'Param' C struct directly.
setParam :: ForeignPtr Param -> (Param -> Param) -> IO ()
setParam p f = withForeignPtr p $ \cp -> peek cp >>= poke cp . f

-- | Set the input charset. Default is Latin-1 (ISO8859-1). See
-- 'Charset'.
setCharsetIn ::  ForeignPtr Param -> Charset -> IO ()
setCharsetIn p c
    = setParam p $ \param -> param { charsetin = charset c }

-- | Set the output charset.
setCharsetOut ::  ForeignPtr Param -> Charset -> IO ()
setCharsetOut p c
    = setParam p $ \param -> param { charsetout = charset c }

-- | Set output format specific options. See 'FormatOpt'.
setFormatOpts ::  ForeignPtr Param -> [FormatOpt] -> IO ()
setFormatOpts p os
    = setParam p $ \param -> param { format_opts = unFormatOpt $ combineFormatOpts os }

-- | Write utf8 byte-order-mark.
setBOM ::  ForeignPtr Param -> IO ()
setBOM p
    = setParam p $ \param -> param { utf8bom = 1 }

unsetBOM ::  ForeignPtr Param -> IO ()
unsetBOM p
    = setParam p $ \param -> param { utf8bom = 0 }

-- | Do not split titles.
setNoSplitTitle ::  ForeignPtr Param -> IO ()
setNoSplitTitle p
    = setParam p $ \param -> param { nosplittitle = 1 }

-- | Split titles.
unsetNoSplitTitle ::  ForeignPtr Param -> IO ()
unsetNoSplitTitle p
    = setParam p $ \param -> param { nosplittitle = 0 }

-- | Write Latex codes.
setLatexOut ::  ForeignPtr Param -> IO ()
setLatexOut p
    = setParam p $ \param -> param { latexout = 1 }

unsetLatexOut ::  ForeignPtr Param -> IO ()
unsetLatexOut p
    = setParam p $ \param -> param { latexout = 0 }

-- | Write characters in XML entities.
setXmlOut ::  ForeignPtr Param -> IO ()
setXmlOut p
    = setParam p $ \param -> param { xmlout = 1 }

unsetXmlOut ::  ForeignPtr Param -> IO ()
unsetXmlOut p
    = setParam p $ \param -> param { xmlout = 0 }

-- | Add reference count to reference id.
setAddcount ::  ForeignPtr Param -> IO ()
setAddcount p
    = setParam p $ \param -> param { addcount = 1 }

unsetAddcount ::  ForeignPtr Param -> IO ()
unsetAddcount p
    = setParam p $ \param -> param { addcount = 0 }

-- | Output a single reference for each file.
setSinglerefperfile ::  ForeignPtr Param -> IO ()
setSinglerefperfile p
    = setParam p $ \param -> param { singlerefperfile = 1 }

unsetSinglerefperfile ::  ForeignPtr Param -> IO ()
unsetSinglerefperfile p
    = setParam p $ \param -> param { singlerefperfile = 0 }

-- | Set the output charset.
setOutputRawOpts ::  ForeignPtr Param -> [Raw] -> IO ()
setOutputRawOpts p os
    = setParam p $ \param -> param { output_raw = unRaw $ combineRawOpts os }

-- | Verbose output.
setVerbose ::  ForeignPtr Param -> IO ()
setVerbose p
    = setParam p $ \param -> param { verbose = 1 }

-- | Verbose output.
setVerboseLevel ::  ForeignPtr Param -> Int -> IO ()
setVerboseLevel p v
    = setParam p $ \param -> param { verbose = toEnum v }

-- | Suppress verbose output.
unsetVerbose ::  ForeignPtr Param -> IO ()
unsetVerbose p
    = setParam p $ \param -> param { verbose = 0 }

-- | Given a 'Param' C structure, a 'Bibl' C structure, the path to
-- the input file (@\"-\"@ for the standard input), read the file,
-- storing the data in the 'Bibl' struct, and report a 'Status'.
bibl_read :: ForeignPtr Param -> ForeignPtr Bibl -> FilePath -> IO Status
bibl_read param bibl path
    = withForeignPtr param $ \cparam ->
      withForeignPtr bibl  $ \cbibl  ->
      withCString    path  $ \cpath  ->
      withCString    "r"   $ \cmode  -> do
        cfile <- if path == "-"
                 then return c_stdin
                 else throwErrnoIfNull "fopen: " (fopen cpath cmode)
        cint  <- c_bibl_read cbibl cfile cpath cparam
        when (path /= "-") $ fclose cfile >> return ()
        return $ Status cint

-- | Given a 'Param' C structure, a 'Bibl' C structure, the path to an
-- output file (@\"-\"@ for the standard output), write the file
-- returning a 'Status'.
bibl_write :: ForeignPtr Param -> ForeignPtr Bibl -> FilePath -> IO Status
bibl_write param bibl path
    = withForeignPtr param $ \cparam ->
      withForeignPtr bibl  $ \cbibl  ->
      withCString    "w"   $ \cmode  -> do
        cfile <- if path == "-"
                 then return c_stdout
                 else withCString path $ throwErrnoIfNull "fopen: " . flip fopen cmode
        cint <- c_bibl_write cbibl cfile cparam
        when (path /= "-") $ fclose cfile >> return ()
        return $ Status cint

bibl_readasis :: ForeignPtr Param -> FilePath -> IO ()
bibl_readasis param path
    = withForeignPtr param  $ \cparam ->
      withCString    path  $ \cpath   -> do
        c_bibl_readasis cparam cpath

bibl_addtoasis :: ForeignPtr Param -> String -> IO ()
bibl_addtoasis param entry
    = withForeignPtr param $ \cparam ->
      withCString    entry $ \centry -> do
        c_bibl_addtoasis cparam centry

bibl_readcorps :: ForeignPtr Param -> FilePath -> IO ()
bibl_readcorps param path
    = withForeignPtr param  $ \cparam ->
      withCString    path  $ \cpath   -> do
        c_bibl_readcorps cparam cpath

bibl_addtocorps :: ForeignPtr Param -> String -> IO ()
bibl_addtocorps param entry
    = withForeignPtr param $ \cparam ->
      withCString    entry $ \centry -> do
        c_bibl_addtocorps cparam centry

bibl_reporterr :: Status -> IO ()
bibl_reporterr (Status st) = c_bibl_reporterr st

newtype BiblioIn  = BiblioIn  { unBiblioIn  :: CInt }
    deriving ( Eq )

#{enum BiblioIn, BiblioIn
 , mods_in       = BIBL_MODSIN
 , bibtex_in     = BIBL_BIBTEXIN
 , ris_in        = BIBL_RISIN
 , endnote_in    = BIBL_ENDNOTEIN
 , copac_in      = BIBL_COPACIN
 , isi_in        = BIBL_ISIIN
 , medline_in    = BIBL_MEDLINEIN
 , endnotexml_in = BIBL_ENDNOTEXMLIN
 , biblatex_in   = BIBL_BIBLATEXIN
 , ebi_in        = BIBL_EBIIN
 , word_in       = BIBL_WORDIN
 , nbib_in       = BIBL_NBIBIN
 }

newtype BiblioOut = BiblioOut { unBiblioOut :: CInt }
    deriving ( Eq )

#{enum BiblioOut, BiblioOut
 , mods_out     = BIBL_MODSOUT
 , bibtex_out   = BIBL_BIBTEXOUT
 , ris_out      = BIBL_RISOUT
 , endnote_out  = BIBL_ENDNOTEOUT
 , isi_out      = BIBL_ISIOUT
 , word2007_out = BIBL_WORD2007OUT
 , adsab_out    = BIBL_ADSABSOUT
 , nbib_out     = BIBL_NBIBOUT
 }

newtype FormatOpt = FormatOpt { unFormatOpt :: CInt }

#include "bibutils.h"

#{enum FormatOpt, FormatOpt
 , bibout_finalcomma = BIBL_FORMAT_BIBOUT_FINALCOMMA
 , bibout_singledash = BIBL_FORMAT_BIBOUT_SINGLEDASH
 , bibout_whitespace = BIBL_FORMAT_BIBOUT_WHITESPACE
 , bibout_brackets   = BIBL_FORMAT_BIBOUT_BRACKETS
 , bibout_uppercase  = BIBL_FORMAT_BIBOUT_UPPERCASE
 , bibout_strictkey  = BIBL_FORMAT_BIBOUT_STRICTKEY
 , bibout_shorttitle = BIBL_FORMAT_BIBOUT_SHORTTITLE
 , bibout_dropkey    = BIBL_FORMAT_BIBOUT_DROPKEY
 , modsout_dropkey   = BIBL_FORMAT_MODSOUT_DROPKEY
 }

newtype Status = Status { status :: CInt }
    deriving ( Eq, Show )

#{enum Status, Status
 , bibl_ok           = BIBL_OK
 , bibl_err_badinput = BIBL_ERR_BADINPUT
 , bibl_err_memerr   = BIBL_ERR_MEMERR
 , bibl_err_cantopen = BIBL_ERR_CANTOPEN
 }

newtype Raw = Raw { unRaw :: CUChar }
    deriving ( Eq, Show )

#{enum Raw, Raw
 , bibl_raw_with_charset_convert = BIBL_RAW_WITHCHARCONVERT
 , bibl_raw_with_make_ref_id     = BIBL_RAW_WITHMAKEREFID
 }

newtype Charset = Charset { charset :: CInt } deriving ( Eq )

#{enum Charset, Charset
 , bibl_charset_unknown      = BIBL_CHARSET_UNKNOWN
 , bibl_charset_unicode      = BIBL_CHARSET_UNICODE
 , bibl_charset_gb18030      = BIBL_CHARSET_GB18030
 , bibl_charset_default      = BIBL_CHARSET_DEFAULT
 , bibl_charset_utf8_default = BIBL_CHARSET_UTF8_DEFAULT
 , bibl_charset_bom_default  = BIBL_CHARSET_BOM_DEFAULT
 }

-- Combine a list of output options into a single option, using bitwise (.|.)
combineRawOpts :: [Raw] -> Raw
combineRawOpts = Raw . foldr ((.|.) . unRaw) 0

-- Combine a list of options into a single option, using bitwise (.|.)
combineFormatOpts :: [FormatOpt] -> FormatOpt
combineFormatOpts = FormatOpt . foldr ((.|.) . unFormatOpt) 0

#include "bibutils.h"
#include "bibl.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

foreign import ccall unsafe "bibl_init"
    c_bibl_init :: Ptr Bibl -> IO ()

foreign import ccall unsafe "bibl_free"
    c_bibl_free :: Ptr Bibl -> IO ()

foreign import ccall unsafe "bibl_initparams"
    c_bibl_initparams :: Ptr Param -> CInt -> CInt -> CString -> IO ()

foreign import ccall unsafe "bibl_freeparams"
    c_bibl_freeparams :: Ptr Param -> IO ()

foreign import ccall unsafe "bibl_read"
    c_bibl_read :: Ptr Bibl -> Ptr CFile -> CString -> Ptr Param -> IO CInt

foreign import ccall unsafe "bibl_write"
    c_bibl_write :: Ptr Bibl -> Ptr CFile -> Ptr Param -> IO CInt

foreign import ccall unsafe "bibl_readasis"
    c_bibl_readasis :: Ptr Param -> CString -> IO ()

foreign import ccall unsafe "bibl_addtoasis"
    c_bibl_addtoasis :: Ptr Param -> CString -> IO ()

foreign import ccall unsafe "bibl_readcorps"
    c_bibl_readcorps :: Ptr Param -> CString -> IO ()

foreign import ccall unsafe "bibl_addtocorps"
    c_bibl_addtocorps :: Ptr Param -> CString -> IO ()

foreign import ccall unsafe "bibl_reporterr"
    c_bibl_reporterr :: CInt -> IO ()

foreign import ccall unsafe "fopen"
    fopen :: CString -> CString -> IO (Ptr CFile)

foreign import ccall unsafe "fclose"
    fclose :: Ptr CFile -> IO CInt

foreign import ccall unsafe "c_stdin"  c_stdin  :: Ptr CFile
foreign import ccall unsafe "c_stdout" c_stdout :: Ptr CFile
