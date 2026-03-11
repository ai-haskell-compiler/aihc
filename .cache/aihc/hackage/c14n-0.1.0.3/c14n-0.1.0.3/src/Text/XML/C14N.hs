--------------------------------------------------------------------------------
-- Haskell bindings for c14n implementation in libxml                         --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

-- | Provides a mid-level interface to libxml's implementation of c14n, i.e.
-- XML canonicalisation.
module Text.XML.C14N (
    -- * Canonicalisation
    c14n_1_0,
    c14n_exclusive_1_0,
    c14n_1_1,
    c14n,

    -- * Parsing
    xml_opt_recover,
    xml_opt_noent,
    xml_opt_dtdload,
    xml_opt_dtdattr,
    xml_opt_dtdvalid,
    xml_opt_noerror,
    xml_opt_nowarning,
    xml_opt_pedantic,
    xml_opt_noblanks,
    xml_opt_sax1,
    xml_opt_xinclude,
    xml_opt_nonet,
    xml_opt_nodict,
    xml_opt_nsclean,
    xml_opt_nocdata,
    xml_opt_noxincnode,
    xml_opt_compact,
    xml_opt_old10,
    xml_opt_nobasefix,
    xml_opt_huge,
    xml_opt_oldsax,
    xml_opt_ignore_env,
    xml_opt_big_lines,
    parseXml
) where

--------------------------------------------------------------------------------

import Control.Exception

import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

import Text.XML.C14N.LibXML

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Error
import Foreign.C.Types

--------------------------------------------------------------------------------

-- | 'parseXml' @parseOpts text@ parses @text@ into an XML document using
-- libxml according to options given by @parseOpts@.
parseXml :: [CInt] -> BS.ByteString -> IO (ForeignPtr LibXMLDoc)
parseXml opts bin = newForeignPtr xmlFreeDoc =<<
    (BS.useAsCStringLen bin $ \(ptr, len) ->
        throwErrnoIfNull "xmlReadMemory" $ xmlReadMemory
            ptr (fromIntegral len) nullPtr nullPtr (foldl (.|.) 0 opts))

-- | 'withXmlXPathNodeList' @docPtr xPathLocation continuation@ evaluates the
-- XPath location path given by @xPathLocation@ in the document context
-- pointed at by @docPtr@ and calls @continuation@ with the result.
withXmlXPathNodeList :: Ptr LibXMLDoc
                     -> BS.ByteString
                     -> (Ptr LibXMLNodeSet -> IO a)
                     -> IO a
withXmlXPathNodeList docPtr expr cont =
    -- initialise a new XPath context, run the continuation with the context
    -- as argument, and then free up the context again afterwards
    bracket (xmlXPathNewContext docPtr) xmlXPathFreeContext $ \ctx ->
    -- get a C string pointer for the XPath location path
    BS.useAsCString expr $ \strPtr ->
    -- evaluate the XPath location path and free up the resulting object
    -- after the continuation is finished; see
    -- http://xmlsoft.org/html/libxml-xpath.html#xmlXPathEval
    bracket
        ( throwErrnoIfNull "xmlXPathEval" $
            xmlXPathEval (castPtr strPtr) ctx
        )
        xmlXPathFreeObject
        -- the XPath object structure contains the node set pointer
        -- at offset 8; see
        -- http://xmlsoft.org/html/libxml-xpath.html#xmlXPathObject
        $ \a -> peekByteOff a 8 >>= cont

-- | 'c14n' @parseOpts mode nsPrefixes keepComments xPathLocation input@
-- canonicalises the document given by @input@, which is parsed using options
-- specified by @parseOpts@. The @mode@ argument deteremines the
-- canonicalisation mode to use. @nsPrefixes@ gives a (potentially empty)
-- list of namespace prefixes which is used when @mode@ is
-- 'c14n_exclusive_1_0'. If @keepComments@ is 'True', all comments are kept
-- in the output. @xPathLocation@ is used to select a set of nodes that should
-- be included in the canonicalised result.
c14n :: [CInt]
     -> CInt
     -> [BS.ByteString]
     -> Bool
     -> Maybe BS.ByteString
     -> BS.ByteString
     -> IO BS.ByteString
c14n opts mode nsPrefixes keepComments xpath bin =
    -- parse the input xml
    parseXml opts bin >>= \docPtr ->
    -- wrap the pointer we got in a foreign pointer
    withForeignPtr docPtr $ \ptr ->
    -- convert the namespace prefixes into C strings
    withMany BS.useAsCString nsPrefixes $ \inclPtr ->
    -- turn the Haskell list of C strings into a C array,
    -- terminated by NULL
    withArray0 nullPtr inclPtr $ \arrayPtr ->
    -- get a pointer to the node set
    maybeWith (withXmlXPathNodeList ptr) xpath $ \nsPtr ->
    -- allocate some memory for a pointer to the results
    alloca $ \outPtr -> do
        -- convert the option determining whether to keep comments from a
        -- Haskell boolean to a CInt
        let commentsOpt = fromIntegral (fromEnum keepComments)
        -- cast from CChar pointers to whatever LibXMLChar is (e.g. Word8)
        let prefixesPtr :: Ptr (Ptr LibXMLChar)
            prefixesPtr = castPtr arrayPtr

        -- run the canonicalisation function on the document;
        -- this function returns the number of bytes that were written
        -- to outPtr or a negative value if this fails
        numBytes <- throwErrnoIf (<0) "xmlC14NDocDumpMemory" $
            xmlC14NDocDumpMemory ptr nsPtr mode prefixesPtr commentsOpt outPtr

        -- dereference the results pointer
        ptrPtr <- peek outPtr

        -- construct a ByteString from the C string and return it
        BS.unsafePackCStringFinalizer
            ptrPtr (fromIntegral numBytes) (freeXml ptrPtr)

--------------------------------------------------------------------------------
