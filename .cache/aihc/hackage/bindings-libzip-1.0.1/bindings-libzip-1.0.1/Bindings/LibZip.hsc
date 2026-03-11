#include <bindings.dsl.h>
#include <zip.h>

-- | This module provides automatic low-level bindings to @libzip@
-- library, version 1.0. See also:
--
--   * @libzip@ documention: <http://nih.at/libzip/libzip.html> and @zip.h@
--
--   * @bindings-DSL@ documentation:
--     <https://github.com/jwiegley/bindings-dsl>
--
-- Higher-level interface is provided by a separate LibZip package:
--
--   * <http://hackage.haskell.org/package/LibZip>
--
-- Note: this is the first version to support libzip 1.0 and it lacks
-- support for some of the new libzip functions. Contributions are welcome.


module Bindings.LibZip where
#strict_import

#integral_t zip_flags_t

-- flags for zip_open

#num ZIP_CREATE
#num ZIP_EXCL
#num ZIP_CHECKCONS
#num ZIP_TRUNCATE
#num ZIP_RDONLY

-- flags for zip_name_locate, zip_fopen, zip_stat, ...

#num ZIP_FL_NOCASE
#num ZIP_FL_NODIR
#num ZIP_FL_COMPRESSED
#num ZIP_FL_UNCHANGED
#num ZIP_FL_RECOMPRESS
#num ZIP_FL_ENCRYPTED
#num ZIP_FL_ENC_GUESS
#num ZIP_FL_ENC_RAW
#num ZIP_FL_ENC_STRICT
#num ZIP_FL_LOCAL
#num ZIP_FL_CENTRAL
#num ZIP_FL_ENC_UTF_8
#num ZIP_FL_ENC_CP437
#num ZIP_FL_OVERWRITE


-- archive global flags flags

#num ZIP_AFL_RDONLY

-- new extra field

#num ZIP_EXTRA_FIELD_ALL
#num ZIP_EXTRA_FIELD_NEW

-- libzip error codes

#num ZIP_ER_OK
#num ZIP_ER_MULTIDISK
#num ZIP_ER_RENAME
#num ZIP_ER_CLOSE
#num ZIP_ER_SEEK
#num ZIP_ER_READ
#num ZIP_ER_WRITE
#num ZIP_ER_CRC
#num ZIP_ER_ZIPCLOSED
#num ZIP_ER_NOENT
#num ZIP_ER_EXISTS
#num ZIP_ER_OPEN
#num ZIP_ER_TMPOPEN
#num ZIP_ER_ZLIB
#num ZIP_ER_MEMORY
#num ZIP_ER_CHANGED
#num ZIP_ER_COMPNOTSUPP
#num ZIP_ER_EOF
#num ZIP_ER_INVAL
#num ZIP_ER_NOZIP
#num ZIP_ER_INTERNAL
#num ZIP_ER_INCONS
#num ZIP_ER_REMOVE
#num ZIP_ER_DELETED
#num ZIP_ER_ENCRNOTSUPP
#num ZIP_ER_RDONLY
#num ZIP_ER_NOPASSWD
#num ZIP_ER_WRONGPASSWD
#num ZIP_ER_OPNOTSUPP
#num ZIP_ER_INUSE
#num ZIP_ER_TELL

-- type of system error value

#num ZIP_ET_NONE
#num ZIP_ET_SYS
#num ZIP_ET_ZLIB

-- compression methods

#num ZIP_CM_DEFAULT
#num ZIP_CM_STORE
#num ZIP_CM_SHRINK
#num ZIP_CM_REDUCE_1
#num ZIP_CM_REDUCE_2
#num ZIP_CM_REDUCE_3
#num ZIP_CM_REDUCE_4
#num ZIP_CM_IMPLODE
#num ZIP_CM_DEFLATE
#num ZIP_CM_DEFLATE64
#num ZIP_CM_PKWARE_IMPLODE
#num ZIP_CM_BZIP2
#num ZIP_CM_LZMA
#num ZIP_CM_TERSE
#num ZIP_CM_LZ77
#num ZIP_CM_WAVPACK
#num ZIP_CM_PPMD

-- encryption methods

#num ZIP_EM_NONE
#num ZIP_EM_TRAD_PKWARE
#num ZIP_EM_UNKNOWN

#num ZIP_OPSYS_DOS
#num ZIP_OPSYS_AMIGA
#num ZIP_OPSYS_OPENVMS
#num ZIP_OPSYS_UNIX
#num ZIP_OPSYS_VM_CMS
#num ZIP_OPSYS_ATARI_ST
#num ZIP_OPSYS_OS_2
#num ZIP_OPSYS_MACINTOSH
#num ZIP_OPSYS_Z_SYSTEM
#num ZIP_OPSYS_CPM
#num ZIP_OPSYS_WINDOWS_NTFS
#num ZIP_OPSYS_MVS
#num ZIP_OPSYS_VSE
#num ZIP_OPSYS_ACORN_RISC
#num ZIP_OPSYS_VFAT
#num ZIP_OPSYS_ALTERNATE_MVS
#num ZIP_OPSYS_BEOS
#num ZIP_OPSYS_TANDEM
#num ZIP_OPSYS_OS_400
#num ZIP_OPSYS_OS_X

#integral_t enum zip_source_cmd
#synonym_t zip_source_cmd_t, <enum zip_source_cmd>
#num ZIP_SOURCE_OPEN
#num ZIP_SOURCE_READ
#num ZIP_SOURCE_CLOSE
#num ZIP_SOURCE_STAT
#num ZIP_SOURCE_ERROR
#num ZIP_SOURCE_FREE
#num ZIP_SOURCE_SEEK
#num ZIP_SOURCE_TELL
#num ZIP_SOURCE_BEGIN_WRITE
#num ZIP_SOURCE_COMMIT_WRITE
#num ZIP_SOURCE_ROLLBACK_WRITE
#num ZIP_SOURCE_WRITE
#num ZIP_SOURCE_SEEK_WRITE
#num ZIP_SOURCE_TELL_WRITE
#num ZIP_SOURCE_SUPPORTS
#num ZIP_SOURCE_REMOVE

#num ZIP_SOURCE_SUPPORTS_READABLE
#num ZIP_SOURCE_SUPPORTS_SEEKABLE
#num ZIP_SOURCE_SUPPORTS_WRITABLE

-- /* for use by sources */
-- struct zip_source_args_seek {
--     zip_int64_t offset;
--     int whence;
-- };
#starttype struct zip_source_args_seek
#field offset, CLLong
#field whence, CInt
#stoptype

#synonym_t zip_source_args_seek_t, <struct zip_source_args_seek>

-- typedef zip_int64_t (*zip_source_callback)(void *, void *, zip_uint64_t, enum zip_source_cmd);
#callback zip_source_callback , Ptr () -> Ptr () -> CULLong -> <zip_source_cmd> -> IO CULLong

-- /* error information */
-- /* use zip_error_*() to access */
-- struct zip_error {
--     int zip_err;	/* libzip error code (ZIP_ER_*) */
--     int sys_err;	/* copy of errno (E*) or zlib error code */
--     char *str;		/* string representation or NULL */
-- };
#starttype struct zip_error
#field zip_err, CInt
#field sys_err, CInt
#field str,     CString
#stoptype

#opaque_t struct zip
#opaque_t struct zip_file
#opaque_t struct zip_source
#synonym_t zip_t, <struct zip>
#synonym_t zip_error_t, <struct zip_error>
#synonym_t zip_file_t, <struct zip_file>
#synonym_t zip_source_t, <struct zip_source>
#synonym_t zip_stat_t, <struct zip_stat>

#num ZIP_STAT_NAME
#num ZIP_STAT_INDEX
#num ZIP_STAT_SIZE
#num ZIP_STAT_COMP_SIZE
#num ZIP_STAT_MTIME
#num ZIP_STAT_CRC
#num ZIP_STAT_COMP_METHOD
#num ZIP_STAT_ENCRYPTION_METHOD
#num ZIP_STAT_FLAGS

#opaque_t time_t

-- struct zip_stat {
--     zip_uint64_t valid;                 /* which fields have valid values */
--     const char *name;                   /* name of the file */
--     zip_uint64_t index;                 /* index within archive */
--     zip_uint64_t size;                  /* size of file (uncompressed) */
--     zip_uint64_t comp_size;             /* size of file (compressed) */
--     time_t mtime;                       /* modification time */
--     zip_uint32_t crc;                   /* crc of file data */
--     zip_uint16_t comp_method;           /* compression method used */
--     zip_uint16_t encryption_method;     /* encryption method used */
--     zip_uint32_t flags;                 /* reserved for future use */
-- };
#starttype struct zip_stat
#field valid, CULLong
#field name, Ptr CChar
#field index, CULLong
#field size, CULLong
#field comp_size, CULLong
#field mtime, CTime
#field crc, CUInt
#field comp_method, CUShort
#field encryption_method, CUShort
#field flags, CUInt
#stoptype

-- make every declaration one-line, and replace
--    ZIP_EXTERN ->
--    \/\*.*$ ->
--    const char * -> CString
--    char * -> Ptr CChar
--    struct foo * -> Ptr <foo>
--    (with regexp: struct \([a-z0-9_]*\) \* -> Ptr <\1> )
--    FILE -> CFile
--    int -> CInt
--    zip_uint8_t -> CUChar
--    zip_int16_t -> CShort
--    zip_uint16_t -> CUShort
--    zip_int32_t -> CInt
--    zip_uint32_t -> CUInt
--    zip_int64_t -> CLLong
--    zip_uint64_t -> CULLong
--    void -> ()
--    const ->
--    zip_source_callback -> <zip_source_callback>
--    foo * -> Ptr foo   -- with regexps
--    regexp-replace "\(.*\)\(zip_[a-z0-9_]+\)(\(.*\));" "#ccall \2 , \3 -> IO (\1)"

-- deprecated API

#ccall zip_add , Ptr <zip> -> CString -> Ptr <zip_source> -> IO (CLLong)
#ccall zip_add_dir , Ptr <zip> -> CString -> IO (CLLong)
#ccall zip_get_file_comment , Ptr <zip> -> CULLong -> Ptr CInt -> CInt -> IO (CString)
#ccall zip_get_num_files , Ptr <zip> -> IO (CInt)
#ccall zip_rename , Ptr <zip> -> CULLong -> CString -> IO (CInt)
#ccall zip_replace , Ptr <zip> -> CULLong -> Ptr <zip_source> -> IO (CInt)
#ccall zip_set_file_comment , Ptr <zip> -> CULLong -> CString -> CInt -> IO (CInt)

-- deprecated since 1.0
#ccall zip_error_get_sys_type , CInt -> IO (CInt)
#ccall zip_error_get , Ptr <zip> -> Ptr CInt -> Ptr CInt -> IO ()
#ccall zip_error_to_str , Ptr CChar -> CULLong -> CInt -> CInt -> IO (CInt)
#ccall zip_file_error_get , Ptr <zip_file> -> Ptr CInt -> Ptr CInt -> IO ()

-- the rest of the API
-- TODO: add new functions which appeared in 1.0

#ccall zip_archive_set_tempdir , Ptr <zip> -> CString -> IO (CInt)
#ccall zip_file_add , Ptr <zip> -> CString -> Ptr <zip_source> -> <zip_flags_t> -> IO (CLLong)
#ccall zip_dir_add , Ptr <zip> -> CString -> <zip_flags_t> -> IO (CLLong)
#ccall zip_close , Ptr <zip> -> IO (CInt)
#ccall zip_discard , Ptr <zip> -> IO ()
#ccall zip_delete , Ptr <zip> -> CULLong -> IO (CInt)
#ccall zip_file_extra_field_delete , Ptr <zip> -> CULLong -> CUShort -> <zip_flags_t> -> IO (CInt)
#ccall zip_file_extra_field_delete_by_id , Ptr <zip> -> CULLong -> CUShort -> CUShort -> <zip_flags_t> -> IO (CInt)
#ccall zip_error_clear , Ptr <zip> -> IO ()
#ccall zip_fclose , Ptr <zip_file> -> IO (CInt)
#ccall zip_fdopen , CInt -> CInt -> Ptr CInt -> IO (Ptr <zip>)
#ccall zip_file_error_clear , Ptr <zip_file> -> IO ()
#ccall zip_file_strerror , Ptr <zip_file> -> IO (CString)
#ccall zip_fopen , Ptr <zip> -> CString -> <zip_flags_t> -> IO (Ptr <zip_file>)
#ccall zip_fopen_encrypted , Ptr <zip> -> CString -> <zip_flags_t> -> CString -> IO (Ptr <zip_file>)
#ccall zip_fopen_index , Ptr <zip> -> CULLong -> <zip_flags_t> -> IO (Ptr <zip_file>)
#ccall zip_fopen_index_encrypted , Ptr <zip> -> CULLong -> <zip_flags_t> -> CString -> IO (Ptr <zip_file>)
#ccall zip_fread , Ptr <zip_file> -> Ptr () -> CULLong -> IO (CLLong)
#ccall zip_get_archive_comment , Ptr <zip> -> Ptr CInt -> <zip_flags_t> -> IO (CString)
#ccall zip_get_archive_flag , Ptr <zip> -> <zip_flags_t> -> <zip_flags_t> -> IO (CInt)
#ccall zip_file_get_comment , Ptr <zip> -> CULLong -> Ptr CUInt -> <zip_flags_t> -> IO (CString)
#ccall zip_file_extra_field_get , Ptr <zip> -> CULLong -> CUShort -> Ptr CUShort -> Ptr CUShort -> <zip_flags_t> -> IO (Ptr CUChar)
#ccall zip_file_extra_field_get_by_id , Ptr <zip> -> CULLong -> CUShort -> CUShort -> Ptr CUShort -> <zip_flags_t> -> IO (Ptr CUChar)
#ccall zip_file_extra_fields_count , Ptr <zip> -> CULLong -> <zip_flags_t> -> IO (CShort)
#ccall zip_file_extra_fields_count_by_id , Ptr <zip> -> CULLong -> CUShort -> <zip_flags_t> -> IO (CShort)
#ccall zip_get_name , Ptr <zip> -> CULLong -> <zip_flags_t> -> IO (CString)
#ccall zip_get_num_entries , Ptr <zip> -> <zip_flags_t> -> IO (CLLong)
#ccall zip_name_locate , Ptr <zip> -> CString -> <zip_flags_t> -> IO (CLLong)
#ccall zip_open , CString -> CInt -> Ptr CInt -> IO (Ptr <zip>)
#ccall zip_file_rename , Ptr <zip> -> CULLong -> CString -> <zip_flags_t> -> IO (CInt)
#ccall zip_file_replace , Ptr <zip> -> CULLong -> Ptr <zip_source> -> <zip_flags_t> -> IO (CInt)
#ccall zip_set_archive_comment , Ptr <zip> -> CString -> CUShort -> IO (CInt)
#ccall zip_set_archive_flag , Ptr <zip> -> <zip_flags_t> -> CInt -> IO (CInt)
#ccall zip_set_default_password , Ptr <zip> -> CString -> IO (CInt)
#ccall zip_file_set_comment , Ptr <zip> -> CULLong -> CString -> CUShort -> <zip_flags_t> -> IO (CInt)
#ccall zip_set_file_compression , Ptr <zip> -> CULLong -> CInt -> CUInt -> IO (CInt)
#ccall zip_file_extra_field_set , Ptr <zip> -> CULLong -> CUShort -> CUShort -> Ptr CUChar -> CUShort -> <zip_flags_t> -> IO (CInt)
#ccall zip_source_buffer , Ptr <zip> -> Ptr () -> CULLong -> CInt -> IO (Ptr <zip_source>)
#ccall zip_source_file , Ptr <zip> -> CString -> CULLong -> CLLong -> IO (Ptr <zip_source>)
#ccall zip_source_filep , Ptr <zip> -> Ptr CFile -> CULLong -> CLLong -> IO (Ptr <zip_source>)
#ccall zip_source_free , Ptr <zip_source> -> IO ()
#ccall zip_source_function , Ptr <zip> -> <zip_source_callback> -> Ptr () -> IO (Ptr <zip_source>)
#ccall zip_source_zip , Ptr <zip> -> Ptr <zip> -> CULLong -> <zip_flags_t> -> CULLong -> CLLong -> IO (Ptr <zip_source>)
#ccall zip_stat , Ptr <zip> -> CString -> <zip_flags_t> -> Ptr <zip_stat> -> IO (CInt)
#ccall zip_stat_index , Ptr <zip> -> CULLong -> <zip_flags_t> -> Ptr <zip_stat> -> IO (CInt)
#ccall zip_stat_init , Ptr <zip_stat> -> IO ()
#ccall zip_strerror , Ptr <zip> -> IO (CString)
#ccall zip_unchange , Ptr <zip> -> CULLong -> IO (CInt)
#ccall zip_unchange_all , Ptr <zip> -> IO (CInt)
#ccall zip_unchange_archive , Ptr <zip> -> IO (CInt)
