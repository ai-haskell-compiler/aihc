#include "HsMmap.h"
#include <windows.h>

#ifdef _DEBUG
int counters = 0;

int system_io_mmap_counters()
{
    return counters;
}
#endif


//foreign import ccall unsafe "system_io_mmap_file_open" c_system_io_mmap_file_open :: CString -> CInt -> IO (Ptr ())
void *system_io_mmap_file_open(const char *filepath, int mode)
{
    /*
    HANDLE WINAPI CreateFileA(
      __in      LPCTSTR lpFileName,
      __in      DWORD dwDesiredAccess,
      __in      DWORD dwShareMode,
      __in_opt  LPSECURITY_ATTRIBUTES lpSecurityAttributes,
      __in      DWORD dwCreationDisposition,
      __in      DWORD dwFlagsAndAttributes,
      __in_opt  HANDLE hTemplateFile
    );
    */
    void *handle = NULL;
    DWORD dwDesiredAccess;
    DWORD dwCreationDisposition;
    if( !filepath )
        return NULL;
    switch(mode) {
        case 0:
            dwDesiredAccess = GENERIC_READ;
            dwCreationDisposition = OPEN_EXISTING;
            break;
        case 1:
            dwDesiredAccess = GENERIC_WRITE|GENERIC_READ;
            dwCreationDisposition = OPEN_EXISTING;
            break;
        case 2:
            dwDesiredAccess = GENERIC_READ;
            dwCreationDisposition = OPEN_EXISTING;
            break;
        case 3:
            dwDesiredAccess = GENERIC_WRITE|GENERIC_READ;
            dwCreationDisposition = OPEN_ALWAYS;
            break;
        default:
            return NULL;
    }
    handle = CreateFileA(filepath,
                         dwDesiredAccess,
                         FILE_SHARE_READ|FILE_SHARE_WRITE|FILE_SHARE_DELETE,
                         NULL,
                         dwCreationDisposition,
                         FILE_ATTRIBUTE_NORMAL,
                         NULL);
    if( handle==INVALID_HANDLE_VALUE )
        return NULL;
#ifdef _DEBUG
    counters++;
#endif
    return handle;
}

//foreign import ccall unsafe "system_io_mmap_file_close" c_system_io_mmap_file_close :: FunPtr(Ptr () -> IO ())
void system_io_mmap_file_close(void *handle)
{
    CloseHandle(handle);
#ifdef _DEBUG
    counters--;
#endif
}

static char zerolength[1];

//foreign import ccall unsafe "system_io_mmap_mmap" c_system_io_mmap_mmap :: Ptr () -> CInt -> CLLong -> CInt -> IO (Ptr ())
void *system_io_mmap_mmap(void *handle, int mode, long long offset, size_t size)
{
    /*
    HANDLE WINAPI CreateFileMapping(
      __in      HANDLE hFile,
      __in_opt  LPSECURITY_ATTRIBUTES lpAttributes,
      __in      DWORD flProtect,
      __in      DWORD dwMaximumSizeHigh,
      __in      DWORD dwMaximumSizeLow,
      __in_opt  LPCTSTR lpName
    );
    LPVOID WINAPI MapViewOfFile(
      __in  HANDLE hFileMappingObject,
      __in  DWORD dwDesiredAccess,
      __in  DWORD dwFileOffsetHigh,
      __in  DWORD dwFileOffsetLow,
      __in  SIZE_T dwNumberOfBytesToMap
    );
    */
    HANDLE mapping;
    void *ptr = NULL;
    DWORD flProtect;
    DWORD dwDesiredAccess;
    switch(mode) {
        case 0:
            flProtect = PAGE_READONLY;
            dwDesiredAccess = FILE_MAP_READ;
            break;
        case 1:
            flProtect = PAGE_READWRITE;
            dwDesiredAccess = FILE_MAP_WRITE;
            break;
        case 2:
            flProtect = PAGE_WRITECOPY;
            dwDesiredAccess = FILE_MAP_COPY;
            break;
        case 3:
            flProtect = PAGE_READWRITE;
            dwDesiredAccess = FILE_MAP_WRITE;
            break;
        default:
            return NULL;
    }
    
    if( size>0 ) {

        mapping = CreateFileMapping(handle, NULL, flProtect, (DWORD) ((offset + size)>>32), (DWORD)(offset + size), NULL);
        if( !mapping ) {
            // FIXME: check error code and translate this to errno
            // DWORD dw = GetLastError();
        }
        ptr = MapViewOfFile(mapping,dwDesiredAccess, (DWORD)(offset>>32), (DWORD)(offset), size );
        if( !ptr ) {
            // FIXME: check error code and translate this to errno
            // DWORD dw = GetLastError();
        }
        CloseHandle(mapping);
    }
    else {
        ptr = zerolength;
    }
#ifdef _DEBUG
    if( ptr ) {
        counters++;
    }
#endif
    return ptr;
}

/*
 * MSDN states:
 *
 * Although an application may close the file handle used to create a file mapping object,
 * the system holds the corresponding file open until the last view of the file is unmapped:
 *
 * Files for which the last view has not yet been unmapped are held open with no sharing restrictions.
 *
 * Who knows what this means?
 *
 * http://msdn.microsoft.com/en-us/library/aa366882(VS.85).aspx
 */
void system_io_mmap_munmap(void *sizeasptr, void *ptr) // Ptr () -> Ptr a -> IO ()
{
    size_t size = (size_t)sizeasptr;
    BOOL result;
    if( size>0 ) {
        result = UnmapViewOfFile(ptr);
#ifdef _DEBUG
        if( result ) {
            counters--;
        }
#endif
    }
    else {
#ifdef _DEBUG
        counters--;
#endif
    }
}

//foreign import ccall unsafe "system_io_mmap_file_size" c_system_io_file_size :: Ptr () -> IO (CLLong)
long long system_io_mmap_file_size(void *handle)
{
    DWORD lobits, hibits;
    lobits = GetFileSize(handle,&hibits);
    return (long long)lobits + ((long long)hibits << 32);
}

int system_io_mmap_extend_file_size(void *handle, long long size)
{
    DWORD lobits = (DWORD)size, hibits = (DWORD)(size>>32);
    HANDLE mapping = CreateFileMapping(handle,NULL,PAGE_READWRITE,hibits,lobits,NULL);
    if(mapping==NULL)
        return -1;
    CloseHandle(mapping);
    return 0;
}

//foreign import ccall unsafe "system_io_mmap_granularity" c_system_io_granularity :: CInt
int system_io_mmap_granularity()
{
    SYSTEM_INFO sysinfo;
    GetSystemInfo(&sysinfo);
    return sysinfo.dwAllocationGranularity;
}


