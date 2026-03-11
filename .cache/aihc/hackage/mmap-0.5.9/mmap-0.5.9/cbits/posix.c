#include "HsMmap.h"

#define _LARGEFILE64_SOURCE 1
#define _FILE_OFFSET_BITS 64

#include <stdint.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/errno.h>

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
    void *handle = NULL;
    int access, fd;
    if( !filepath )
        return NULL;
    switch(mode) {
    case 0:
	access = O_RDONLY;
	break;
    case 1:
	access = O_RDWR;
	break;
    case 2:
	access = O_RDONLY;
	break;
    case 3:
	access = O_RDWR|O_CREAT;
	break;
    default:
	return NULL;
    }
#ifdef O_NOCTTY
    access |= O_NOCTTY;
#endif
#ifdef O_LARGEFILE
    access |= O_LARGEFILE;
#endif
#ifdef O_NOINHERIT
    access |= O_NOINHERIT;
#endif
    fd = open(filepath,access,0666);
    if( fd == -1 ) {
        return NULL;
    }
#ifdef _DEBUG
    counters++;
#endif
    handle = (void *)((intptr_t)fd + 1);
    return handle;
}

//foreign import ccall unsafe "system_io_mmap_file_close" c_system_io_mmap_file_close :: FunPtr(Ptr () -> IO ())
void system_io_mmap_file_close(void *handle)
{
    int fd = (int)(intptr_t)handle - 1;
    close(fd);
#ifdef _DEBUG
    counters--;
#endif
}

static char zerolength[1];

//foreign import ccall unsafe "system_io_mmap_mmap" c_system_io_mmap_mmap :: Ptr () -> CInt -> CLLong -> CInt -> IO (Ptr ())
void *system_io_mmap_mmap(void *handle, int mode, long long offset, size_t size)
{
    void *ptr = NULL;
    int prot;
    int flags;
    int fd = (int)(intptr_t)handle - 1;

    switch(mode) {
    case 0:
	prot = PROT_READ;
	flags = MAP_PRIVATE;
	break;
    case 1:
	prot = PROT_READ|PROT_WRITE;
	flags = MAP_SHARED;
	break;
    case 2:
	prot = PROT_READ|PROT_WRITE;
	flags = MAP_PRIVATE;
	break;
    case 3:
	prot = PROT_READ|PROT_WRITE;
	flags = MAP_SHARED;
	break;
    default:
	return NULL;
    }

    if( size>0 ) {
        ptr = mmap(NULL,size,prot,flags,fd,offset);

        if( ptr == MAP_FAILED ) {
	    return NULL;
        }
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

void system_io_mmap_munmap(void *sizeasptr, void *ptr) // Ptr CInt -> Ptr a -> IO ()
{
    size_t size = (size_t)sizeasptr;
    int result = 0;
    if( size>0 ) {
        result = munmap(ptr,size);
#ifdef _DEBUG
        if( result==0 ) {
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
    int fd = (int)(intptr_t)handle - 1;
    struct stat st;
    fstat(fd,&st);
    return st.st_size;
}

int system_io_mmap_extend_file_size(void *handle, long long size)
{
    int fd = (int)(intptr_t)handle - 1;
    return ftruncate(fd,size);
}


//foreign import ccall unsafe "system_io_mmap_granularity" c_system_io_granularity :: CInt
int system_io_mmap_granularity()
{
    return getpagesize();
}
