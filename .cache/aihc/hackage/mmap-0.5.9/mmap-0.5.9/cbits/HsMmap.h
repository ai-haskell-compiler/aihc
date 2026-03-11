#ifndef __HSMMAP_H__
#define __HSMMAP_H__

#include <stddef.h>

void *system_io_mmap_file_open(const char *filepath, int mode);

void system_io_mmap_file_close(void *handle);

void *system_io_mmap_mmap(void *handle, int mode, long long offset, size_t size);

void system_io_mmap_munmap(void *sizeasptr, void *ptr);

long long system_io_mmap_file_size(void *handle);

int system_io_mmap_extend_file_size(void *handle, long long size);

int system_io_mmap_granularity();

// this is only implemented in _DEBUG builds
int system_io_mmap_counters();


#endif /* __HSMMAP_H__ */
