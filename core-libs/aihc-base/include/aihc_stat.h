/*
 * The struct stat accessors of System.Posix.Internals.
 *
 * A Haskell module cannot read a field of a C structure or ask for its
 * size. These inline functions do that against the platform headers, and
 * each capi import of System.Posix.Internals calls one of them.
 */
#ifndef AIHC_STAT_H
#define AIHC_STAT_H

#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>

#define AIHC_SIZEOF_STAT ((int)sizeof(struct stat))

static inline mode_t aihc_st_mode(struct stat *st) { return st->st_mode; }
static inline off_t aihc_st_size(struct stat *st) { return st->st_size; }
static inline time_t aihc_st_mtime(struct stat *st) { return st->st_mtime; }
static inline dev_t aihc_st_dev(struct stat *st) { return st->st_dev; }
static inline ino_t aihc_st_ino(struct stat *st) { return st->st_ino; }

static inline int aihc_s_isreg(mode_t mode) { return S_ISREG(mode); }
static inline int aihc_s_isdir(mode_t mode) { return S_ISDIR(mode); }
static inline int aihc_s_ischr(mode_t mode) { return S_ISCHR(mode); }
static inline int aihc_s_isblk(mode_t mode) { return S_ISBLK(mode); }
static inline int aihc_s_isfifo(mode_t mode) { return S_ISFIFO(mode); }
static inline int aihc_s_issock(mode_t mode) { return S_ISSOCK(mode); }

#endif
