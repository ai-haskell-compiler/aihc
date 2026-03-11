/* -----------------------------------------------------------------------------
 *
 * Module      :  C support for Graphics.Rendering.GLU.Raw.Extensions
 * Copyright   :  (c) Sven Panne 2018
 * License     :  BSD3
 *
 * Maintainer  :  Sven Panne <svenpanne@gmail.com>
 * Stability   :  stable
 * Portability :  portable
 *
 * -------------------------------------------------------------------------- */

#if defined(USE_GETPROCADDRESS)

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

void*
hs_GLU_getProcAddress(const char *name)
{
  static int firstTime = 1;
  static HMODULE handle = NULL;

  if (firstTime) {
    firstTime = 0;
    handle = LoadLibrary(TEXT("glu32"));
  }

  return handle ? GetProcAddress(handle, name) : NULL;
}

/* -------------------------------------------------------------------------- */
#elif defined(USE_NSADDRESSOFSYMBOL)

#include <mach-o/dyld.h>
#include <stdlib.h>
#include <string.h>

void*
hs_GLU_getProcAddress(const char *name)
{
  NSSymbol symbol;

  /* Prepend a '_' for the Unix C symbol mangling convention */
  char* symbolName = (char*)malloc(strlen(name) + 2);
  if (!symbolName) {
    return NULL;
  }
  symbolName[0] = '_';
  strcpy(symbolName + 1, name);

  if (!NSIsSymbolNameDefined(symbolName)) {
    free(symbolName);
    return NULL;
  }

  symbol = NSLookupAndBindSymbol(symbolName);
  free(symbolName);
  if (!symbol) {
    return NULL;
  }

  return NSAddressOfSymbol(symbol);
}

/* -------------------------------------------------------------------------- */
#elif defined(USE_DLSYM)

#include <stdlib.h>
#include <dlfcn.h>

static const char* libNames[] = {
#ifdef __APPLE__
  /* Try public framework path first. */
  "/Library/Frameworks/OpenGL.framework/OpenGL",
  /* If the public path failed, try the system framework path. */
  "/System/Library/Frameworks/OpenGL.framework/OpenGL"
#else
  "libGLU.so", "libGLU.so.1"
#endif
};

void*
hs_GLU_getProcAddress(const char *name)
{
  static int firstTime = 1;
  static void *handle = NULL;

  if (firstTime) {
    int i, numNames = (int)(sizeof(libNames) / sizeof(libNames[0]));
    firstTime = 0;
    for (i = 0;   (!handle) && (i < numNames);   ++i) {
      handle = dlopen(libNames[i], RTLD_LAZY | RTLD_GLOBAL);
    }
  }

  return handle ? dlsym(handle, name) : NULL;
}

/* -------------------------------------------------------------------------- */
#else

#error "Don't know how to retrieve GLU entries"

#endif
