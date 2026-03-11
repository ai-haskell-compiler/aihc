#include <Windows.h>
#include <stdlib.h>
#include <stdio.h>
#include <wchar.h>

int create_pseudo_console(COORD size, HANDLE outputReadHandle, HANDLE inputWriteHandle)
{
    int result = S_OK;

    // Create communication channels

    // - Close these after CreateProcess of child application with pseudoconsole object.
    HANDLE inputReadSide, outputWriteSide;

    if (!CreatePipe(&inputReadSide, &inputWriteHandle, NULL, 0))
    {
        return result;
        // return HRESULT_FROM_WIN32(GetLastError());
    }

    if (!CreatePipe(&outputReadHandle, &outputWriteSide, NULL, 0))
    {
        return result;
        // return HRESULT_FROM_WIN32(GetLastError());
    }

    // TODO: Figure out how to call the CreatePseudoConsole function! Haskell compiler can't find HPCON and 
    // CreatePseudoConsole. Maybe add HPCON and CreatePseudoConsole to the Win32 API?
    // HPCON hPC;
    // result = CreatePseudoConsole(size, inputReadSide, outputWriteSide, 0, &hPC);

    return result;
}