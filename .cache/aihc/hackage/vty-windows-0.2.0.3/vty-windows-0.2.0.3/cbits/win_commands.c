#include <windows.h>
#include <stdlib.h>
#include <stdio.h>
#include <wchar.h>

int set_screen_size(int x, int y, HANDLE hOut)
{
    COORD coord = { x, y };
    return SetConsoleScreenBufferSize(hOut, coord);
}
