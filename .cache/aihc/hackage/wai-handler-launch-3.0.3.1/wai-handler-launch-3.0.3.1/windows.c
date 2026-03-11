#include <windows.h>
#include <shellapi.h>
#include <stdio.h>

void launch(char *s)
{
    ShellExecute(NULL, "open", s, NULL, NULL, SW_SHOWNORMAL);
}
