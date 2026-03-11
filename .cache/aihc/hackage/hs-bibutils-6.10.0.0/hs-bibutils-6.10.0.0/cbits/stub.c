#include <stdio.h>

FILE *c_stdout()
{
    return stdout;
}

FILE *c_stdin()
{
    return stdin;
}

const char progname[] = "hs-bibutils";
