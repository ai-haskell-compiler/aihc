#include <dirent.h>
#include <glob.h>
#include <libgen.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define N_CANDIDATES 26

int weight_mode_on;
int verbose_on;

int is_project_root(char* cdt);
char* find_project_root_weighted(char* cwd);
char* find_project_root(char* cwd);
