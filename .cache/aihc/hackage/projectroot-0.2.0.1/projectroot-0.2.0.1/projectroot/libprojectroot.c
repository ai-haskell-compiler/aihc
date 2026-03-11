#include <dirent.h>
#include <glob.h>
#include <libgen.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "libprojectroot.h"

int weight_mode_on = 0;
int verbose_on = 0;

static const char* const candidates[] = {
  ".git",
  ".hg",
  ".fslckout",
  ".bzr",
  "_darcs",
  ".svn",
  "CVS",
  "rebar.config",
  "project.clj",
  "SConstruct",
  "pom.xml",
  "build.sbt",
  "build.gradle",
  "Gemfile",
  "requirements.txt",
  "tox.ini",
  "package.json",
  "node_modules",
  "gulpfile.js",
  "Gruntfile.js",
  "bower.json",
  "composer.json",
  "Cargo.toml",
  "stack.yaml",
  ".cabal-sandbox",
  "*.cabal",
  "mix.exs",
};

int is_project_root(char* cdt) {
  char pattern[1024];
  glob_t res;
  size_t cdt_size = strlen(cdt);

  strcpy(pattern, cdt);
  strcat(pattern, "/");

  for(int i = 0; i < N_CANDIDATES; i++) {
    strcat(pattern, candidates[i]);
    if(verbose_on) fprintf(stderr, "Matching: %s ~= %s\n", cdt, pattern);
    if(glob(pattern, GLOB_NOSORT, NULL, &res) == 0) {
      if(verbose_on) fprintf(stderr, "--> Matched (weight %d)\n", N_CANDIDATES - i + 1);
      return N_CANDIDATES - i + 1;
    }
    pattern[cdt_size + 1] = 0;
  }

  return 0;
}

char* find_project_root_weighted(char* cwd) {
  char* cdt = cwd;
  char* best_cdt = malloc(sizeof(char) * 1024);
  int best_cdt_weight = 0;
  int cdt_weight;

  while(strcmp(cdt, "/") != 0) {
    if((cdt_weight = is_project_root(cdt)) && cdt_weight > best_cdt_weight) {
      best_cdt_weight = cdt_weight;
      strcpy(best_cdt, cdt);
    }
    cdt = dirname(cdt);
  }

  return best_cdt;
}

char* find_project_root(char* cwd) {
  char* cdt = cwd;

  while(strcmp(cdt, "/") != 0) {
    if(is_project_root(cdt)) return cdt;
    cdt = dirname(cdt);
  }

  return NULL;
}
