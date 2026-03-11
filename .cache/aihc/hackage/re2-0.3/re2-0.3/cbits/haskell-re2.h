#ifndef HASKELL_RE2_H
#define HASKELL_RE2_H

#include <stddef.h>
#include <stdint.h>

#include <re2/re2.h>
#include <re2/set.h>

#ifdef __cplusplus
extern "C" {
#endif  // __cplusplus

re2::RE2::Options *haskell_re2_alloc_options();

void haskell_re2_free_options(re2::RE2::Options *opts);

void haskell_re2_setopt_encoding(re2::RE2::Options *opts, int);
void haskell_re2_setopt_posix_syntax(re2::RE2::Options *opts, bool);
void haskell_re2_setopt_longest_match(re2::RE2::Options *opts, bool);
void haskell_re2_setopt_max_mem(re2::RE2::Options *opts, int64_t);
void haskell_re2_setopt_literal(re2::RE2::Options *opts, bool);
void haskell_re2_setopt_never_nl(re2::RE2::Options *opts, bool);
void haskell_re2_setopt_dot_nl(re2::RE2::Options *opts, bool);
void haskell_re2_setopt_never_capture(re2::RE2::Options *opts, bool);
void haskell_re2_setopt_case_sensitive(re2::RE2::Options *opts, bool);
void haskell_re2_setopt_perl_classes(re2::RE2::Options *opts, bool);
void haskell_re2_setopt_word_boundary(re2::RE2::Options *opts, bool);
void haskell_re2_setopt_one_line(re2::RE2::Options *opts, bool);

re2::RE2 *haskell_re2_compile_pattern(re2::RE2::Options *opts, const char *input, int input_len);

void haskell_re2_delete_pattern(re2::RE2 *regex);

const char *haskell_re2_error(re2::RE2 *regex);

int haskell_re2_error_code(re2::RE2 *regex);

const char *haskell_re2_pattern_input(re2::RE2 *regex);

int haskell_re2_pattern_groups(re2::RE2 *regex, char ***group_names, size_t **group_name_lens);

int haskell_re2_program_size(re2::RE2 *regex);

void haskell_re2_quote_meta(const char *in, int in_len, char **out, size_t *out_len);

bool haskell_re2_replace(re2::RE2 *regex, const char *in, size_t in_len, const char *rewrite, int rewrite_len, char **out, size_t *out_len);

void haskell_re2_global_replace(re2::RE2 *regex, const char *in, size_t in_len, const char *rewrite, int rewrite_len, char **out, size_t *out_len, int *count);

bool haskell_re2_extract(re2::RE2 *regex, const char *in, int in_len, const char *rewrite, int rewrite_len, char **out, size_t *out_len);

bool haskell_re2_match(re2::RE2 *regex, const char *in, int in_len, int startpos, int endpos, int anchor, int num_captures, char ***captures, size_t **capture_lens, int *capture_count);

#ifdef __cplusplus
}
#endif  // __cplusplus

#endif  // HASKELL_RE2_H
