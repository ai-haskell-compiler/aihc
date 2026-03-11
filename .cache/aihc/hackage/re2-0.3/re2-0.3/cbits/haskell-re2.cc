#include "haskell-re2.h"

#include <cstddef>
#include <cstdlib>

#include <re2/re2.h>
#include <re2/set.h>

#define HSRE2_MALLOC(type, count) static_cast<type*>(malloc(sizeof(type)*count))

extern "C" {

re2::RE2::Options *haskell_re2_alloc_options() {
  re2::RE2::Options* opts = new re2::RE2::Options();
  opts->set_log_errors(false);
  return opts;
}

void haskell_re2_free_options(re2::RE2::Options *opts) {
  delete opts;
}

#define IMPL_SETOPT(optName, type) void haskell_re2_setopt_##optName(re2::RE2::Options *opts, type val) { \
  opts->set_##optName(val); \
}

void haskell_re2_setopt_encoding(re2::RE2::Options *opts, int val) {
  opts->set_encoding(re2::RE2::Options::Encoding(val));
}

IMPL_SETOPT(posix_syntax, bool);
IMPL_SETOPT(longest_match, bool);
IMPL_SETOPT(max_mem, int64_t);
IMPL_SETOPT(literal, bool);
IMPL_SETOPT(never_nl, bool);
IMPL_SETOPT(dot_nl, bool);
IMPL_SETOPT(never_capture, bool);
IMPL_SETOPT(case_sensitive, bool);
IMPL_SETOPT(perl_classes, bool);
IMPL_SETOPT(word_boundary, bool);
IMPL_SETOPT(one_line, bool);

#undef IMPL_SETOPT

re2::RE2 *haskell_re2_compile_pattern(re2::RE2::Options* opts, const char *input, int input_len) {
	return new re2::RE2(re2::StringPiece(input, input_len), *opts);
}

void haskell_re2_delete_pattern(re2::RE2 *regex) {
	delete regex;
}

const char *haskell_re2_error(re2::RE2 *regex) {
	const std::string &err = regex->error();
	if (err.size() == 0) {
		return NULL;
	}
	return err.c_str();
}

int haskell_re2_error_code(re2::RE2 *regex) {
	return regex->error_code();
}

const char *haskell_re2_pattern_input(re2::RE2 *regex) {
	return regex->pattern().c_str();
}

int haskell_re2_pattern_groups(re2::RE2 *regex, char ***group_names, size_t **group_name_lens) {
	int count = regex->NumberOfCapturingGroups();
	if (count == 0) {
		return count;
	}
	*group_names = HSRE2_MALLOC(char*, count);
	*group_name_lens = HSRE2_MALLOC(size_t, count);
	typedef std::map<int, std::string> Names;
	const Names& names = regex->CapturingGroupNames();
	for (int ii = 0; ii < count; ii++) {
		Names::const_iterator iter = names.find(ii+1);
		if (iter == names.end()) {
			(*group_names)[ii] = NULL;
			(*group_name_lens)[ii] = 0;
		} else {
			const std::string& s = iter->second;
			(*group_name_lens)[ii] = s.size();
			(*group_names)[ii] = HSRE2_MALLOC(char, s.size());
			memcpy((*group_names)[ii], s.c_str(), s.size());
		}
	}
	return count;
}

int haskell_re2_program_size(re2::RE2 *regex) {
	return regex->ProgramSize();
}

void haskell_re2_quote_meta(const char *in, int in_len, char **out, size_t *out_len) {
	std::string quoted = re2::RE2::QuoteMeta(re2::StringPiece(in, in_len));
	*out_len = quoted.size();
	*out = HSRE2_MALLOC(char, quoted.size());
	memcpy(*out, quoted.c_str(), quoted.size());
}

bool haskell_re2_replace(re2::RE2 *regex, const char *in, size_t in_len, const char *rewrite, int rewrite_len, char **out, size_t *out_len) {
	std::string str(in, in_len);
	if (re2::RE2::Replace(&str, *regex, re2::StringPiece(rewrite, rewrite_len))) {
		*out_len = str.size();
		*out = HSRE2_MALLOC(char, str.size());
		memcpy(*out, str.c_str(), str.size());
		return true;
	}
	return false;
}

void haskell_re2_global_replace(re2::RE2 *regex, const char *in, size_t in_len, const char *rewrite, int rewrite_len, char **out, size_t *out_len, int *count) {
	std::string str(in, in_len);
	*count = re2::RE2::GlobalReplace(&str, *regex, re2::StringPiece(rewrite, rewrite_len));
	if (*count > 0) {
		*out_len = str.size();
		*out = HSRE2_MALLOC(char, str.size());
		memcpy(*out, str.c_str(), str.size());
	}
}

bool haskell_re2_extract(re2::RE2 *regex, const char *in, int in_len, const char *rewrite, int rewrite_len, char **out, size_t *out_len) {
	std::string str;
	if (re2::RE2::Extract(re2::StringPiece(in, in_len), *regex, re2::StringPiece(rewrite, rewrite_len), &str)) {
		*out_len = str.size();
		*out = HSRE2_MALLOC(char, str.size());
		memcpy(*out, str.c_str(), str.size());
		return true;
	}
	return false;
}

bool haskell_re2_match(re2::RE2 *regex, const char *in, int in_len, int startpos, int endpos, int anchor, int num_captures, char ***captures, size_t **capture_lens, int *capture_count) {
	if (num_captures < 0) {
		num_captures = regex->NumberOfCapturingGroups() + 1;
	}
	*capture_count = num_captures;
	re2::StringPiece *vec = new re2::StringPiece[num_captures];
	if (!regex->Match(re2::StringPiece(in, in_len), startpos, endpos, re2::RE2::Anchor(anchor), vec, num_captures)) {
		delete[] vec;
		return false;
	}
	if (num_captures > 0) {
		*captures = HSRE2_MALLOC(char*, num_captures);
		*capture_lens = HSRE2_MALLOC(size_t, num_captures);
		for (int ii = 0; ii < num_captures; ii++) {
			if (vec[ii].data() == NULL) {
				(*captures)[ii] = NULL;
				(*capture_lens)[ii] = 0;
			} else {
				(*captures)[ii] = const_cast<char*>(vec[ii].data());
				(*capture_lens)[ii] = vec[ii].size();
			}
		}
	}
	delete[] vec;
	return true;
}

}
