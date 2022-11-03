#include "elpcre.h"
#include <emacs-module.h>
#include <pcre2.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

int plugin_is_GPL_compatible;

char *elpcre_alloc_from_str(emacs_env *env, emacs_value value) {
  char *str;
  ptrdiff_t len;
  len = LENGTH(env, value) + 1;
  str = malloc(sizeof(char) * len);
  env->copy_string_contents(env, value, str, &len);
  return str;
}

emacs_value elpcre_make_list_from_match(emacs_env *env, pcre2_match_data *data) {
  emacs_value result;
  uint32_t cnt = pcre2_get_ovector_count(data);
  emacs_value *values = malloc(sizeof(emacs_value) * 2 * cnt);
  PCRE2_SIZE *vdata = pcre2_get_ovector_pointer(data);

  for (int i = 0; i < cnt; i++) {
    values[2 * i] = env->make_integer(env, vdata[2 * i]);
    values[2 * i + 1] = env->make_integer(env, vdata[2 * i + 1]);
  }


  result = env->funcall(env, env->intern(env, "list"), cnt * 2, values);
  free(values);
  return result;
}

pcre2_code *elpcre_compile_emacs_string(emacs_env *env, emacs_value string,
                                        uint32_t options, int *error_code,
                                        size_t *error_offset,
                                        pcre2_compile_context *context) {
  char *code;
  uint32_t flags;
  ptrdiff_t len;

  len = LENGTH(env, string) + 1;
  code = malloc(sizeof(char) * len);
  env->copy_string_contents(env, string, code, &len);

  /* printf("PATTERN: %s\n", code); */

  pcre2_code *res =
      pcre2_compile((const unsigned char *)code, PCRE2_ZERO_TERMINATED, flags,
                    error_code, error_offset, context);
  free(code);

  return res;
}

static emacs_value elpcre_compile(emacs_env *env, ptrdiff_t nargs,
                                  emacs_value args[], void *data) {
  emacs_value str;
  uint32_t flags;

  int ecode;
  size_t eoff;
  pcre2_compile_context *ctx = NULL;

  if (nargs < 2 || !STRINGP(env, args[0]) || !INTEGERP(env, args[1]))
    return env->intern(env, "nil");

  str = args[0];
  flags = env->extract_integer(env, args[1]);

  pcre2_code *res =
      elpcre_compile_emacs_string(env, str, flags, &ecode, &eoff, ctx);

  if (res)
    return env->make_user_ptr(env, &pcre2_code_finalizer, res);

  return CONS(env, env->make_integer(env, ecode), env->make_integer(env, eoff))
}

static emacs_value elpcre_match(emacs_env *env, ptrdiff_t nargs,
                                emacs_value args[], void *data) {

  // Required if the first arg isn't a pattern
  int ecode;
  size_t eoff;
  pcre2_compile_context *ctx = NULL;

  emacs_value expr, instr, offset;
  // For cleanup
  bool compiled_here = false;

  pcre2_match_data *match;
  char *string;
  pcre2_code *code;

  emacs_value final_result;

  bool return_raw;

  if (nargs < 3 || !STRINGP(env, args[1]) || !INTEGERP(env, args[2]))
    return env->intern(env, "nil");
  expr = args[0];
  instr = args[1];
  offset = args[2];

  if (nargs == 4 && env->is_not_nil(env, args[3])) {
    /* printf("WILL RETURN RAW: %d\n", nargs); */
    return_raw = true;
  }

  if (STRINGP(env, expr)) {
    code = elpcre_compile_emacs_string(env, expr, 0, &ecode, &eoff, ctx);
    if (!code)
      return CONS(env, env->make_integer(env, ecode),
                  env->make_integer(env, eoff));
    compiled_here = true;
  } else if (USER_PTRP(env, expr)) {
    code = (pcre2_code *)env->get_user_ptr(env, expr);
  }

  PCRE2_SIZE pcre_offset = env->extract_integer(env, offset);

  // Must be freed later
  match = pcre2_match_data_create_from_pattern(code, NULL);
  string = elpcre_alloc_from_str(env, instr);

  /* printf("VALUE: %s\n", string); */

  // Try to match
  int result = pcre2_match(code, string, PCRE2_ZERO_TERMINATED, pcre_offset, 0,
                           match, NULL);

  free(string);
  if (compiled_here)
    pcre2_code_free(code);

  if (result < 0) {
    pcre2_match_data_free(match);
    return env->make_integer(env, result);
  }

  if (return_raw) {
    return env->make_user_ptr(env, pcre2_match_finalizer, match);
  }
  /* printf("RESULT: %d", result); */
  // Extract match data
  final_result = elpcre_make_list_from_match(env, match);
  pcre2_match_data_free(match);

  return final_result;
}

static emacs_value elpcre_get_match_range(emacs_env *env, ptrdiff_t nargs,
                                          emacs_value args[], void *data) {
  pcre2_match_data *match_data;
  if (USER_PTRP(env, args[0])) {
    match_data = env->get_user_ptr(env, args[0]);
    return elpcre_make_list_from_match(env, match_data);
  }
  return env->intern(env, "nil");
}

/* Bind NAME to FUN.  */
static void bind_function(emacs_env *env, const char *name, emacs_value Sfun) {
  emacs_value Qfset = env->intern(env, "fset");
  emacs_value Qsym = env->intern(env, name);
  emacs_value args[] = {Qsym, Sfun};

  env->funcall(env, Qfset, 2, args);
}

/* Provide FEATURE to Emacs.  */
static void provide(emacs_env *env, const char *feature) {
  emacs_value Qfeat = env->intern(env, feature);
  emacs_value Qprovide = env->intern(env, "provide");
  emacs_value args[] = {Qfeat};

  env->funcall(env, Qprovide, 1, args);
}

int emacs_module_init(struct emacs_runtime *ert) {
  emacs_env *env = ert->get_environment(ert);
  bind_function(env, "elpcre-match",
                env->make_function(env, 3, 4, elpcre_match, "doc", NULL));

  bind_function(
      env, "elpcre-match-range",
      env->make_function(env, 1, 1, elpcre_get_match_range, "doc", NULL));

  /* bind_function(env, "elpcre-match-group-number", */
  /*               env->make_function(env, 2, 2, elpcre_get_match_group_number, */
  /*                                  "doc", NULL)); */

  provide(env, "el-pcre-lib");
  return 0;
}
