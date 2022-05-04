#ifndef ELPCRE_H
#define ELPCRE_H

#define PCRE2_CODE_UNIT_WIDTH 8
#include "pcre2.h"

#define STRINGP(env, value)                                                    \
  env->eq(env, env->type_of(env, value), env->intern(env, "string"))

#define USER_PTRP(env, value)                                                    \
  env->eq(env, env->type_of(env, value), env->intern(env, "user-ptr"))

#define INTEGERP(env, value)                                                   \
  env->eq(env, env->type_of(env, value), env->intern(env, "integer"))

#define LENGTH(env, value)                                                     \
  env->extract_integer(env, env->funcall(env, env->intern(env, "length"), 1,   \
                                         (emacs_value[]){value}))

#define CONS(env, value1, value2)                                              \
  env->funcall(env, env->intern(env, "cons"), 2,                               \
               (emacs_value[]){value1, value2});

void pcre2_code_finalizer(void *code) { pcre2_code_free((pcre2_code *)code); }
void pcre2_match_finalizer(void *match) { pcre2_match_data_free((pcre2_match_data *)match); }

#endif
