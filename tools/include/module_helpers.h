#ifndef MODULE_HELPERS_H
#define MODULE_HELPERS_H

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

#endif
