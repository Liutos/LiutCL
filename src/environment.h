#ifndef ENVIRONMENT_H
#define ENVIRONMENT_H

#include <setjmp.h>

#include "env_types.h"
#include "types.h"

extern BlockEnvironment make_block_env(Symbol, jmp_buf, BlockEnvironment);
extern Environment extend_cons_binding(Cons, Cons, Environment);
extern Environment extend_env(Symbol, LispObject, Environment);
extern Environment extend_env_by_name(char *, Package, LispObject, Environment);
extern Environment make_empty_env(void);
extern Environment make_new_env(Cons, Cons, Environment);
extern LispObject get_value(Symbol, Environment);
extern void describe_env(Environment, Stream);

extern Environment global_constant_env;
extern Environment global_dynamic_env;

#endif
