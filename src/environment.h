#ifndef ENVIRONMENT_H
#define ENVIRONMENT_H

#include "types.h"
#include "env_types.h"
#include <setjmp.h>

extern LispObject get_value(Symbol, Environment);
extern Environment put_binding(Symbol, LispObject, Environment);
extern Environment new_env(void);
extern Environment init_primitives(Environment);
extern void describe_env(Environment);
extern Environment extend_cons_binding(Cons, Cons, Environment);
extern Environment extend_binding(Symbol, LispObject, Environment);
extern Environment new_binding_env(Cons, Cons, Environment);
extern Environment init_variables(Environment);
extern SymValMap make_single_map(Symbol, LispObject);
extern BlockEnvironment make_block_env(Symbol, jmp_buf, BlockEnvironment);

#endif
