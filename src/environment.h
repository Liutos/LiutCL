#ifndef ENVIRONMENT_H
#define ENVIRONMENT_H

#include "types.h"

extern LispObject get_value(Symbol, Environment);
extern Environment put_binding(Symbol, LispObject, Environment);
extern Environment new_env(void);
extern Environment init_environment(Environment);
extern void describe_env(Environment);
extern Environment extend_cons_binding(Cons, Cons, Environment);

#endif
