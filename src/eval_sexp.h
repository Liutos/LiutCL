#ifndef EVAL_SEXP_H
#define EVAL_SEXP_H

#include "env_types.h"
#include "types.h"

extern LispObject eval_sexp(Cons, Environment, Environment, BlockEnvironment, Environment);
extern void describe_global_stack(void);
extern void init_special_operators(void);

#endif
