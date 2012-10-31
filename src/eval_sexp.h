#ifndef EVAL_SEXP_H
#define EVAL_SEXP_H

#include "types.h"
#include "env_types.h"

extern LispObject eval_sexp(Cons, Environment, Environment, BlockEnvironment, Environment);
extern void init_special_operators(void);

#endif
