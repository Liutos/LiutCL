#ifndef EVAL_SEXP_H
#define EVAL_SEXP_H

#include "types.h"

extern LispObject eval_sexp(Cons, Environment, Environment);
extern void init_special_operators(void);

#endif
