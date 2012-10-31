#ifndef PRINT_SEXP_H
#define PRINT_SEXP_H

#include "types.h"

extern void print_sexp(LispObject);
extern void print_cons(Cons);
extern void print_atom(Atom);

#endif
