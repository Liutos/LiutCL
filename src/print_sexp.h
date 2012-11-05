#ifndef PRINT_SEXP_H
#define PRINT_SEXP_H

#include "types.h"

extern void print_atom(Atom, Stream);
extern void print_cons(Cons, Stream);
extern void print_object(LispObject, Stream);

#endif
