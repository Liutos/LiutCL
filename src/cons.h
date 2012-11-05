#ifndef CONS_H
#define CONS_H

#include "types.h"

extern Cons make_cons(LispObject, LispObject);
extern LispObject safe_car(LispObject);
extern LispObject safe_cdr(LispObject);
extern int cons_length(Cons);
extern void free_cons(Cons);

#endif
