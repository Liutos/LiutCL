#ifndef CONS_H
#define CONS_H

#include "types.h"

extern Cons make_cons(LispObject, LispObject);
extern Cons nth_cdr(unsigned int, List);
extern LispObject get_by_key(LispObject, List);
extern LispObject nth_car(unsigned int, List);
extern List make_list(int, LispObject);
extern int cons_length(Cons);
extern void free_cons(Cons);
extern void set_nth(unsigned int, List, LispObject);

#endif
