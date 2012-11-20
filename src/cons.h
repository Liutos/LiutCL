#ifndef CONS_H
#define CONS_H

#include "types.h"

extern Cons make_cons(LispObject, LispObject);
extern List make_list(LispObject, LispObject);
extern void free_cons(Cons);
extern LispObject car(Cons);
extern LispObject cdr(Cons);
extern void set_car(Cons, LispObject);
extern void set_cdr(Cons, LispObject);
extern BOOL consp(LispObject);

extern Cons nth_cdr(unsigned int, List);
extern LispObject assoc(LispObject, List);
extern LispObject get_by_key(LispObject, List);
extern LispObject get_by_keyword(char *, List);
extern LispObject nth_car(unsigned int, List);
extern List make_list_aux(int, LispObject);
extern int cons_length(Cons);
extern void set_by_key(Symbol, LispObject, List);
extern void set_nth(unsigned int, List, LispObject);

#endif
