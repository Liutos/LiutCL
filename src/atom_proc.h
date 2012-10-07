#ifndef ATOM_PROC_H
#define ATOM_PROC_H

#include "types.h"

extern Symbol make_symbol(char *);
extern Symbol ensure_symbol_exists(char *);
extern Function make_c_fun_object(primitive_t);
extern BOOL is_true_obj(LispObject);
extern Function make_i_fun_object(Cons, LispObject, Environment);

#endif
