#ifndef ATOM_PROC_H
#define ATOM_PROC_H

#include "types.h"
#include "env_types.h"

extern Symbol make_symbol(char *);
extern Symbol ensure_symbol_exists(char *);
extern Function make_c_fun_object(primitive_t);
extern BOOL is_true_obj(LispObject);
extern Function make_i_fun_object(Cons, LispObject, Environment, Environment, BlockEnvironment);
extern BOOL is_atom_object(LispObject);
extern BOOL is_tail(LispObject);
extern BOOL is_symbol(LispObject);
extern Character make_char(char);

extern Symbol lt_nil;
extern Symbol lt_t;
extern Symbol lt_quote, lt_if, lt_begin, lt_set, lt_lambda;

extern Symbol lt_dset;
extern Symbol lt_dynamic;
extern Symbol lt_catch;
extern Symbol lt_throw;
extern Symbol lt_block;
extern Symbol lt_return_from;

#endif
