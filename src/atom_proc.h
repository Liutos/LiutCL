#ifndef ATOM_PROC_H
#define ATOM_PROC_H

#include "env_types.h"
#include "types.h"

extern BOOL is_atom_object(LispObject);
extern BOOL is_symbol(LispObject);
extern BOOL is_tail(LispObject);
extern BOOL is_true_obj(LispObject);
extern Character make_char(char);
extern Fixnum make_fixnum(int);
extern Function make_Lisp_function(Cons, LispObject, Environment, Environment, BlockEnvironment, Environment);
extern Function make_C_function(primitive_t, int);
extern String make_string(char *);

extern Symbol lt_nil;
extern Symbol lt_t;

extern Symbol lt_block;
extern Symbol lt_catch;
extern Symbol lt_defvar;
extern Symbol lt_fset;
extern Symbol lt_function;
extern Symbol lt_if;
extern Symbol lt_lambda;
extern Symbol lt_progn;
extern Symbol lt_quote;
extern Symbol lt_return_from;
extern Symbol lt_set;
extern Symbol lt_throw;

#endif
