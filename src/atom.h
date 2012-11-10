#ifndef ATOM_H
#define ATOM_H

#include "types.h"

extern BOOL is_atom_object(LispObject);
extern BOOL is_symbol(LispObject);
extern BOOL is_tail(LispObject);
extern BOOL is_true_obj(LispObject);
extern BOOL numeric_eq(Number, Number);
extern Character make_char(char);
extern Fixnum make_fixnum(int);
extern Float make_float(double);
extern String make_string(char *);
extern String str_add_char(String, Character);

#endif
