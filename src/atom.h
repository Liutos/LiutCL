#ifndef ATOM_H
#define ATOM_H

#include "env_types.h"
#include "types.h"

extern BOOL is_atom_object(LispObject);
extern BOOL is_symbol(LispObject);
extern BOOL is_tail(LispObject);
extern BOOL is_true_obj(LispObject);
extern Character make_char(char);
extern Fixnum make_fixnum(int);
extern Float make_float(double);
extern String make_string(char *);

#endif
