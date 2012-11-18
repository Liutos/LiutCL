#ifndef ATOM_H
#define ATOM_H

#include "types.h"

extern BOOL is_unbound(LispObject);
extern Character make_char(char);
extern String make_string(char *);
extern String str_add_char(String, Character);

#endif
