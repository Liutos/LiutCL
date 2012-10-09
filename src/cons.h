#ifndef CONS_H
#define CONS_H

#include "types.h"

extern Cons make_cons_cell(LispObject, LispObject);
extern LispObject safe_cdr(LispObject);
extern LispObject safe_car(LispObject);

#endif
