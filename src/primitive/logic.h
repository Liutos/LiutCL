#ifndef LOGIC_H
#define LOGIC_H

#include "../types.h"

#define PHEAD(fn_name) LispObject fn_name(Cons)

extern PHEAD(and_two);
extern PHEAD(or_two);

#endif
