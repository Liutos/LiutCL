#ifndef ARIT_H
#define ARIT_H

#include "../types.h"

#define PHEAD(fn_name) LispObject fn_name(Cons)

extern PHEAD(add);
extern PHEAD(gt);
extern PHEAD(div);
extern PHEAD(mul);
extern PHEAD(fixnum_eq);
extern PHEAD(sub);

#endif
