#ifndef PRIMITIVES_H
#define PRIMITIVES_H

#include "types.h"

#define PHEAD(fn_name) LispObject fn_name(Cons)

extern PHEAD(lt_car);
extern PHEAD(lt_cdr);
extern PHEAD(lt_cons);
extern PHEAD(lt_eq);
extern PHEAD(lt_type_of);
extern PHEAD(quit);

#endif
