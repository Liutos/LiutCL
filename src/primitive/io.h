#ifndef IO_H
#define IO_H

#include "../types.h"

#define PHEAD(fn_name) LispObject fn_name(Cons)

extern PHEAD(lt_read_a_char);
extern PHEAD(lt_write_a_char);

#endif
