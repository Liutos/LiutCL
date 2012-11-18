#ifndef OBJECT_H
#define OBJECT_H

#include <stdarg.h>
#include <stdio.h>

#include "types.h"

extern LispObject make_object(void);
extern LispType type_of(LispObject);
extern Values make_values(int cnt, ...);

#endif
