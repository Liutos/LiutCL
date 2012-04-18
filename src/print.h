#ifndef PRINT_H
#define PRINT_H

#include "types.h"

extern void print_object(struct LispObject *);
extern void print_env(ENVIRONMENT *, BOOLEAN);

#endif
