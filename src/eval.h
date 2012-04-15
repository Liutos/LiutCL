#ifndef EVAL_H
#define EVAL_H

#include "types.h"

struct LispObject *eval_expression(struct LispObject *, ENVIRONMENT *);

#endif
