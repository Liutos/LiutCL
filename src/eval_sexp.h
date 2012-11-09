#ifndef EVAL_SEXP_H
#define EVAL_SEXP_H

#include <setjmp.h>

#include "env_types.h"
#include "macro_def.h"
#include "types.h"

extern DEFEVAL(eprogn, _);
extern DEFEVAL(eval_operator, _);
extern DEFEVAL(eval_sexp, _);

extern jmp_buf escape;

#endif
