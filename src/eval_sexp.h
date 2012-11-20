#ifndef EVAL_SEXP_H
#define EVAL_SEXP_H

#include <setjmp.h>

#include "macro_def.h"
#include "types.h"

extern DEFINE_EVAL(eprogn, _);
extern DEFINE_EVAL(eval_sexp, _);

extern jmp_buf escape;

#endif
