#ifndef FUNCTION_H
#define FUNCTION_H

#include "macro_def.h"
#include "types.h"

extern Arity make_arity_t(int, int, BOOL, BOOL, int, List);
extern Function make_C_function(primitive_t, Arity, FunctionType);
extern O2E4(make_Lisp_function);
extern O2E4(make_Lisp_macro);
extern O2E4(invoke_function);

#endif
