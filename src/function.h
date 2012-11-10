#ifndef FUNCTION_H
#define FUNCTION_H

#include "macro_def.h"
#include "types.h"

extern Arity make_arity_kw(int, int, BOOL, ...);
extern Arity make_arity_t(int, int, BOOL, BOOL, int, List);
extern Frame cons2frame(Cons, Arity);
extern Function make_C_function(primitive_t, Arity, FunctionType);
extern List make_keywords(int, ...);
extern DEFMK(make_Lisp_function);
extern DEFMK(make_Lisp_macro);
extern DEFINVOKE(invoke_function, _);

#endif
