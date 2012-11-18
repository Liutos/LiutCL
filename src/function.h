#ifndef FUNCTION_H
#define FUNCTION_H

#include "macro_def.h"
#include "types.h"

extern Arity make_arity(int, int, BOOL, BOOL, int, List);
extern Arity make_arity_kw(int, int, BOOL, ...);
extern Arity new_with_kws(Arity, ...);
extern DEFINVOKE(invoke_function, _, Cons);
extern DEFMK(make_Lisp_function);
extern DEFMK(make_Lisp_macro);
extern Environment reg_primitive(char *, Package, primitive_t, Arity, FunctionType, Environment);
extern Frame cons2frame(Cons, Arity);
extern Function make_C_function(primitive_t, Arity, FunctionType);
extern List frame2cons(Frame);
extern List make_keywords(int, ...);

#endif
