#ifndef FUNCTION_H
#define FUNCTION_H

#include "macro_def.h"
#include "types.h"

extern arity_t make_arity(int, int, BOOL, BOOL, int, List);
extern arity_t make_arity_kw(int, int, BOOL, ...);
extern arity_t new_with_kws(arity_t, ...);
extern DEFINVOKE(invoke_function, _, Cons);
extern DEFMK(make_Lisp_function);
extern DEFMK(make_Lisp_macro);
extern Environment reg_primitive(char *, Package, primitive_t, arity_t, FunctionType, Environment);
extern Frame cons2frame(Cons, arity_t);
extern Function make_C_function(primitive_t, arity_t, FunctionType);
extern List frame2cons(Frame);
extern List make_keywords(int, ...);
extern void describe_frame(Frame);
extern void reg_inits(primitive_t, char *, char *);

extern hash_table_t init_exprs;

#endif
