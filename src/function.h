#ifndef FUNCTION_H
#define FUNCTION_H

#include "types.h"

extern Arity make_arity_t(int, int, BOOL, BOOL, int, List);
extern Function make_C_function(primitive_t, Arity, FunctionType);
extern Function make_Lisp_function(List parms, LispObject expr, Environment lenv, Environment denv, BlockEnvironment benv, Environment fenv);
extern LispObject invoke_function(Function function, Cons args, Environment lenv, Environment denv, BlockEnvironment benv, Environment fenv);

#endif
