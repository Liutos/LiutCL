/*
 * macro_def.h
 *
 * Definition of macros about define and call.
 *
 * Copyright (C) 2012-11-09 liutos <mat.liutos@gmail.com>
 */
#ifndef MACRO_DEF_H
#define MACRO_DEF_H

#include "types.h"

/* Calls the eval_* functions for a single value. */
#define CALL_EVAL(eval_fn, arg)                         \
    eval_fn(arg, lenv, denv, fenv, benv, genv, FALSE)
/* Calls the invoke_* functions. */
#define CALL_INVOKE(invoker, fn, args)                  \
    invoker(fn, args, lenv, denv, fenv, benv, genv)
/* Calls the make_* functions. */
#define CALL_MK(maker, arg1, arg2)                      \
    maker(arg1, arg2, lenv, denv, fenv, benv, genv)
/* Abbreviates the header line of eval_* functions. */
#define DEFEVAL(name, hd)                       \
    LispObject name(LispObject hd,              \
                    Environment lenv,           \
                    Environment denv,           \
                    Environment fenv,           \
                    BlockEnvironment benv,      \
                    GoEnvironment genv,         \
                    BOOL is_need_mv)
/* Abbreviates the header line of invoke_* functions. */
#define DEFINVOKE(fn_name, fn, arg_type)                 \
    LispObject fn_name(Function fn,             \
                       arg_type args,              \
                       Environment lenv,        \
                       Environment denv,        \
                       Environment fenv,        \
                       BlockEnvironment benv,   \
                       GoEnvironment genv)
/* Abbreviates the header line of make_* functions. */
#define DEFMK(mk_name)                          \
    LispObject mk_name(List parms,              \
                       List expr,               \
                       Environment lenv,        \
                       Environment denv,        \
                       Environment fenv,        \
                       BlockEnvironment benv,   \
                       GoEnvironment genv)
/* Passes the `is_need_mv' the same as in current invokation.  */
#define MMCALL_EVAL(eval_fn, arg)                               \
    eval_fn(arg, lenv, denv, fenv, benv, genv, is_need_mv)
/* Passes the `is_need_mv' value TRUE for requesting a multiple-values-return. */
#define MVCALL_EVAL(eval_fn, arg)                       \
    eval_fn(arg, lenv, denv, fenv, benv, genv, TRUE)
/* Registers a regular function `prim' with name `name' to package COMMON-LISP. */
#define cfreg(name, prim, arity) freg(name, pkg_cl, prim, arity)
/* Registers a special operator `prim' with name `name' to package COMMON-LISP. */
#define csreg(name, prim, arity) sreg(name, pkg_cl, prim, arity)
/* Registers a regular function `prim' with name `name' to package `pkg'. */
#define freg(name, pkg, prim, arity) reg(name, pkg, prim, arity, REGULAR)

extern Environment reg_primitive(char *, Package, primitive_t, arity_t, FunctionType, Environment);
#define reg(name, pkg, prim, arity, type)                \
    env = reg_primitive(name, pkg, prim, arity, type, env)
#define sreg(name, pkg, prim, arity) reg(name, pkg, prim, arity, SPECIAL)

#endif
