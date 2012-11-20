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

#define DEFINE_WITH_ENV(type, name, ...)        \
    type name                                   \
    (__VA_ARGS__,                               \
     Environment lenv,                          \
     Environment denv,                          \
     Environment fenv,                          \
     BlockEnvironment benv,                     \
     GoEnvironment genv)
#define CALL_WITH_ENV(function, ...)                    \
    function(__VA_ARGS__, lenv, denv, fenv, benv, genv)

#define DEFINE_EVAL(name, arg)                                          \
    DEFINE_WITH_ENV(LispObject, name, LispObject arg, BOOL is_need_mv)
#define CALL_EVAL(eval_fn, arg)                 \
    CALL_WITH_ENV(eval_fn, arg, FALSE)
#define MMCALL_EVAL(eval_fn, arg)               \
    CALL_WITH_ENV(eval_fn, arg, is_need_mv)
#define MVCALL_EVAL(eval_fn, arg)               \
    CALL_WITH_ENV(eval_fn, arg, TRUE)

#define DEFINE_INVOKER(invoker, fn, arg_type)                           \
    DEFINE_WITH_ENV(LispObject, invoker, Function fn, arg_type args)
#define CALL_INVOKER(invoker, fn, args)         \
    CALL_WITH_ENV(invoker, fn, args)

#define DEFINE_MAKER(maker)                                     \
    DEFINE_WITH_ENV(LispObject, maker, List parms, List expr)
#define CALL_MAKER(maker, arg1, arg2)           \
    CALL_WITH_ENV(maker, arg1, arg2)

extern Environment reg_primitive (char *, Package, primitive_t, arity_t, FunctionType, Environment);
#define reg(name, pkg, prim, arity, type)                       \
    env = reg_primitive(name, pkg, prim, arity, type, env)
#define freg(name, pkg, prim, arity)            \
    reg(name, pkg, prim, arity, REGULAR)
#define sreg(name, pkg, prim, arity)            \
    reg(name, pkg, prim, arity, SPECIAL)
#define cfreg(name, prim, arity)                \
    freg(name, pkg_cl, prim, arity)
#define csreg(name, prim, arity)                \
    sreg(name, pkg_cl, prim, arity)

#endif
