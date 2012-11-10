/*
 * macro_def.h
 *
 * Definition of macros about define and call.
 *
 * Copyright (C) 2012-11-09 liutos <mat.liutos@gmail.com>
 */
#ifndef MACRO_DEF_H
#define MACRO_DEF_H

#define CALL_EVAL(eval_fn, arg)                 \
    eval_fn(arg, lenv, denv, fenv, benv, genv)
#define CALL_INVOKE(invoker, fn, args)                  \
    invoker(fn, args, lenv, denv, fenv, benv, genv)
#define CALL_MK(maker, arg1, arg2)                      \
    maker(arg1, arg2, lenv, denv, fenv, benv, genv)
#define DEFEVAL(name, hd)                       \
    LispObject name(LispObject hd,              \
                    Environment lenv,           \
                    Environment denv,           \
                    Environment fenv,           \
                    BlockEnvironment benv,      \
                    GoEnvironment genv)
#define DEFINVOKE(fn_name, fn)                  \
    LispObject fn_name(Function fn,             \
                       Frame args,              \
                       Environment lenv,        \
                       Environment denv,        \
                       Environment fenv,        \
                       BlockEnvironment benv,   \
                       GoEnvironment genv)
#define DEFMK(mk_name)                          \
    LispObject mk_name(List parms,              \
                       List expr,               \
                       Environment lenv,        \
                       Environment denv,        \
                       Environment fenv,        \
                       BlockEnvironment benv,   \
                       GoEnvironment genv)

#endif
