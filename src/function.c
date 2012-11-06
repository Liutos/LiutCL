/*
 * function.c
 *
 *
 *
 * Copyright (C) 2012-11-06 liutos <mat.liutos@gmail.com>
 */
#include <stdlib.h>

#include "cons.h"
#include "environment.h"
#include "eval_sexp.h"
#include "stream.h"
#include "symbol_table.h"
#include "types.h"

#define CALL_EVAL(eval_fn, arg) eval_fn(arg, lenv, denv, benv, fenv)
#define eq(x, y) (x == y)

Function make_function_aux(void)
{
    return TO_FUNCTION(malloc(sizeof(struct function_t)));
}

Function make_C_function(primitive_t prim, Arity arity, FunctionType type)
{
    Function fn = make_function_aux();

    FUNCTION_CFLAG(fn) = TRUE;
    FTYPE(fn) = type;
    PRIMITIVE(fn) = prim;
    ARITY(fn) = arity;

    return fn;
}

arity_t make_arity_t(int req_cnt, int opt_cnt, BOOL rest_flag, BOOL key_flag, int key_cnt, List keywords)
{
    arity_t arity;

    arity = malloc(sizeof(struct arity_t));
    arity->req_count = req_cnt;
    arity->opt_count = opt_cnt;
    arity->rest_flag = rest_flag;
    arity->key_flag = key_flag;
    arity->key_count = key_cnt;
    arity->keywords = keywords;

    return arity;
}

Arity make_arity(List parms)
{
    BOOL rest_flag;
    Symbol key, opt, rest;
    int key_count, opt_count, req_count;

    key = ensure_symbol_exists("&KEY");
    opt = ensure_symbol_exists("&OPTIONAL");
    rest = ensure_symbol_exists("&REST");
    key_count = opt_count = req_count = 0;
    /* Required parameters */
    while (CONS_P(parms)) {
        if (eq(key, CAR(parms)) || eq(opt, CAR(parms)) || eq(rest, CAR(parms)))
            break;
        req_count++;
        parms = CDR(parms);
    }
    /* Optional parameters */
    if (eq(opt, CAR(parms))) {
        parms = CDR(parms);
        while (CONS_P(parms)) {
            if (eq(key, CAR(parms)) || eq(rest, CAR(parms)))
                break;
            opt_count++;
            parms = CDR(parms);
        }
    }
    /* Keyword parameters */
    if (eq(key, CAR(parms))) {
        parms = CDR(parms);
        while (CONS_P(parms)) {
            if (eq(opt, CAR(parms))) {
                write_format(standard_error, "Lambda list marker &OPTIONAL not allowed here.\n");
                exit(1);
            }
            if (eq(rest, CAR(parms)))
                break;
            key_count++;
            parms = CDR(parms);
        }
    }
    /* Rest parameters */
    if (eq(rest, CAR(parms))) {
        parms = CDR(parms);
        if (CONS_P(parms))
            rest_flag = TRUE;
        parms = CDR(parms);
        if (eq(key, CAR(parms)) || eq(opt, CAR(parms))) {
            write_format(standard_error, "Lambda list marker $! not allowed here.\n");
            exit(1);
        } else if (parms != lt_nil) {
            write_format(standard_error, "Lambda list element $! is superfluous. Only one var...\n", CAR(parms));
            exit(1);
        }
    }

    return make_arity_t(req_count, opt_count, rest_flag, key_count != 0, key_count, lt_nil);
}

Function make_Lisp_function(List parms, LispObject expr, Environment lenv, Environment denv, BlockEnvironment benv, Environment fenv)
/* The argument `expr' must be a proper list because it will be evaluated by
   eval_progn in function invoke_Lisp_function. */
{
    Function fn = make_function_aux();

    /* ARITY(fn) = cons_length(parms); */
    ARITY(fn) = make_arity(parms);
    BLOCK_ENV(fn) = benv;
    FDEFINITION_ENV(fn) = fenv;
    FUNCTION_CFLAG(fn) = FALSE;
    FTYPE(fn) = REGULAR;
    EXPRESSION(fn) = expr;
    LEXICAL_ENV(fn) = lenv;
    PARAMETERS(fn) = parms;

    return fn;
}

#ifndef FS
LispObject invoke_C_function(Function C_fn, Cons args)
{
    return PRIMITIVE(C_fn)(args);
}
#else
LispObject invoke_C_function(Function C_fn, Cons args, Environment lenv, Environment denv, BlockEnvironment benv, Environment fenv)
{
    return PRIMITIVE(C_fn)(args, lenv, denv, benv, fenv);
}
#endif

LispObject invoke_Lisp_function(Function Lisp_function, Cons args, Environment denv)
/* Invokes a Lisp function means evaluating the function's body in a extended
   environment. */
{
    BlockEnvironment benv = BLOCK_ENV(Lisp_function);
    Environment fenv = FDEFINITION_ENV(Lisp_function);
    Environment lenv =
        make_new_env(PARAMETERS(Lisp_function),
                     args,
                     LEXICAL_ENV(Lisp_function));
    
    return CALL_EVAL(eval_progn, EXPRESSION(Lisp_function));
}

#ifndef FS
LispObject invoke_function(Function function, Cons args, Environment denv)
{
    if (TRUE == FUNCTION_CFLAG(function))
	return invoke_C_function(function, args);
    else
	return invoke_Lisp_function(function, args, denv);
}
#else
LispObject invoke_function(Function function, Cons args, Environment lenv, Environment denv, BlockEnvironment benv, Environment fenv)
{
    if (TRUE == FUNCTION_CFLAG(function))
	return invoke_C_function(function, args, lenv, denv, benv, fenv);
    else
	return invoke_Lisp_function(function, args, denv);
}
#endif
