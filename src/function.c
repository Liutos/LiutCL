/*
 * function.c
 *
 * Creation and application of Lisp function and its relative stuff.
 *
 * Copyright (C) 2012-11-06 liutos <mat.liutos@gmail.com>
 */
#include <stdarg.h>
#include <stdlib.h>

#include "cons.h"
#include "environment.h"
#include "eval_sexp.h"
#include "macro_def.h"
#include "package.h"
#include "print_sexp.h"
#include "stream.h"
#include "symbol.h"
#include "types.h"

Frame cons2frame(Cons args, Arity arity)
{
    size_t quantity;
    Frame frame;

    quantity = arity->req_count + arity->opt_count;
    frame = malloc(sizeof(struct frame_t));
    frame->quantity = quantity;
    frame->rargs = malloc(quantity * sizeof(LispObject));
    for (int i = 0; i < quantity; i++) {
        frame->rargs[i] = CAR(args);
        args = CDR(args);
    }
    frame->rest_or_kws = args;

    return frame;
}

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

arity_t make_arity_t(req_cnt, opt_cnt, rest_flag, key_flag, key_cnt, keywords)
     int req_cnt;
     int opt_cnt;
     BOOL rest_flag;
     BOOL key_flag;
     int key_cnt;
     List keywords;
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

List make_keywords_aux(va_list ap)
{
    Cons cur, head, pre;
    char *name;

    name = va_arg(ap, char *);
    pre = head = make_cons(lt_nil, lt_nil);
    while (name != NULL) {
        cur = make_cons(gen_pkg_sym(name, pkg_kw), lt_nil);
        _CDR(pre) = cur;
        pre = cur;
        name = va_arg(ap, char *);
    }
    va_end(ap);

    return CDR(head);
}

List make_keywords(int _, ...)
{
    va_list ap;

    va_start(ap, _);

    return make_keywords_aux(ap);
}

Arity make_arity_kw(int req_cnt, int opt_cnt, BOOL rest_flag, ...)
{
    List kws;
    va_list ap;

    va_start(ap, rest_flag);
    kws = make_keywords_aux(ap);

    return make_arity_t(req_cnt, opt_cnt, rest_flag, TRUE, cons_length(kws), kws);
}

Arity make_arity(List parms)
{
    BOOL rest_flag;
    Symbol key, opt, rest;
    int key_count, opt_count, req_count;

    key = gen_pkg_sym("&KEY", pkg_cl);
    opt = gen_pkg_sym("&OPTIONAL", pkg_cl);
    rest = gen_pkg_sym("&REST", pkg_cl);
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

/* LispObject make_Lisp_function(parms, expr, lenv, denv, fenv, benv, genv) */
/*      List parms; */
/*      LispObject expr; */
/*      Environment lenv; */
/*      Environment denv; */
/*      Environment fenv; */
/*      BlockEnvironment benv; */
/*      GoEnvironment genv; */
DEFMK(make_Lisp_function)
{
    Function fn;

    fn = make_function_aux();
    ARITY(fn) = make_arity(parms);
    BLOCK_ENV(fn) = benv;
    FDEFINITION_ENV(fn) = fenv;
    FUNCTION_CFLAG(fn) = FALSE;
    FTYPE(fn) = REGULAR;
    GO_ENV(fn) = genv;
    EXPRESSION(fn) = expr;
    LEXICAL_ENV(fn) = lenv;
    PARAMETERS(fn) = parms;

    return fn;
}

/* Function make_Lisp_macro(parms, expr, lenv, denv, fenv, benv, genv) */
/*      List parms; */
/*      List expr; */
/*      Environment lenv; */
/*      Environment denv; */
/*      Environment fenv; */
/*      BlockEnvironment benv; */
/*      BlockEnvironment genv; */
DEFMK(make_Lisp_macro)
{
    Function fn;

    fn = CALL_MK(make_Lisp_function, parms, expr);
    FTYPE(fn) = MACRO;

    return fn;
}

DEFINVOKE(invoke_C_function, C_fn)
{
    return PRIMITIVE(C_fn)(args, lenv, denv, fenv, benv, genv);
}

List frame2cons(Frame frame)
{
    Cons cur, head, pre;

    pre = head = make_cons(lt_nil, lt_nil);
    for (int i = 0; i < frame->quantity; i++) {
        cur = make_cons(frame->rargs[i], lt_nil);
        _CDR(pre) = cur;
        pre = cur;
    }
    _CDR(pre) = frame->rest_or_kws;

    return CDR(head);
}

LispObject invoke_Lisp_function(Function Lisp_function, frame_t args, Environment denv)
{
    BlockEnvironment benv;
    Environment fenv;
    Environment lenv;
    GoEnvironment genv;

    benv = BLOCK_ENV(Lisp_function);
    fenv = FDEFINITION_ENV(Lisp_function);
    lenv = make_new_env(PARAMETERS(Lisp_function),
                        frame2cons(args),
                        LEXICAL_ENV(Lisp_function));
    genv = GO_ENV(Lisp_function);
    
    return CALL_EVAL(eprogn, EXPRESSION(Lisp_function));
}

DEFINVOKE(invoke_Lisp_macro, macro_fn)
{
    BlockEnvironment _benv;
    Environment _fenv, _lenv;
    GoEnvironment _genv;
    LispObject expanded_expr;

    _benv = benv;
    _genv = genv;
    _fenv = fenv;
    _lenv = lenv;
    benv = BLOCK_ENV(macro_fn);
    fenv = FDEFINITION_ENV(macro_fn);
    lenv = make_new_env(PARAMETERS(macro_fn),
                        frame2cons(args),
                        LEXICAL_ENV(macro_fn));
    expanded_expr = CALL_EVAL(eprogn, EXPRESSION(macro_fn));
    print_object(expanded_expr, standard_output);
    benv = _benv;
    genv = _genv;
    fenv = _fenv;
    lenv = _lenv;
    
    return CALL_EVAL(eval_sexp, expanded_expr);
}

DEFINVOKE(invoke_function, function)
{
    if (TRUE == FUNCTION_CFLAG(function))
	return CALL_INVOKE(invoke_C_function, function, args);
    else if (REGULAR == FTYPE(function))
	return invoke_Lisp_function(function, args, denv);
    else
        return CALL_INVOKE(invoke_Lisp_macro, function, args);
}
