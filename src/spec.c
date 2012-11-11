/*
 * spec.c
 *
 * Function implementation of special operators in Common Lisp
 *
 * Copyright (C) 2012-11-05 liutos <mat.liutos@gmail.com>
 */
#include <assert.h>
#include <setjmp.h>
#include <stdlib.h>
#include <string.h>

#include "atom.h"
#include "cons.h"
#include "environment.h"
#include "eval_sexp.h"
#include "function.h"
#include "macro_def.h"
#include "pdecls.h"
#include "stream.h"
#include "types.h"

PHEAD(lt_block)
{
    Symbol name;
    jmp_buf block_context;
    int val;

    name = ARG1;
    val = setjmp(block_context);
    if (0 == val) {
        benv = make_block_env(name, block_context, benv);
        RETURN(CALL_EVAL(eprogn, RK));
    } else
        RETURN((LispObject)val);
}

PHEAD(lt_catch)
{
    LispObject tag;
    int val;
    jmp_buf catch_context;

    tag = CALL_EVAL(eval_sexp, ARG1);
    /* Simulates the dynamic scope by twice application of memcpy */
    memcpy(catch_context, escape, sizeof(jmp_buf));
    val = setjmp(escape);
    if (0 == val) {
        LispObject value;

        value = CALL_EVAL(eprogn, RK);
        memcpy(escape, catch_context, sizeof(jmp_buf));
        RETURN(value);
    } else {                    /* Returns from a invokation of THROW */
        Cons cons;

        cons = (Cons)val;
        memcpy(escape, catch_context, sizeof(jmp_buf));
        if (eq(CAR(cons), tag))
            RETURN(CDR(cons));
        else
            longjmp(escape, val);
    }
}

PHEAD(lt_defvar)
{
    Symbol name;

    name = ARG1;
    assert(SYMBOL_P(name));
    if (NULL == get_value(name, global_dynamic_env)) {
        LispObject value;

        value = CALL_EVAL(eval_sexp, ARG2);
        SYMBOL_VALUE(name) = value;
        update_env(name, value, global_dynamic_env);
    }
    RETURN(name);
}

PHEAD(lt_fset)
{
    LispObject form, name, value;

    form = ARG2;
    name = CALL_EVAL(eval_sexp, ARG1);
    value = CALL_EVAL(eval_sexp, form);
    extend_env(name, value, fenv);
    SYMBOL_FUNCTION(name) = value;
    RETURN(lt_nil);
}

PHEAD(lt_function)
{
    RETURN(get_value(ARG1, fenv));
}

PHEAD(lt_go)
{
    LispObject tag;

    tag = ARG1;
    while (genv != NULL) {
        if (is_go_able(tag, genv))
            longjmp(genv->context, (int)tag);
        genv = genv->prev;
    }
    error_format("GO: no tag named %! is currently visible", tag);
    /* exit(1); */
    longjmp(toplevel, 1);
}

PHEAD(lt_if)
{
    if (lt_nil != CALL_EVAL(eval_sexp, ARG1))
        RETURN(CALL_EVAL(eval_sexp, ARG2));
    else
        RETURN(CALL_EVAL(eval_sexp, ARG3));
}

PHEAD(lt_lambda)
{
    RETURN(CALL_MK(make_Lisp_function, ARG1, RK));
}

PHEAD(lt_mk_macro)
{
    RETURN(CALL_MK(make_Lisp_macro, ARG1, RK));
}

PHEAD(lt_progn)
{
    List form;

    form = RK;
    if (!CONS_P(form))
	RETURN(lt_nil);
    if (!CONS_P(CDR(form)))
	RETURN(CALL_EVAL(eval_sexp, CAR(form)));
    while (CONS_P(form)) {
	if (!CONS_P(CDR(form)))
	    break;
	CALL_EVAL(eval_sexp, CAR(form));
	form = CDR(form);
    }
    RETURN(CALL_EVAL(eval_sexp, CAR(form)));
}

PHEAD(lt_quote)
{
    RETURN(ARG1);
}

PHEAD(lt_return_from)
{
    LispObject result;
    Symbol name;

    result = CALL_EVAL(eval_sexp, ARG2);
    name = ARG1;
    while (benv != NULL) {
        if (benv->name == name)
            longjmp(benv->context, (int)result);
        benv = benv->prev;
    }
    error_format("RETURN-FROM: No such a block contains name %!.\n", name);
    /* exit(1); */
    longjmp(toplevel, 1);
}

PHEAD(lt_setq)
{
    LispObject form, pairs, value, var;

    pairs = RK;
    while (!TAIL_P(pairs)) {
        var = CAR(pairs);
        pairs = CDR(pairs);
        if (TAIL_P(pairs)) {
            error_format("SETQ: Odd number of arguments %!.\n", ARG1);
            /* exit(1); */
            longjmp(toplevel, 1);
        }
        form = CAR(pairs);
        value = CALL_EVAL(eval_sexp, form);
        /* extend_env(var, value, lenv); */
        update_env(var, value, lenv);
        pairs = CDR(pairs);
    }
    RETURN(value);
}

PHEAD(lt_tagbody)
{
    Cons expr;
    List cur, pre, tags;
    int val;
    jmp_buf context;

    expr = RK;
    pre = tags = make_cons(lt_nil, lt_nil);
    while (CONS_P(expr)) {
        if (ATOM_P(CAR(expr))) {
            cur = make_cons(CAR(expr), lt_nil);
            _CDR(pre) = cur;
            pre = cur;
        }
        expr = CDR(expr);
    }
    pre = tags;
    tags = CDR(tags);
    free_cons(pre);
    val = setjmp(context);
    genv = make_go_env(tags, context, genv);
    if (0 == val) {
        expr = RK;
        while (CONS_P(expr)) {
            if (!ATOM_P(CAR(expr)))
                CALL_EVAL(eval_sexp, CAR(expr));
            expr = CAR(expr);
        }
    } else {                    /* Come from a GO invokation */
        LispObject tag;

        expr = RK;
        tag = (LispObject)val;
        while (CONS_P(expr))
            if (!eq(tag, CAR(expr)))
                expr = CDR(expr);
            else
                break;
        while (CONS_P(expr)) {
            if (!ATOM_P(CAR(expr)))
                CALL_EVAL(eval_sexp, CAR(expr));
            expr = CDR(expr);
        }
    }
    RETURN(lt_nil);
}

PHEAD(lt_throw)
{
    LispObject result;
    LispObject tag;

    result = CALL_EVAL(eval_sexp, ARG2);
    tag = CALL_EVAL(eval_sexp, ARG1);
    longjmp(escape, (int)make_cons(tag, result));
}
