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

#include "cons.h"
#include "environment.h"
#include "eval_sexp.h"
#include "function.h"
#include "macro_def.h"
#include "package.h"
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
    longjmp(toplevel, MISSING_GO_TAG);
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

Cons values2cons(Values vals)
{
    Cons cur, head, pre;
    values_t v;

    pre = head = make_cons(lt_nil, lt_nil);
    v = theVALUES(vals);
    for (int i = 0; i < v->count; i++) {
        cur = make_cons(v->objs[i], lt_nil);
        /* _CDR(pre) = cur; */
        set_cdr(pre, cur);
        pre = cur;
    }

    return CDR(head);
}

PHEAD(lt_multiple_value_call)
{
    Function function;
    List forms;
    Cons cur, head, pre;

    function = CALL_EVAL(eval_sexp, ARG1);
    forms = RK;
    pre = head = make_cons(lt_nil, lt_nil);
    while (forms != lt_nil) {
        LispObject value;

        value = MVCALL_EVAL(eval_sexp, CAR(forms));
        if (NO_VALUES_P(value))
            continue;
        if (SINGLE_VALUES_P(value)) {
            cur = make_cons(PRIMARY_VALUE(value), lt_nil);
            /* _CDR(pre) = cur; */
            set_cdr(pre, cur);
            pre = cur;
        } else {
            cur = values2cons(value);
            /* _CDR(pre) = cur; */
            set_cdr(pre, cur);
            while (CDR(pre) != lt_nil)
                pre = CDR(pre);
        }
    }
    RETURN(CALL_INVOKE(invoke_function, function, CDR(head)));
}

PHEAD(lt_multiple_value_list)
{
    LispObject form;
    Values vals;

    form = ARG1;
    vals = MVCALL_EVAL(eval_sexp, form);
    RETURN(values2cons(vals));
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

PHEAD(lt_progv)
{
    List symbols, values;
    List form;

    symbols = CALL_EVAL(eval_sexp, ARG1);
    values = CALL_EVAL(eval_sexp, ARG2);
    form = RK;
    denv = make_new_env(symbols, values, denv);
    RETURN(CALL_EVAL(eprogn, form));
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
    longjmp(toplevel, MISSING_BLOCK_NAME);
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
            longjmp(toplevel, 1);
        }
        form = CAR(pairs);
        value = CALL_EVAL(eval_sexp, form);
        update_env(var, value, lenv);
        SYMBOL_VALUE(var) = value;
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
            /* _CDR(pre) = cur; */
            set_cdr(pre, cur);
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

void init_spec(Environment env)
{
    csreg("BLOCK", lt_block, req1rest);
    csreg("CATCH", lt_catch, req1rest);
    sreg("FSET", pkg_lt, lt_fset, req2);
    csreg("FUNCTION", lt_function, req1);
    csreg("GO", lt_go, req1);
    csreg("IF", lt_if, req2opt1);
    reg_inits(lt_if, "(nil)", NULL);
    csreg("LAMBDA", lt_lambda, req1rest);
    csreg("MK-MACRO", lt_mk_macro, req1rest);
    csreg("MULTIPLE-VALUE-CALL", lt_multiple_value_call, req1rest);
    csreg("MULTIPLE-VALUE-LIST", lt_multiple_value_list, req1);
    csreg("PROGN", lt_progn, rest);
    csreg("PROGV", lt_progv, req2rest);
    csreg("QUOTE", lt_quote, req1);
    csreg("RETURN-FROM", lt_return_from, req2);
    csreg("SETQ", lt_setq, rest);
    csreg("TAGBODY", lt_tagbody, rest);
    csreg("THROW", lt_throw, req2);
}
