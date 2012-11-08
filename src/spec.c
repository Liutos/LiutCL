/*
 * spec.c
 *
 *
 *
 * Copyright (C) 2012-11-05 liutos <mat.liutos@gmail.com>
 */
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "atom.h"
#include "cons.h"
#include "environment.h"
#include "eval_sexp.h"
#include "function.h"
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
    if (0 == val)
        return eprogn(CDR(args), lenv, denv,
                      make_block_env(name, block_context, benv), fenv);
    else
        return (LispObject)val;
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

        value = CALL_EVAL(eprogn, CDR(args));
        memcpy(escape, catch_context, sizeof(jmp_buf));

        return value;
    } else {                    /* Returns from a invokation of THROW */
        Cons cons;

        cons = (Cons)val;
        memcpy(escape, catch_context, sizeof(jmp_buf));
        if (eq(CAR(cons), tag))
            return CDR(cons);
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

        value = CALL_EVAL(eval_sexp, SECOND(args));
        SYMBOL_VALUE(name) = value;
        global_dynamic_env = extend_env(name, value, global_dynamic_env);
    }

    return name;
}

PHEAD(lt_fset)
{
    LispObject name;

    name = CALL_EVAL(eval_sexp, ARG1);
    extend_env(name, CALL_EVAL(eval_sexp, SECOND(args)), fenv);

    return lt_nil;
}

PHEAD(lt_function)
{
    return get_value(ARG1, fenv);
}

PHEAD(lt_if)
{
    if (lt_nil != CALL_EVAL(eval_sexp, ARG1))
        return CALL_EVAL(eval_sexp, ARG2);
    else
        return CALL_EVAL(eval_sexp, ARG3);
}

PHEAD(lt_lambda)
{
    return make_Lisp_function(CAR(args), CDR(args), lenv, denv, benv, fenv);
}

PHEAD(lt_progn)
{
    if (!CONS_P(args))
	return lt_nil;
    if (!CONS_P(CDR(args)))
	return CALL_EVAL(eval_sexp, CAR(args));
    while (CONS_P(args)) {
	if (!CONS_P(CDR(args)))
	    break;
	CALL_EVAL(eval_sexp, CAR(args));
	args = CDR(args);
    }

    return CALL_EVAL(eval_sexp, CAR(args));
}

PHEAD(lt_quote)
{
    RETURN(ARG1);
}

PHEAD(lt_setq)
{
    LispObject form, pairs, value, var;

    pairs = args;
    while (!TAIL_P(pairs)) {
        var = CAR(pairs);
        pairs = CDR(pairs);
        if (TAIL_P(pairs)) {
            write_format(standard_error, "Odd number of arguments %!", ARG1);
            exit(1);
        }
        form = CAR(pairs);
        value = CALL_EVAL(eval_sexp, form);
        extend_env(var, value, lenv);
        pairs = CDR(pairs);
    }
    RETURN(value);
}

PHEAD(lt_return_from)
{
    LispObject result;
    Symbol name;

    result = CALL_EVAL(eval_sexp, SECOND(args));
    name = FIRST(args);
    while (benv != NULL) {
        if (benv->name == name)
            longjmp(benv->context, (int)result);
        benv = benv->prev;
    }
    write_format(standard_error, "No such a block contains name %!\n", name);
    exit(1);
}

PHEAD(lt_throw)
{
    LispObject result;
    LispObject tag;

    result = CALL_EVAL(eval_sexp, SECOND(args));
    tag = CALL_EVAL(eval_sexp, FIRST(args));
    longjmp(escape, (int)make_cons(tag, result));
}
