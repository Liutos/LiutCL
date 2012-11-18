/*
 * eval_sexp.c
 *
 * Evaluator of S-expression.
 *
 * Copyright (C) 2012-10-04 liutos
 */
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "atom.h"
#include "cons.h"
#include "environment.h"
#include "function.h"
#include "macro_def.h"
#include "object.h"
#include "print_sexp.h"
#include "stream.h"
#include "symbol.h"
#include "types.h"

#define VALUES(x) process_values(x, is_need_mv)

DEFEVAL(eval_cons, _);
DEFEVAL(eval_progn, _);
DEFEVAL(eval_sexp, _);

LispObject return_value;

jmp_buf escape;
/* Variable `toplevel' should not be changed. */
jmp_buf toplevel;

DEFEVAL(eprogn, exps)
{
    while (CONS_P(exps)) {
	if (!CONS_P(CDR(exps)))
	    break;
	CALL_EVAL(eval_sexp, CAR(exps));
	exps = CDR(exps);
    }

    return MVCALL_EVAL(eval_sexp, CAR(exps));
}

void check_arity_pattern(Arity arity, List args)
{
    BOOL key_flag, rest_flag;
    int key_count, opt_count, req_count;

    key_flag = arity->key_flag;
    rest_flag = arity->rest_flag;
    key_count = arity->key_count;
    req_count = arity->req_count;
    opt_count = arity->opt_count;
    /* Checks the required arguments. */
    for (int i = 0; i < req_count; ++i) {
        if (!CONS_P(args)) {
            write_format(standard_error, "Two few arguments\n");
            longjmp(toplevel, TOO_FEW_ARGUMENTS);
        }
        args = CDR(args);
    }
    if (CONS_P(args) &&
        0 == opt_count && FALSE == key_flag && FALSE == rest_flag) {
        write_format(standard_error, "Two many arguments\n");
        longjmp(toplevel, TOO_MANY_ARGUMENTS);
    }
    /* Checks the optional arguments. */
    for (int i = 0; i < opt_count; ++i) {
        if (!CONS_P(args))
            break;
        args = CDR(args);
    }
    if (CONS_P(args) && FALSE == key_flag && FALSE == rest_flag) {
        write_format(standard_error, "Two many arguments\n");
        longjmp(toplevel, TOO_MANY_ARGUMENTS);
    }
    if (TRUE == rest_flag && FALSE == key_flag)
        return;
    /* Checks the keyword arguments. */
    for (int i = 0; i < key_count; ++i) {
        if (CONS_P(args) && !CONS_P(CDR(args))) {
            error_format("keyword arguments in ($!) should occur pairwise.\n",
                         CAR(args));
            longjmp(toplevel, 1);
        }
        if (CONS_P(args) && CONS_P(CDR(args)))
            args = CDDR(args);
    }
}

DEFEVAL(eval_args, args)
{
    Cons cur, head, pre;

    pre = head = make_cons(lt_nil, lt_nil);
    while (CONS_P(args)) {
        LispObject val;

        val = CALL_EVAL(eval_sexp, CAR(args));
        cur = make_cons(val, lt_nil);
        _CDR(pre) = cur;
        pre = cur;
        args = CDR(args);
    }

    return CDR(head);
}

LispObject process_values(LispObject value, BOOL is_need_mv)
{
    if (is_need_mv) {
        if (VALUES_P(value))
            return value;
        else {
            values_t vals;

            vals = malloc(sizeof(struct values_t));
            vals->count = 1;
            vals->objs = malloc(sizeof(LispObject));
            vals->objs[0] = value;

            return TO_VALUES(vals);
        }
    } else {
        if (VALUES_P(value)) {
            /* Returns NIL when no values */
            if (0 == theVALUES(value)->count)
                return lt_nil;
            else
                return PRIMARY_VALUE(value);
        } else
            return value;
    }
}

DEFEVAL(eval_cons, exps)
{
    Cons args;
    LispObject op, value;

    args = CDR(exps);
    op = FIRST(exps);
    if (CONS_P(op))
        op = CALL_EVAL(eval_cons, op);
    else if (SYMBOL_P(op))
        op = get_value(op, fenv);
    if (!FUNCTION_P(op)) {
        error_format("%! isn't a funcallable object.\n", op);
        longjmp(toplevel, TYPE_ERROR);
    }
    /* Evaluates the arguments and check. */
    if (REGULAR == FTYPE(op))
        args = CALL_EVAL(eval_args, args);
    check_arity_pattern(ARITY(op), args);
    value = CALL_INVOKE(invoke_function, op, args);

    return VALUES(value);
}

LispObject eval_symbol(Symbol sym, Environment lenv, Environment denv)
{
    LispObject value;

    if (is_keyword(sym))
        return sym;
    /* Search in the global constant environment */
    if ((value = get_value(sym, global_constant_env)) != NULL)
        return value;
    /* Search in the current lexical scope environment */
    else if ((value = get_value(sym, lenv)) != NULL)
        return value;
    /* Search in the current dynamic scope environment */
    else if ((value = get_value(sym, denv)) != NULL)
        return value;
    else {
        error_format("No binding of symbol %!\n", sym);
        longjmp(toplevel, UNBOUND_VARIABLE);
    }
}

DEFEVAL(eval_sexp, exp)
{
    if (NULL == exp)
        return NULL;
    if (CONS_P(exp))
        return MMCALL_EVAL(eval_cons, exp);
    else if (SYMBOL_P(exp))
        return eval_symbol(exp, lenv, denv);
    else                        /* Self-evaluating objects. */
        return exp;
}
