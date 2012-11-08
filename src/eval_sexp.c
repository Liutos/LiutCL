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
#include "edecls.h"
#include "env_types.h"
#include "environment.h"
#include "function.h"
#include "object.h"
#include "print_sexp.h"
#include "stream.h"
#include "types.h"

DEFEVAL(eval_cons, _);
DEFEVAL(eval_progn, _);
DEFEVAL(eval_sexp, _);

jmp_buf escape;

DEFEVAL(eprogn, exps)
{
    if (!CONS_P(exps))
	return lt_nil;
    if (!CONS_P(CDR(exps)))
	return CALL_EVAL(eval_sexp, CAR(exps));
    while (CONS_P(exps)) {
	if (!CONS_P(CDR(exps)))
	    break;
	CALL_EVAL(eval_sexp, CAR(exps));
	exps = CDR(exps);
    }

    return CALL_EVAL(eval_sexp, CAR(exps));
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
    for (int i = 0; i < req_count; ++i) {
        if (!CONS_P(args)) {
            write_format(standard_error, "Two few arguments\n");
            exit(1);
        }
        args = CDR(args);
    }
    if (CONS_P(args) &&
        0 == opt_count && FALSE == key_flag && FALSE == rest_flag) {
        write_format(standard_error, "Two many arguments\n");
        exit(1);
    }
    for (int i = 0; i < opt_count; ++i) {
        if (!CONS_P(args))
            break;
        args = CDR(args);
    }
    if (CONS_P(args) && FALSE == key_flag && FALSE == rest_flag) {
        write_format(standard_error, "Two many arguments\n");
        exit(1);
    }
    if (TRUE == rest_flag && FALSE == key_flag)
        return;
    for (int i = 0; i < key_count; ++i) {
        if (CONS_P(args) && !CONS_P(CDR(args))) {
            write_format(standard_error, "keyword arguments in ($!) should occur pairwise.\n", CAR(args));
            exit(1);
        }
        if (CONS_P(args) && CONS_P(CDR(args)))
            args = CDDR(args);
    }
}

DEFEVAL(eval_operator, op)
{
    if (SYMBOL_P(op)) {
	LispObject tmp;

        tmp = get_value(op, fenv);
	if (tmp != NULL)
	    return tmp;
	else {
            write_format(standard_error, "No binding of symbol %!\n", op);
	    exit(1);
	}
    } else
	return CALL_EVAL(eval_cons, op);
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

DEFEVAL(eval_cons, exps)
{
    Cons args;
    LispObject op;

    args = CDR(exps);
    op = FIRST(exps);
    switch (type_of(op)) {
    case CONS:
        op = CALL_EVAL(eval_cons, op);
        if (!FUNCTION_P(op)) {
            write_format(standard_error, "%! isn't a functional object.\n", op);
            exit(1);
        } else
            goto LABEL;
    case SYMBOL:
        op = CALL_EVAL(eval_operator, op);
        if (!FUNCTION_P(op)) {
            write_format(standard_error, "%! isn't a functional object.\n", op);
            exit(1);
        }
    LABEL:
    case FUNCTION: {
        if (REGULAR == FTYPE(op))
            args = CALL_EVAL(eval_args, args);
        check_arity_pattern(ARITY(op), args);

        return invoke_function(op, args, lenv, denv, benv, fenv);
    }
    default :
        write_format(standard_error, "%! isn't a functional object.\n", op);
        exit(1);
    }
}

LispObject eval_atom(Atom exp, Environment lenv, Environment denv)
{
    if (SYMBOL_P(exp)) {
	LispObject tmp;

        /* Search in the global constant environment */
        tmp = get_value(exp, global_constant_env);
	if (tmp != NULL)
	    return tmp;
        /* Search in the current lexical scope environment */
        else if ((tmp = get_value(exp, lenv)) != NULL)
            return tmp;
        /* Search in the current dynamic scope environment */
        else if ((tmp = get_value(exp, denv)) != NULL)
            return tmp;
	else {
            write_format(standard_error, "No binding of symbol %!\n", exp);
	    exit(0);
	}
    } else
	return exp;
}

DEFEVAL(eval_sexp, exp)
{
    if (NULL == exp)
        return NULL;
    if (CONS_P(exp))
        return CALL_EVAL(eval_cons, exp);
    else
        return eval_atom(exp, lenv, denv);
}
