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

#include "atom_proc.h"
#include "cons.h"
#include "env_types.h"
#include "environment.h"
#include "function.h"
#include "object.h"
#include "primitives.h"
#include "print_sexp.h"
#include "stream.h"
#include "types.h"
#include "vm_stack.h"

#define CALL_EVAL(eval_fn, arg) eval_fn(arg, lenv, denv, benv, fenv)
#define DEFEVAL(name, hd)                       \
    LispObject name(LispObject hd,              \
                    Environment lenv,           \
                    Environment denv,           \
                    BlockEnvironment benv,      \
                    Environment fenv)
#define DEFSPEC(name, hd) DEFEVAL(name, hd)
#define eq(x, y) (x == y)

DEFEVAL(eval_cons, _);
DEFEVAL(eval_progn, _);
DEFEVAL(eval_sexp, _);

jmp_buf escape;

BOOL is_special_operator(LispObject op)
{
    Symbol lt_specs[] = {
        lt_block,
        lt_catch,
        lt_defvar,
        lt_function,
        lt_fset,
        lt_progn,
#ifndef FS
        lt_if,
        lt_lambda,
        lt_quote,
#endif
        lt_return_from,
        lt_setq,
        lt_throw,
    };
    int length = sizeof(lt_specs) / sizeof(Symbol);

    for (int i = 0; i < length; ++i)
        if (eq(op, lt_specs[i]))
            return TRUE;

    return FALSE;
}

DEFSPEC(eval_block, argv)
{
    Symbol name;
    jmp_buf context;
    int val;

    name = FIRST(argv);
    val = setjmp(context);
    if (0 == val)
        return eval_progn(CDR(argv), lenv, denv,
                          make_block_env(name, context, benv), fenv);
    else
        return (LispObject)val;
}

DEFSPEC(eval_catch, argv)
{
    LispObject tag;
    int val;
    jmp_buf context;

    tag = CALL_EVAL(eval_sexp, FIRST(argv));
    /* Simulates the dynamic scope by twice application of memcpy */
    memcpy(context, escape, sizeof(jmp_buf));
    val = setjmp(escape);
    if (0 == val) {
        LispObject value;

        value = CALL_EVAL(eval_progn, CDR(argv));
        memcpy(escape, context, sizeof(jmp_buf));

        return value;
    } else {                    /* Returns from a invokation of THROW */
        Cons cons;

        cons = (Cons)val;
        memcpy(escape, context, sizeof(jmp_buf));
        if (eq(CAR(cons), tag))
            return CDR(cons);
        else
            longjmp(escape, val);
    }
}

DEFSPEC(eval_defvar, argv)
{
    Symbol name;

    name = FIRST(argv);
    if (NULL == get_value(name, global_dynamic_env)) {
        LispObject value;

        value = CALL_EVAL(eval_sexp, SECOND(argv));
        SYMBOL_VALUE(name) = value;
        global_dynamic_env = extend_env(name, value, global_dynamic_env);
    }

    return name;
}

DEFSPEC(eval_function, arg)
{
    return get_value(arg, fenv);
}

DEFSPEC(eval_progn, exps)
{
    if (!CONS_P(exps))
	return lt_nil;
    if (!CONS_P(CDR(exps)))
	return CALL_EVAL(eval_sexp, CAR(exps));
    while (!is_tail(exps)) {
	if (is_tail(CDR(exps)))
	    break;
	CALL_EVAL(eval_sexp, CAR(exps));
	exps = CDR(exps);
    }

    return CALL_EVAL(eval_sexp, CAR(exps));
}

DEFSPEC(eval_return_from, argv)
{
    LispObject result;
    Symbol name;

    result = CALL_EVAL(eval_sexp, SECOND(argv));
    name = FIRST(argv);
    while (benv != NULL) {
        if (benv->name == name)
            longjmp(benv->context, (int)result);
        benv = benv->prev;
    }
    write_format(standard_error, "No such a block contains name %!\n", name);
    exit(1);
}

DEFSPEC(eval_throw, argv)
{
    LispObject result;
    LispObject tag;

    result = CALL_EVAL(eval_sexp, SECOND(argv));
    tag = CALL_EVAL(eval_sexp, FIRST(argv));
    longjmp(escape, (int)make_cons(tag, result));
}

#ifndef FS
DEFEVAL(eval_spec, exp)
{
    Cons argv;
    LispObject op;

    argv = CDR(exp);
    op = FIRST(exp);
    if (eq(op, lt_block))
        return CALL_EVAL(eval_block, argv);
    if (eq(op, lt_catch))
        return CALL_EVAL(eval_catch, argv);
    if (eq(op, lt_defvar))
        return CALL_EVAL(eval_defvar, argv);
    if (eq(op, lt_fset)) {
        extend_env(FIRST(argv), CALL_EVAL(eval_sexp, SECOND(argv)), fenv);

        return lt_nil;
    }
    if (eq(op, lt_function))
        return CALL_EVAL(eval_function, FIRST(argv));
    if (eq(op, lt_if)) {
	if (is_true_obj(CALL_EVAL(eval_sexp, FIRST(argv))))
            return CALL_EVAL(eval_sexp, SECOND(argv));
	else
            return CALL_EVAL(eval_sexp, THIRD(argv));
    }
    if (eq(op, lt_lambda))
	return make_Lisp_function(CAR(argv), CDR(argv), lenv, denv, benv, fenv);
    if (eq(op, lt_progn))
        return CALL_EVAL(eval_progn, argv);
    if (eq(op, lt_quote))
        return FIRST(argv);
    if (eq(op, lt_return_from))
        return CALL_EVAL(eval_return_from, argv);
    if (eq(op, lt_setq)) {
        LispObject value;

        value = CALL_EVAL(eval_sexp, SECOND(argv));
        extend_env(FIRST(argv), value, lenv);

        return value;
    }
    if (eq(op, lt_throw))
        return CALL_EVAL(eval_throw, argv);
    write_string(standard_error, TO_STRING("WTF!\n"));
    exit(1);
}
#endif

/* Argument `parms' must be a proper list. */
int check_arity(int arity, List parms)
{
 LABEL:
    if (0 == arity && eq(lt_nil, parms)) return 0;
    if (0 == arity) return cons_length(parms);
    if (eq(lt_nil, parms)) return arity;
    arity--;
    parms = CDR(parms);
    goto LABEL;
    /* return check_arity(arity - 1, CDR(parms)); */
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
    while (!is_tail(args)) {
        LispObject val;

        val = CALL_EVAL(eval_sexp, CAR(args));
        push_object(val);
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
#ifdef FS
    switch (enum_type_of(op)) {
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
        /* int result = check_arity(ARITY(op), args); */
        /* if (result != 0) { */
        /*     write_format(standard_error, "Wrong number of args in %!\n", op); */
        /*     exit(1); */
        /* } else { */
            if (REGULAR == FTYPE(op))
                args = CALL_EVAL(eval_args, args);
            else {
                Cons tmp;

                tmp = args;
                while (!is_tail(tmp)) {
                    push_object(CAR(tmp));
                    tmp = CDR(tmp);
                }
            }
            check_arity_pattern(ARITY(op), args);
            /* return invoke_function(op, args, denv); */
            return invoke_function(op, args, lenv, denv, benv, fenv);
        /* } */
    }
    default :
        write_format(standard_error, "%! isn't a functional object.\n", op);
        exit(1);
    }
#else
    if (is_special_operator(op))
        return CALL_EVAL(eval_spec, exps);
    else {
        /* Regular function application */
        Function fn;

        fn = CALL_EVAL(eval_operator, op);
        /* int result = check_arity(ARITY(fn), args); */
        /* if (result != 0) { */
        /*     write_format(standard_error, "Wrong number of args in $!\n", fn); */
        /*     exit(1); */
        /* } else { */
            args = CALL_EVAL(eval_args, args);
            check_arity_pattern(ARITY(fn), args);

            return invoke_function(fn, args, denv);
        /* } */
    }
#endif
}

LispObject eval_atom(Atom exp, Environment lenv, Environment denv)
{
    if (SYMBOL_P(exp)) {
	LispObject tmp;

        tmp = get_value(exp, lenv);
	if (tmp != NULL)
	    return tmp;
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
