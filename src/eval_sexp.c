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
#include "primitives.h"
#include "print_sexp.h"
#include "stream.h"
#include "types.h"

#define CALL_EVAL(eval_fn, arg) eval_fn(arg, lenv, denv, benv, fenv)
#define DEFEVAL(name, hd)                       \
    LispObject name(LispObject hd,              \
                    Environment lenv,           \
                    Environment denv,           \
                    BlockEnvironment benv,      \
                    Environment fenv)
#define eq(x, y) (x == y)

DEFEVAL(eval_progn, _);
DEFEVAL(eval_cons, _);
DEFEVAL(eval_sexp, _);

jmp_buf escape;

inline LispObject invoke_C_function(Function C_function, Cons args)
{
    return PRIMITIVE(C_function)(args);
}

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

LispObject invoke_function(Function function, Cons args, Environment denv)
{
    if (TRUE == FUNCTION_CFLAG(function))
	return invoke_C_function(function, args);
    else
	return invoke_Lisp_function(function, args, denv);
}

BOOL is_special_operator(LispObject op)
{
    Symbol lt_specs[] = {
        lt_block,
        lt_catch,
        lt_defvar,
        lt_function,
        lt_fset,
        lt_if,
        lt_lambda,
        lt_progn,
        lt_quote,
        lt_return_from,
        lt_set,
        lt_throw,
    };
    int length = sizeof(lt_specs) / sizeof(Symbol);
    int i;
    for (i = 0; i < length; ++i)
        if (eq(op, lt_specs[i]))
            return TRUE;

    return FALSE;
}

DEFEVAL(eval_block, argv)
{
    jmp_buf context;
    int val;
    Symbol name = FIRST(argv);
    val = setjmp(context);
    if (0 == val)
        return eval_progn(CDR(argv), lenv, denv,
                          make_block_env(name, context, benv), fenv);
    else
        return (LispObject)val;
}

DEFEVAL(eval_catch, argv)
{
    jmp_buf context;
    int val;
    LispObject tag = CALL_EVAL(eval_sexp, FIRST(argv));
    /* Simulates the dynamic scope by twice application of memcpy */
    memcpy(context, escape, sizeof(jmp_buf));
    val = setjmp(escape);
    if (0 == val) {
        LispObject value = CALL_EVAL(eval_progn, CDR(argv));
        memcpy(escape, context, sizeof(jmp_buf));

        return value;
    } else {                    /* Returns from a invokation of THROW */
        Cons cons = (Cons)val;
        memcpy(escape, context, sizeof(jmp_buf));
        if (eq(CAR(cons), tag))
            return CDR(cons);
        else
            longjmp(escape, val);
    }
}

DEFEVAL(eval_defvar, argv)
{
    Symbol name = FIRST(argv);
    if (NULL == get_value(name, global_dynamic_env))
        global_dynamic_env =
            extend_env(name,
                       CALL_EVAL(eval_sexp, SECOND(argv)),
                       global_dynamic_env);

    return name;
}

DEFEVAL(eval_function, arg)
{
    return get_value(arg, fenv);
}

DEFEVAL(eval_progn, exps)
{
    if (is_tail(exps))
	return lt_nil;
    if (is_tail(CDR(exps)))
	return CALL_EVAL(eval_sexp, CAR(exps));
    while (!is_tail(exps)) {
	if (is_tail(CDR(exps)))
	    break;
	CALL_EVAL(eval_sexp, CAR(exps));
	exps = CDR(exps);
    }

    return CALL_EVAL(eval_sexp, CAR(exps));
}

DEFEVAL(eval_return_from, argv)
{
    Symbol name = FIRST(argv);
    LispObject result = CALL_EVAL(eval_sexp, SECOND(argv));
    while (benv != NULL) {
        if (benv->name == name)
            longjmp(benv->context, (int)result);
        benv = benv->prev;
    }
    write_format(standard_error, "No such a block contains name %!\n", name);
    exit(1);
}

DEFEVAL(eval_throw, argv)
{
    LispObject tag = CALL_EVAL(eval_sexp, FIRST(argv));
    LispObject result = CALL_EVAL(eval_sexp, SECOND(argv));
    longjmp(escape, (int)make_cons(tag, result));
}

DEFEVAL(eval_spec, exp)
{
    Cons argv = CDR(exp);
    LispObject op = FIRST(exp);

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
    if (eq(op, lt_set)) {
        extend_env(FIRST(argv), CALL_EVAL(eval_sexp, SECOND(argv)), lenv);

        return lt_nil;
    }
    if (eq(op, lt_throw))
        return CALL_EVAL(eval_throw, argv);
    write_string(standard_error, TO_STRING("WTF!\n"));
    exit(1);
}

/* Argument `parms' must be a proper list. */
int check_arity(int arity, Cons parms)
{
    if (0 == arity && eq(lt_nil, parms)) return 0;
    if (0 == arity) return cons_length(parms);
    if (eq(lt_nil, parms)) return arity;
    return check_arity(arity - 1, CDR(parms));
}

DEFEVAL(eval_operator, op)
{
    if (SYMBOL_P(op)) {
	LispObject tmp = get_value(op, fenv);
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
    Cons head, pre, cur;

    pre = head = make_cons(lt_nil, lt_nil);
    while (!is_tail(args)) {
        cur = make_cons(CALL_EVAL(eval_sexp, CAR(args)), lt_nil);
        _CDR(pre) = cur;
        pre = cur;
        args = CDR(args);
    }

    return CDR(head);
}

DEFEVAL(eval_cons, exps)
{
    Cons args = CDR(exps);
    LispObject op = FIRST(exps);
    if (is_special_operator(op))
        return CALL_EVAL(eval_spec, exps);
    else {
        Function fn = CALL_EVAL(eval_operator, op);
        int result = check_arity(ARITY(fn), args);
        if (result != 0) {
            write_format(standard_error, "Wrong number of args\n");
            exit(1);
        } else {
            args = CALL_EVAL(eval_args, args);

            return invoke_function(fn, args, denv);
        }
    }
}

LispObject eval_atom(Atom exp, Environment lenv, Environment denv)
{
    if (SYMBOL_P(exp)) {
	LispObject tmp = get_value(exp, lenv);
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
