/*
 * eval_sexp.c
 *
 * Evaluator for S-expressions.
 *
 * Copyright (C) 2012-10-04 liutos
 */
#include "types.h"
#include "environment.h"
#include "atom_proc.h"
#include "primitives.h"
#include "print_sexp.h"
#include "cons.h"
#include <stdio.h>
#include <stdlib.h>

extern Symbol lt_quote, lt_if, lt_begin, lt_void, lt_lambda;
LispObject eval_sexp(LispObject, Environment);

BOOL is_symbol(LispObject object)
{
    return SYMBOL == object->type;
}

LispObject eval_atom(Atom exp, Environment env)
{
    LispObject tmp;

    if (is_symbol(exp)) {
	tmp = get_value(exp, env);
	if (tmp != NULL)
	    return tmp;
	else {
	    fprintf(stderr, "No binding of symbol %s.\n", exp->symbol_name);
	    exit(0);
	}
    } else
	return exp;
}

LispObject invoke_c_fun(Function cfunc, Cons args)
{
    return PRIMITIVE(cfunc)(args);
}

LispObject invoke_i_fun(Function ifunc, Cons args)
{
    Environment env;

    env = extend_cons_binding(PARAMETERS(ifunc),
			      args,
			      LOCAL_ENV(ifunc));
    /* describe_env(env); */

    return eval_sexp(EXPRESSION(ifunc), env);
}

LispObject invoke_function(Function func, Cons args)
{
    if (TRUE == FUNC_FLAG(func))
	return invoke_c_fun(func, args);
    else
	return invoke_i_fun(func, args);
}

LispObject eprogn(Cons exps, Environment env)
{
    if (NULL == exps)
	return lt_void;
    if (NULL == CDR(exps))
	return eval_sexp(CAR(exps), env);
    while (exps != NULL) {
	if (NULL == CDR(exps))
	    break;
	eval_sexp(CAR(exps), env);
	exps = CDR(exps);
    }

    return eval_sexp(CAR(exps), env);
}

LispObject eval_args(Cons args, Environment env)
{
    if (args != NULL)
	return make_cons_cell(eval_sexp(CAR(args), env),
			      eval_args(CDR(args), env));
    else
	return NULL;
}

LispObject eval_cons(Cons exps, Environment env)
{
    Function func;

    /* if (FALSE == is_symbol(CAR(exps))) { */
    /* 	fprintf(stderr, "Do not support non-symbol object at function position: "); */
    /* 	print_sexp(CAR(exps)); */
    /* 	exit(1); */
    /* } */
    if (CAR(exps) == lt_quote)
	return CAR(CDR(exps));
    if (CAR(exps) == lt_if) {
	if (is_true_obj(eval_sexp(FIRST(CDR(exps)), env)))
	    return eval_sexp(SECOND(CDR(exps)), env);
	else
	    return eval_sexp(THIRD(CDR(exps)), env);
    }
    if (CAR(exps) == lt_begin)
	return eprogn(CDR(exps), env);
    if (CAR(exps) == lt_lambda)
	return make_i_fun_object(SECOND(exps), THIRD(exps), env);
    /* func = get_value(CAR(exps), env); */
    func = eval_sexp(CAR(exps), env);
    if (NULL == func) {
	fprintf(stderr, "Null-pointer exception.\n");
	exit(1);
    }

    return invoke_function(func, eval_args(CDR(exps), env));
}

LispObject eval_sexp(LispObject exp, Environment env)
{
    if (TYPE(exp) != CONS)
	return eval_atom(exp, env);
    else
	return eval_cons(exp, env);
}
