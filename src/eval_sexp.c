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

/* extern Symbol lt_quote, lt_if, lt_begin, lt_lambda, lt_set; */

LispObject eval_sexp(LispObject, Environment, Environment);
LispObject eval_cons(Cons, Environment, Environment);

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
/* Invokes a closure means evaluating the function body's expressions in
   a locally newly extended environment. */
{
    Environment env;

    /* This is the `newly extended environment'. */
    /* For processing correctly, there must be a function designed for
       interpreted function invokation because at that case, the outer
       environment exists when the closure was created, its content
       shouldn't be modified by the function application. */
    env = new_apply_env(PARAMETERS(ifunc), args, LOCAL_ENV(ifunc));
    /* The code below commented is the old one. */

    /* env = extend_cons_binding(PARAMETERS(ifunc), */
    /* 			      args, */
    /* 			      LOCAL_ENV(ifunc)); */

    return eval_sexp(EXPRESSION(ifunc), env, FUNCTION(ifunc)->denv);
}

LispObject invoke_function(Function func, Cons args)
{
    if (TRUE == FUNC_FLAG(func))
	return invoke_c_fun(func, args);
    else
	return invoke_i_fun(func, args);
}

LispObject eprogn(Cons exps, Environment env, Environment denv)
{
    if (is_tail(exps))
	return lt_void;
    if (is_tail(CDR(exps)))
	return eval_sexp(CAR(exps), env, denv);
    while (!is_tail(exps)) {
	if (is_tail(CDR(exps)))
	    break;
	eval_sexp(CAR(exps), env, denv);
	exps = CDR(exps);
    }

    return eval_sexp(CAR(exps), env, denv);
}

LispObject eval_args(Cons args, Environment env, Environment denv)
{
    if (!is_tail(args))
	return make_cons_cell(eval_sexp(CAR(args), env, denv),
			      eval_args(CDR(args), env, denv));
    else
	return lt_void;
}

LispObject eval_operator(LispObject op, Environment env, Environment denv)
{
    if (is_symbol(op)) {
	LispObject tmp;

	tmp = get_value(op, denv);
	if (tmp != NULL)
	    return get_value(op, denv);
	else {
	    fprintf(stderr, "No binding of symbol %s.\n", op->symbol_name);
	    exit(1);
	}
    } else
	return eval_cons(op, env, denv);
}

LispObject eval_cons(Cons exps, Environment env, Environment denv)
{
    Function func;

    /* Cases of special operators */
    if (CAR(exps) == lt_quote)
	return CAR(CDR(exps));
    if (CAR(exps) == lt_if) {
	if (is_true_obj(eval_sexp(FIRST(CDR(exps)), env, denv)))
	    return eval_sexp(SECOND(CDR(exps)), env, denv);
	else {
	    Cons argv = CDR(exps);

	    return eval_sexp(safe_car(safe_cdr(safe_cdr(argv))), env, denv);
	}
    }
    if (CAR(exps) == lt_begin)
	return eprogn(CDR(exps), env, denv);
    if (CAR(exps) == lt_set) {
        extend_binding(SECOND(exps),
                       eval_sexp(THIRD(exps), env, denv),
                       env);

        return lt_void;
    }

    if (CAR(exps) == lt_lambda)
	return make_i_fun_object(SECOND(exps), THIRD(exps), env, denv);
    /* Process the special operator added by myself. */
    if (CAR(exps) == lt_dset) {
	extend_binding(SECOND(exps),
		       eval_sexp(THIRD(exps), env, denv),
		       denv);

	return lt_void;
    }
    if (CAR(exps) == lt_dynamic)
	return eval_atom(eval_sexp(SECOND(exps), env, denv), denv);

    func = eval_operator(CAR(exps), env, denv);
    if (NULL == func) {
	fprintf(stderr, "Null-pointer exception.\n");
	exit(1);
    }

    return invoke_function(func, eval_args(CDR(exps), env, denv));
}

LispObject eval_sexp(LispObject exp, Environment env, Environment denv)
/* Parameter 'denv' means dynamic environment. */
{
    if (TYPE(exp) != CONS)
	return eval_atom(exp, env);
    else
	return eval_cons(exp, env, denv);
}
