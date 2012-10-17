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
#include "env_types.h"
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <string.h>

LispObject eval_sexp(LispObject, Environment, Environment);
LispObject eval_cons(Cons, Environment, Environment);

jmp_buf context;

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
    env = new_binding_env(PARAMETERS(ifunc), args, LOCAL_ENV(ifunc));

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
/* This function name used in _Lisp in Small Pieces_. */
{
    if (is_tail(exps))
	return lt_nil;
    /* The if statement above ensures that the parameter `exps' used
       below would never be the lt_void, the empty list in the Lisp
       level. */
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
	return lt_nil;
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
/* This function should handles only the Lisp objects of type Cons, 
   not included the symbol `nil'. */
{
    Function func;
    Cons argv;

 INTERP:                        /* The support of tail-recursion optimization */
    argv = SCDR(exps);
    /* Cases of special operators in Scheme */
    if (CAR(exps) == lt_quote)
	return CAR(argv);
    if (CAR(exps) == lt_if) {
	if (is_true_obj(eval_sexp(FIRST(argv), env, denv))) {
	    /* return eval_sexp(SECOND(argv), env, denv); */
            exps = SECOND(argv); /* A try of tail-recursive optimization. */
            goto INTERP;
	} else {
            /* return eval_sexp(SCAR(SCDR(SCDR(argv))), env, denv); */
            exps = THIRD(argv);
            goto INTERP;
        }
    }
    if (CAR(exps) == lt_begin)
	return eprogn(argv, env, denv);
    if (CAR(exps) == lt_set) {
        extend_binding(SCAR(argv),
                       eval_sexp(SCAR(SCDR(argv)), env, denv),
                       env);

        return lt_nil;
    }

    if (CAR(exps) == lt_lambda)
	return make_i_fun_object(SECOND(exps), THIRD(exps), env, denv);
    /* Process the special operator added by myself. */
    if (CAR(exps) == lt_dset) {
	extend_binding(SECOND(exps),
		       eval_sexp(THIRD(exps), env, denv),
		       denv);

	return lt_nil;
    }
    if (CAR(exps) == lt_dynamic)
	return eval_atom(eval_sexp(SECOND(exps), env, denv), denv);
    if (CAR(exps) == lt_catch) { /* Symbol LT/CATCH */
        jmp_buf tmp_context;
        int val;
        Symbol label;

        label = eval_sexp(FIRST(argv), env, denv);
        memcpy(tmp_context, context, sizeof(jmp_buf));
        val = setjmp(context);
        if (0 == val) {
            LispObject val = eprogn(SCDR(argv), env, denv);
            if (tmp_context != NULL)
                memcpy(context, tmp_context, sizeof(jmp_buf));

            return val;
        } else {
            SymValMap tmp_map = (SymValMap)val;

            memcpy(context, tmp_context, sizeof(jmp_buf));
            if (tmp_map->symbol == label)
                return tmp_map->value;
            else
                longjmp(context, (int)tmp_map);
        }
    }
    if (CAR(exps) == lt_throw) { /* Symbol LT/THROW */
        Symbol label = eval_sexp(FIRST(argv), env, denv);
        LispObject value = eval_sexp(SECOND(argv), env, denv);
        SymValMap map = make_single_map(label, value);

        longjmp(context, (int)map);
    }

    func = eval_operator(CAR(exps), env, denv);
    if (NULL == func) {
	fprintf(stderr, "Null-pointer exception.\n");
	exit(1);
    }

    return invoke_function(func, eval_args(argv, env, denv));
}

LispObject eval_sexp(LispObject exp, Environment env, Environment denv)
/* Parameter 'denv' means dynamic environment. Currently the `denv' also
   stores the function value in it but I think providing a specific
   environmen named fenv would be better. */
{
    if (NULL == exp)
        return NULL;
    if (TYPE(exp) != CONS)
	return eval_atom(exp, env);
    else
	return eval_cons(exp, env, denv);
}
