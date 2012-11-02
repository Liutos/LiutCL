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
#include "stream.h"

#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <string.h>

#define eq(x, y) (x == y)
#define DEFEVAL(name, hd) LispObject name(LispObject hd, Environment env, Environment denv, BlockEnvironment block_env, Environment fenv)

/* LispObject eval_sexp(LispObject, Environment, Environment, BlockEnvironment, Environment); */
/* LispObject eval_cons(Cons, Environment, Environment, BlockEnvironment, Environment); */
DEFEVAL(eval_sexp, _);
DEFEVAL(eval_cons, _);

jmp_buf context;

LispObject eval_atom(Atom exp, Environment env)
{
    LispObject tmp;

    if (is_symbol(exp)) {
	tmp = get_value(exp, env);
	if (tmp != NULL)
	    return tmp;
	else {
	    fprintf(stderr, "No binding of symbol %s.\n", SYMBOL_NAME(exp));
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

    return eval_sexp(EXPRESSION(ifunc), env, FUNCTION(ifunc)->denv, BLOCK_ENV(ifunc), FENV(ifunc));
}

LispObject invoke_function(Function func, Cons args)
{
    if (TRUE == FUNC_FLAG(func))
	return invoke_c_fun(func, args);
    else
	return invoke_i_fun(func, args);
}

DEFEVAL(eprogn, exps)
/* This function name used in _Lisp in Small Pieces_. */
{
    if (is_tail(exps))
	return lt_nil;
    /* The if statement above ensures that the parameter `exps' used
       below would never be the lt_void, the empty list in the Lisp
       level. */
    if (is_tail(CDR(exps)))
	return eval_sexp(CAR(exps), env, denv, block_env, fenv);
    while (!is_tail(exps)) {
	if (is_tail(CDR(exps)))
	    break;
	eval_sexp(CAR(exps), env, denv, block_env, fenv);
	exps = CDR(exps);
    }

    return eval_sexp(CAR(exps), env, denv, block_env, fenv);
}

DEFEVAL(eval_args, args)
{
    if (!is_tail(args))
	return make_cons_cell(eval_sexp(CAR(args), env, denv, block_env, fenv),
			      eval_args(CDR(args), env, denv, block_env, fenv));
    else
	return lt_nil;
}

DEFEVAL(eval_operator, op)
{
    if (is_symbol(op)) {
	LispObject tmp;

	tmp = get_value(op, fenv);
	if (tmp != NULL)
	    return tmp;
	else {
	    fprintf(stderr, "No binding of symbol %s.\n", SYMBOL_NAME(op));
	    exit(1);
	}
    } else
	return eval_cons(op, env, denv, block_env, fenv);
}

DEFEVAL(eval_catch, argv)
{
    jmp_buf tmp_context;
    int val;
    Symbol label;

    label = eval_sexp(FIRST(argv), env, denv, block_env, fenv);
    memcpy(tmp_context, context, sizeof(jmp_buf));
    val = setjmp(context);
    if (0 == val) {
        LispObject val = eprogn(SCDR(argv), env, denv, block_env, fenv);
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

DEFEVAL(eval_cons, exps)
/* This function should handles only the Lisp objects of type Cons, 
   not included the symbol `nil'. */
{
    Function func;
    Cons argv;
    LispObject op;

 INTERP:                        /* The support of tail-recursion optimization */
    argv = SCDR(exps);
    op = SCAR(exps);
    /* Cases of special operators in Scheme */
    if (eq(op, lt_quote))
	return CAR(argv);
    if (eq(op, lt_if)) {   /* 如果把if对应的操作给函数化了，那么就不能应用goto来实现尾递归优化了。 */
	if (is_true_obj(eval_sexp(FIRST(argv), env, denv, block_env, fenv))) {
	    exps = SECOND(argv); /* A try of tail-recursive optimization. */
            goto INTERP;
	} else {
            exps = THIRD(argv);
            goto INTERP;
        }
    }
    if (eq(op, lt_begin))
	return eprogn(argv, env, denv, block_env, fenv);
    if (eq(op, lt_set)) {
        extend_binding(SCAR(argv),
                       eval_sexp(SCAR(SCDR(argv)), env, denv, block_env, fenv),
                       env);

        return lt_nil;
    }

    if (eq(op, lt_lambda)) /* LAMBDA */
	return make_i_fun_object(SECOND(exps), THIRD(exps), env, denv, block_env);
    /* Process the special operator added by myself. */
    if (eq(op, lt_dset)) {
	extend_binding(SECOND(exps),
		       eval_sexp(THIRD(exps), env, denv, block_env, fenv),
		       denv);

	return lt_nil;
    }
    if (eq(op, lt_dynamic)) /* Symbol LT/DYNAMIC */
	return eval_atom(eval_sexp(SECOND(exps), env, denv, block_env, fenv), denv);
    if (eq(op, lt_catch)) /* Symbol LT/CATCH */
        return eval_catch(argv, env, denv, block_env, fenv);
    if (eq(op, lt_throw)) { /* Symbol LT/THROW */
        Symbol label = eval_sexp(FIRST(argv), env, denv, block_env, fenv); /* 这里假设了第一个参数是要求值的，因为我一时忘了Common Lisp是否对其进行求值了。 */
        LispObject value = eval_sexp(SECOND(argv), env, denv, block_env, fenv);
        SymValMap map = make_single_map(label, value);

        longjmp(context, (int)map);
    }
    if (eq(op, lt_block)) { /* Symbol LT/BLOCK */
        jmp_buf context;
        int val;
        Symbol name = FIRST(argv);/* eval_sexp(FIRST(argv), env, denv, block_env). This argument should not be evaluated. */;

        val = setjmp(context);
        if (0 == val)
            return eprogn(CDR(argv), env, denv, make_block_env(name, context, block_env), fenv);
        else
            return (LispObject)val;
    }
    if (eq(op, lt_return_from)) { /* Symbol LT/RETURN-FROM */
        Symbol name = FIRST(argv);     /* Name should not be evaluated. */
        LispObject value = eval_sexp(SECOND(argv), env, denv, block_env, fenv);

        while (block_env != NULL) {
            if (block_env->name == name)
                longjmp(block_env->context, (int)value);
            block_env = block_env->prev;
        }
        printf("No such a block environment contains name `");
        print_atom(name, standard_output);
        printf("'.\n");
        exit(1);
    }

    /* Regular function evaluation. */
    func = eval_operator(op, env, denv, block_env, fenv);

    return invoke_function(func, eval_args(argv, env, denv, block_env, fenv));
}

/* exp : inner representation of input expression;
 * env : lexical environment;
 * denv : dynamic environment;
 * block_env : environment contains bindings of name and context.
 */
DEFEVAL(eval_sexp, exp)
/* Parameter 'denv' means dynamic environment. Currently the `denv' also
   stores the function value in it but I think providing a specific
   environmen named fenv would be better. */
{
    if (NULL == exp)
        return NULL;
    if (CONS_P(exp))
        return eval_cons(exp, env, denv, block_env, fenv);
    else
        return eval_atom(exp, env);
}
