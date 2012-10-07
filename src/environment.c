/*
 * environment.c
 *
 * Operators about processing the environment.
 *
 * Copyright (C) 2012-10-05 liutos
 */
#include "types.h"
#include "atom_proc.h"
#include "primitives.h"
#include "object.h"
#include <stdio.h>
#include <stdlib.h>

#define reg(name, prim) tmp = add_primitive(name, prim, tmp)

extern void print_atom(Atom);
extern Symbol lt_true, lt_false, lt_void;

LispObject get_value(Symbol symbol, Environment env)
{
    while (env != NULL) {
	if (env->symbol == symbol)
	    return env->value;
	env = env->next;
    }

    return NULL;
}

Environment extend_binding(Symbol symbol, LispObject value, Environment env)
{
    Environment node;

    node = malloc(sizeof(struct SymValMap));
    node->symbol = symbol;
    node->value = value;
    node->next = env;

    return node;
}

/* For binding the parameters of a closure. */
Environment extend_cons_binding(Cons symbols, Cons values, Environment env)
{
    while (symbols != NULL) {
	if (NULL == values) {
	    fprintf(stderr, "Too less values.\n");
	    exit(1);
	}
	env = extend_binding(CAR(symbols), CAR(values), env);
	symbols = CDR(symbols);
	values = CDR(values);
    }
    if (NULL == values)
	return env;
    else {
	fprintf(stderr, "Too much symbols.\n");
	exit(1);
    }
}

Environment extend_binding_by_name(char *name, LispObject value, Environment env)
{
    return extend_binding(ensure_symbol_exists(name), value, env);
}

Environment new_env(void)
{
    Environment env;

    env = malloc(sizeof(struct SymValMap));
    env->next = NULL;

    return env;
}

Environment add_primitive(char *func_name, primitive_t prim, Environment env)
{
    Function func;

    func = make_c_fun_object(prim);

    return extend_binding(ensure_symbol_exists(func_name), func, env);
}

Environment init_environment(Environment env)
{
    Environment tmp;

    tmp = env;
    /* Add primitives */
    reg("plus-two", plus_two);
    reg("mult-two", mult_two);
    reg("quit", quit);
    reg("gt-two", gt_two);
    reg("and-two", and_two);
    reg("sub-two", sub_two);
    reg("div-two", div_two);
    /* Add variables */
    lt_true = new_object();
    lt_true->type = BOOLEAN;
    tmp = extend_binding_by_name("#t", lt_true, tmp);
    lt_false = new_object();
    TYPE(lt_false) = BOOLEAN;
    tmp = extend_binding_by_name("#f", lt_false, tmp);
    lt_void = ensure_symbol_exists("void");

    tmp = extend_binding_by_name("void", lt_void, tmp);

    return tmp;
}

void describe_env(Environment env)
{
    while (env != NULL) {
	print_atom(env->symbol);
	printf("\t->\t%p\n", env->value);
	env = env->next;
    }
}
