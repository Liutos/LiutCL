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
extern Symbol lt_true, lt_false, lt_void, lt_t;

LispObject get_value_in_one(Symbol symbol, Environment env)
{
    SymValMap map;

    map = env->map;
    while (map != NULL) {
	if (map->symbol == symbol)
	    return map->value;
	map = map->next;
    }

    return NULL;
}

LispObject get_value(Symbol symbol, Environment env)
{
    LispObject value;

    while (env != NULL) {
	value = get_value_in_one(symbol, env);
	if (value != NULL)
	    return value;
	env = env->next_env;
    }

    return NULL;
}

Environment extend_binding(Symbol symbol, LispObject value, Environment env)
{
    SymValMap node;

    node = malloc(sizeof(struct SymValMap));
    node->symbol = symbol;
    node->value = value;
    node->next = env->map->next; /* Inserts at the head position */
    env->map->next = node;

    return env;
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

    env = malloc(sizeof(struct Environment));
    env->map = malloc(sizeof(struct SymValMap));
    env->map->next = NULL;
    env->next_env = NULL;

    return env;
}

Environment add_primitive(char *func_name, primitive_t prim, Environment env)
{
    Function func;

    func = make_c_fun_object(prim);

    return extend_binding(ensure_symbol_exists(func_name), func, env);
}

Environment init_primitives(Environment env)
{
    Environment tmp;

    tmp = env;
    /* Add primitives */
    reg("add-two", add_two);	/* Original lisp function name is plus-two. */
    reg("mul-two", mul_two);	/* Original lisp function name is mult-two. */
    reg("quit", quit);
    reg("gt-two", gt_two);
    reg("and-two", and_two);
    reg("sub-two", sub_two);
    reg("div-two", div_two);
    reg("or-two", or_two);
    reg("get-cons-car", get_cons_car);
    reg("get-cons-cdr", get_cons_cdr);
    reg("numeric-eq", numeric_eq);
    reg("lt-eq", lt_eq);

    /* lt_void = ensure_symbol_exists("nil"); */
    /* lt_t = ensure_symbol_exists("t"); */

    /* tmp = extend_binding_by_name("nil", lt_void, tmp); */
    /* tmp = extend_binding_by_name("t", lt_t, tmp); */

    return tmp;
}

Environment init_variables(Environment env)
{
    Environment tmp = env;

    lt_void = ensure_symbol_exists("nil");
    lt_t = ensure_symbol_exists("t");

    tmp = extend_binding_by_name("nil", lt_void, tmp);
    tmp = extend_binding_by_name("t", lt_t, tmp);

    return tmp;
}

void describe_one_env(Environment env)
{
    SymValMap map;

    map = env->map->next;
    while (map != NULL) {
	print_atom(map->symbol);
	printf("\t->\t");
	if (is_atom_object(map->value))
	    print_atom(map->value);
	else
	    printf("%p", map->value);
	putchar('\n');
	map = map->next;
    }
}

void describe_env(Environment env)
{
    while (env != NULL) {
	describe_one_env(env);
	env = env->next_env;
    }
}

Environment new_apply_env(Cons parms, Cons values, Environment env)
/* This function is just used for creating a new environment for
   function application. */
{
    Environment nenv;

    nenv = new_env();
    nenv->next_env = env;
    extend_cons_binding(parms, values, nenv);

    return nenv;
}
