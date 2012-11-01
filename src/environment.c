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
#include "cons.h"
#include "env_types.h"
#include "stream.h"

#include <stdio.h>
#include <stdlib.h>

extern void print_atom(Atom);

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
    while (!is_tail(symbols)) {
	if (is_tail(values)) {
	    fprintf(stderr, "Too less values.\n");
	    exit(1);
	}
	env = extend_binding(CAR(symbols), CAR(values), env);
        /* The two calling to function `is_tail' above ensures that
           the parameters `symbols' and `values' would never be a
           empty list in the two lines of code below. */
	symbols = CDR(symbols);
	values = CDR(values);
    }
    if (is_tail(values))
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
#define reg(name, prim) tmp = add_primitive(name, prim, tmp)

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
    reg("car", lt_car);
    reg("cdr", lt_cdr);
    reg("numeric-eq", numeric_eq);
    reg("eq", lt_eq);
    reg("cons", lt_cons);
    reg("type-of", lt_type_of);
    reg("lt/read-a-char", lt_read_a_char);
    reg("lt/write-a-char", lt_write_a_char);

    return tmp;
}

Environment init_variables(Environment env)
{
    Environment tmp = env;

    lt_nil = ensure_symbol_exists("nil");
    lt_t = ensure_symbol_exists("t");

    tmp = extend_binding_by_name("nil", lt_nil, tmp);
    tmp = extend_binding_by_name("t", lt_t, tmp);
    standard_output = make_file_stream(stdout);
    tmp = extend_binding_by_name("*standard-output*", standard_output, tmp);
    standard_input = make_file_stream(stdin);
    tmp = extend_binding_by_name("*standard-input*", standard_input, tmp);

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

Environment new_binding_env(Cons parms, Cons values, Environment env)
/* This function is just used for creating a new environment for
   function application. And actually you can use it at anywhere
   when you'd like to bind some lexical variables but don't want
   to pollute the outer lexical environment. */
{
    Environment nenv;

    nenv = new_env();
    nenv->next_env = env;
    extend_cons_binding(parms, values, nenv);

    return nenv;
}

SymValMap make_single_map(Symbol symbol, LispObject value)
{
    SymValMap map = malloc(sizeof(struct SymValMap));

    map->symbol = symbol;
    map->value = value;
    map->next = NULL;

    return map;
}

BlockEnvironment make_block_env(Symbol name, jmp_buf context, BlockEnvironment prev_block_env)
{
    BlockEnvironment block_env = malloc(sizeof(struct block_environment));

    block_env->name = name;
    *block_env->context = *context;
    block_env->prev = prev_block_env;

    return block_env;
}
