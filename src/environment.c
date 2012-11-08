/*
 * environment.c
 *
 * 
 *
 * Copyright (C) 2012-10-05 liutos
 */
#include <stdio.h>
#include <stdlib.h>

#include "atom.h"
#include "cons.h"
#include "env_types.h"
#include "package.h"
#include "print_sexp.h"
#include "stream.h"
#include "symbol.h"
#include "types.h"

Environment global_constant_env;
Environment global_dynamic_env;

LispObject get_value_aux(Symbol symbol, Environment env)
{
    env_entry_t map;

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
    while (env != NULL) {
	LispObject value;

        value = get_value_aux(symbol, env);
	if (value != NULL)
	    return value;
	env = env->next;
    }

    return NULL;
}

Environment extend_env(Symbol symbol, LispObject value, Environment env)
{
    env_entry_t node;

    node = malloc(sizeof(struct env_entry_t));
    node->symbol = symbol;
    node->value = value;
    node->next = env->map->next;
    env->map->next = node;

    return env;
}

Environment extend_env_by_cons(Cons symbols, Cons values, Environment env)
{
    while (!TAIL_P(symbols)) {
	if (TAIL_P(values)) {
	    fprintf(stderr, "Too less values.\n");
	    exit(1);
	}
	env = extend_env(CAR(symbols), CAR(values), env);
	symbols = CDR(symbols);
	values = CDR(values);
    }
    if (TAIL_P(values))
	return env;
    else {
	fprintf(stderr, "Too much symbols.\n");
	exit(1);
    }
}

Environment extend_env_by_name(char *name, Package pkg, LispObject value, Environment env)
{
    return extend_env(gen_pkg_sym(name, pkg), value, env);
}

Environment make_empty_env(void)
{
    Environment env;

    env = malloc(sizeof(struct environment_t));
    env->map = malloc(sizeof(struct env_entry_t));
    env->map->next = NULL;
    env->next = NULL;

    return env;
}

void describe_env_aux(Environment env, Stream stream)
{
    env_entry_t map;

    map = env->map->next;
    while (map != NULL) {
        write_format(stream, "$!\t->\t", map->symbol);
	if (!CONS_P(map->value))
	    print_atom(map->value, stream);
	else
	    printf("%p", map->value);
        write_char(standard_output, TO_CHAR('\n'));
	map = map->next;
    }
}

void describe_env(Environment env, Stream stream)
{
    while (env != NULL) {
	describe_env_aux(env, stream);
	env = env->next;
    }
}

Environment make_new_env(Cons parms, Cons values, Environment prev_env)
{
    Environment env;

    env = make_empty_env();
    env->next = prev_env;

    return extend_env_by_cons(parms, values, env);
}

BlockEnvironment make_block_env(Symbol name, jmp_buf context, BlockEnvironment prev_block_env)
{
    BlockEnvironment block_env;

    block_env = malloc(sizeof(struct block_environment_t));
    block_env->name = name;
    *block_env->context = *context;
    block_env->prev = prev_block_env;

    return block_env;
}
