/*
 * environment.c
 *
 * Creation, extension and search operations on environment.
 *
 * Copyright (C) 2012-10-05 liutos
 */
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>

#include "cons.h"
#include "eval_sexp.h"
#include "hash_table.h"
#include "package.h"
#include "print_sexp.h"
#include "stream.h"
#include "symbol.h"
#include "types.h"

Environment global_constant_env;
Environment global_dynamic_env;

table_entry_t get_value_aux(Symbol symbol, Environment env)
{
    return find_table_entry(symbol, env->map);
}

table_entry_t get_entry(Symbol var, Environment env)
{
    while (env != NULL) {
	table_entry_t ent;

        ent = get_value_aux(var, env);
	if (ent != NULL)
	    return ent;
	env = env->next;
    }

    return NULL;
}

LispObject get_value(Symbol symbol, Environment env)
{
    table_entry_t ent;

    ent = get_entry(symbol, env);
    if (ent != NULL)
        return (LispObject)(ent->value);
    else
        return NULL;
}

Environment extend_env(Symbol symbol, LispObject value, Environment env)
{
    add_key_value(symbol, value, env->map);

    return env;
}

Environment extend_env_by_kvs(Cons symbols, Cons values, Environment env)
{
    while (!TAIL_P(symbols)) {
	if (TAIL_P(values)) {
	    error_format("Too less values.\n");
	    exit(1);
	}
	env = extend_env(CAR(symbols), CAR(values), env);
	symbols = CDR(symbols);
	values = CDR(values);
    }
    if (TAIL_P(values))
	return env;
    else {
	error_format("Too much symbols.\n");
        longjmp(toplevel, 1);
    }
}

Environment extend_env_by_name(char *name, Package pkg, LispObject value, Environment env)
{
    return extend_env(gen_symbol(name, pkg), value, env);
}

Environment make_empty_env(void)
{
    Environment env;

    env = malloc(sizeof(struct environment_t));
    env->map = make_hash_table_t(47, hash_ptr, ptr_cmp);
    env->next = NULL;

    return env;
}

void print_kv(void *key, void *value)
{
    Symbol sym;
    LispObject val;

    sym = (Symbol)key;
    val = (LispObject)value;
    write_format(standard_output, "$!\t->\t", sym);
    if (!CONS_P(val))
        print_atom(val, standard_output);
    else
        write_address(standard_output, val);
    write_char(standard_output, TO_CHAR('\n'));
}

void describe_env_aux(Environment env, Stream stream)
{
    hash_table_t map;

    map = env->map;
    hash_table_iterate(map, print_kv);
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

    return extend_env_by_kvs(parms, values, env);
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

GoEnvironment make_go_env(List tags, jmp_buf context, GoEnvironment prev_go_env)
{
    GoEnvironment go_env;

    go_env = malloc(sizeof(struct go_environment_t));
    go_env->tags = tags;
    *go_env->context = *context;
    go_env->prev = prev_go_env;

    return go_env;
}

BOOL is_go_able(LispObject tag, GoEnvironment genv)
{
    List tags;

    tags = genv->tags;
    while (CONS_P(tags)) {
        if (eq(tag, CAR(tags)))
            return TRUE;
        tags = CDR(tags);
    }

    return FALSE;
}

void update_env(Symbol var, LispObject val, Environment env)
{
    table_entry_t ent;

    ent = get_entry(var, env);
    if (ent != NULL)
        ent->value = val;
    else
        extend_env(var, val, env);
}
