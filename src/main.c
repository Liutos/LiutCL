/*
 * main.c
 *
 * The interpreter for a Lisp dialect named LiutCL
 *
 * Copyright (C) 2012-04-15 liutos
 */
#include "eval.h"
#include "model.h"
#include "primitives.h"
#include "print.h"
#include "read.h"
#include "types.h"

#include <stdio.h>
#include <stdlib.h>

ENVIRONMENT *make_toplevel_env(void)
{
    ENVIRONMENT *env;

    env = malloc(sizeof(ENVIRONMENT));
    env->type = ATOM;
    env->atom_type = LOOKUP_TABLE;
    env->head_node = malloc(sizeof(struct LookupEntry));
    env->head_node->symbol_name = NULL;
    env->head_node->symbol_object = NULL;
    env->head_node->next = NULL;
    env->next_env = NULL;

    return env;
}

int main(int argc, char *argv[])
{
    char *expression;
    struct LispObject *env;
    struct LispObject *object, *value;

    env = make_toplevel_env();
    init_primitives(env);

    do {
    	printf("LiutCL> ");
    	expression = read_expression(stdin);
    	object = make_object(expression, env);
	value = eval_expression(object, env);
	print_object(value);
    } while (1);

    return 0;
}
