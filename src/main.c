/*
 * main.c
 *
 *
 *
 * Copyright (C) 2012-04-15 liutos
 */
#include "eval.h"
#include "init.h"
#include "model.h"
#include "print.h"
#include "read.h"
#include "types.h"

#include <stdlib.h>

int main(int argc, char *argv[])
{
    char *expression;
    struct LispObject env;
    struct LispObject *object;

    env.type = ATOM;
    env.atom_type = LOOKUP_TABLE;
    env.head_node = malloc(sizeof(struct LookupEntry));
    env.head_node->symbol_name = NULL;
    env.head_node->symbol_object = NULL;
    env.head_node->next = NULL;
    init_primitives(&env);

    print_object(&env);
    do {
	printf("CL-USER> ");
	expression = read_expression(stdin);
	object = make_object(expression, &env);
	print_object(eval_expression(object, &env));
    } while (1);

    return 0;
}
