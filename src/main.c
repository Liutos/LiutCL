/*
 * main.c
 *
 *
 *
 * Copyright (C) 2012-04-15 liutos
 */
#include "eval.h"
#include "model.h"
#include "primitives.h"
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

    /* print_object(&env);		/\* This function call is for debugging *\/ */
    do {
	printf("LiutCL> ");
	expression = read_expression(stdin);
	object = make_object(expression, &env);
	/* print_object(object);	/\* For debugging *\/ */
	print_object(eval_expression(object, &env));
	/* print_object(&env); */
    } while (1);

    return 0;
}
