/*
 * xread-print.c
 *
 * Test about parsing a string to generate the inner structures of S-expression 
 * and printing them to terminal the same as the input string
 *
 * Copyright (C) 2012-10-03 liutos
 */
#include "print_sexp.h"
#include "parse_sexp.h"
#include "types.h"
#include "environment.h"
#include "atom_proc.h"
#include "eval_sexp.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
    Environment global_env;
    char *expr;

    /* The following three lines of code is... */
    init_symbol_table();
    global_env = NULL;
    global_env = init_environment(global_env);
    /* VERY IMPORTANT! Don't comment them! */
    expr = "(div-two (plus-two 2 2) (sub-two 3 1))";
    print_sexp(eval_sexp(parse_sexp(expr), global_env));

    return 0;
}
