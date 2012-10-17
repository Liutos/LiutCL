/*
 * liutcl.c
 *
 *
 *
 * Copyright (C) 2012-10-15 liutos mat.liutos@gmail.com
 */
#include "atom_proc.h"
#include "environment.h"
#include "types.h"
#include "parse_sexp.h"
#include "read_sexp.h"
#include "eval_sexp.h"
#include "print_sexp.h"
#include "env_types.h"
#include <stdio.h>

int main(int argc, char *argv[])
{
    Environment lexical_env;    /* Lexical environment */
    Environment dynamic_env;    /* Dynamic environment */
    char *expr;
    LispObject sexp;

    init_symbol_table();
    lexical_env = new_env();
    lexical_env = init_variables(lexical_env);
    dynamic_env = new_env();
    dynamic_env = init_primitives(dynamic_env);
    do {
        expr = read_sexp(stdin);
        sexp = parse_sexp(expr);
        print_sexp(eval_sexp(sexp, lexical_env, dynamic_env));
    } while (1);

    return 0;
}

