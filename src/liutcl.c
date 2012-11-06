/*
 * liutcl.c
 *
 * 
 *
 * Copyright (C) 2012-10-15 liutos mat.liutos@gmail.com
 */
#include <stdio.h>

#include "env_types.h"
#include "environment.h"
#include "eval_sexp.h"
#include "init.h"
#include "parse_sexp.h"
#include "print_sexp.h"
#include "read_sexp.h"
#include "stream.h"
#include "types.h"

int main(int argc, char *argv[])
{
    Environment lexical_env;
    Environment fdefinition_env;
    char *input;
    LispObject sexp, result;
    BlockEnvironment block_env;

    init_special_operators();
    global_dynamic_env = make_empty_env();
    global_dynamic_env = init_dvars(global_dynamic_env);
    block_env = NULL;
    fdefinition_env = make_empty_env();
    fdefinition_env = init_primitives(fdefinition_env);
    lexical_env = make_empty_env();
    lexical_env = init_variables(lexical_env);
    do {
        write_format(standard_output, "LT-USER> ");
        fflush(stdout);
        input = read_sexp(stdin);
        sexp = parse_sexp(input);
        result = eval_sexp(sexp, lexical_env, global_dynamic_env, block_env, fdefinition_env);
        print_object(result, standard_output);
    } while (1);

    return 0;
}

