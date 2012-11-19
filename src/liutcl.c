/*
 * liutcl.c
 *
 * 
 *
 * Copyright (C) 2012-10-15 liutos mat.liutos@gmail.com
 */
#include <stdio.h>

#include "environment.h"
#include "eval_sexp.h"
#include "function.h"
#include "hash_table.h"
#include "init.h"
#include "package.h"
#include "parse_sexp.h"
#include "print_sexp.h"
#include "read_sexp.h"
#include "stream.h"
#include "symbol.h"
#include "types.h"

int main(int argc, char *argv[])
{
    BlockEnvironment benv;
    Environment fenv, lenv;
    GoEnvironment genv;
    LispObject sexp, result;
    char *input;

    /* Initialize the global symbol table */
    init_packages();
    init_exprs = make_hash_table_t(47, hash_ptr, ptr_cmp);
    /* Initialize the global constant environment */
    global_constant_env = make_empty_env();
    global_constant_env = init_cvars(global_constant_env);
    /* Initialize the global dynamic scope environment */
    global_dynamic_env = make_empty_env();
    global_dynamic_env = init_dvars(global_dynamic_env);

    benv = NULL;
    genv = NULL;
    fenv = make_empty_env();
    fenv = init_primitives(fenv);
    lenv = make_empty_env();
    setjmp(toplevel);

    do {
        write_format(standard_output, "CL-USER> ");
        fflush(STREAM_FILE(standard_output));

        input = read_sexp(stdin);
        sexp = parse_input(input);
        result = eval_sexp(sexp, lenv, global_dynamic_env, fenv, benv, genv, TRUE);
        print_object(result, standard_output);
        free_sexp(input);
    } while (1);

    return 0;
}

