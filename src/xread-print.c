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
    Environment global_env, denv;
    char *expr;

    /* The following three lines of code is... */
    init_symbol_table();
    global_env = new_env();
    global_env = init_variables(global_env);
    denv = new_env();
    denv = init_primitives(denv);
    /* VERY IMPORTANT! Don't comment them! */
    /* describe_env(denv); */

    /* In Lisp-2 language, the first appeared symbol `fact' would be stored
       in lexical environment, the variable `global_env', however, the
       value of symbol `fact' within the lambda would look up within the
       dynamic environment, the variable `denv'. Therefore, the evaluator
       may throw the message that ``No binding of symbol fact.'' */
    expr = "(begin (lt/dset! fact (lambda (n) (if (numeric-eq 0 n) 1 (mul-two n (fact (sub-two n 1)))))) (fact 5))";
    print_sexp(eval_sexp(parse_sexp(expr), global_env, denv));

    return 0;
}
