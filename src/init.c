/*
 * init.c
 *
 *
 *
 * Copyright (C) 2012-11-04 liutos <mat.liutos@gmail.com>
 */
#include "atom_proc.h"
#include "environment.h"
#include "primitives.h"
#include "stream.h"
#include "symbol_table.h"
#include "types.h"

#include "primitive/arit.h"
#include "primitive/io.h"
#include "primitive/logic.h"

Environment reg_primitive(char *fn_name, primitive_t prim, int arity, Environment env)
{
    Function fn;

    fn = make_C_function(prim, arity);

    return extend_env(ensure_symbol_exists(fn_name), fn, env);
}

Environment init_primitives(Environment env)
{
#define reg(name, prim, arity) env = reg_primitive(name, prim, arity, env)
    reg("ADD", add, 2);
    reg("MUL", mul, 2);
    reg("QUIT", quit, 0);
    reg("GT", gt, 2);
    reg("AND2", and_two, 2);
    reg("SUB", sub, 2);
    reg("DIV", div, 2);
    reg("OR2", or_two, 2);
    reg("CAR", lt_car, 1);
    reg("CDR", lt_cdr, 1);
    reg("FIXNUM-EQ", fixnum_eq, 2);
    reg("EQ", lt_eq, 2);
    reg("CONS", lt_cons, 2);
    reg("TYPE-OF", lt_type_of, 1);
    reg("LT/READ-A-CHAR", lt_read_a_char, 1);
    reg("LT/WRITE-A-CHAR", lt_write_a_char, 1);

    return env;
}

Environment init_variables(Environment env)
{
    Environment tmp = env;

    lt_nil = ensure_symbol_exists("NIL");
    tmp = extend_env_by_name("NIL", lt_nil, tmp);

    lt_t = ensure_symbol_exists("T");
    tmp = extend_env_by_name("T", lt_t, tmp);

    standard_output = make_file_stream(stdout);
    tmp = extend_env_by_name("*STANDARD-OUTPUT*", standard_output, tmp);

    standard_input = make_file_stream(stdin);
    tmp = extend_env_by_name("*STANDARD-INPUT*", standard_input, tmp);

    standard_error = make_file_stream(stderr);
    tmp = extend_env_by_name("*STANDARD-ERROR*", standard_error, tmp);

    return tmp;
}

void init_special_operators(void)
{
    lt_block = ensure_symbol_exists("BLOCK");
    lt_catch = ensure_symbol_exists("CATCH");
    lt_defvar = ensure_symbol_exists("DEFVAR");
    lt_fset = ensure_symbol_exists("LT/FSET!");
    lt_function = ensure_symbol_exists("FUNCTION");
    lt_if = ensure_symbol_exists("IF");
    lt_lambda = ensure_symbol_exists("LAMBDA");
    lt_progn = ensure_symbol_exists("PROGN");
    lt_quote = ensure_symbol_exists("QUOTE");
    lt_return_from = ensure_symbol_exists("RETURN-FROM");
    lt_set = ensure_symbol_exists("SETQ");
    lt_throw = ensure_symbol_exists("THROW");
}
