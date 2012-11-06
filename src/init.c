/*
 * init.c
 *
 *
 *
 * Copyright (C) 2012-11-04 liutos <mat.liutos@gmail.com>
 */
#include "atom_proc.h"
#include "environment.h"
#include "function.h"
#include "stream.h"
#include "symbol_table.h"
#include "types.h"

#include "primitive/arit.h"
#include "primitive/cons_proc.h"
#include "primitive/io.h"
#include "primitive/logic.h"
#include "primitive/object_proc.h"

#ifdef FS
#include "primitive/spec.h"
#endif

Environment reg_primitive(char *fn_name, primitive_t prim, Arity arity, FunctionType type, Environment env)
{
    Function fn;

    fn = make_C_function(prim, arity, type);

    return extend_env(ensure_symbol_exists(fn_name), fn, env);
}

Environment init_primitives(Environment env)
{
#define reg(name, prim, arity, type) env = reg_primitive(name, prim, arity, type, env)
#define freg(name, prim, arity) reg(name, prim, arity, REGULAR)

    Arity req1 = make_arity_t(1, 0, FALSE, FALSE, 0, lt_nil);
    Arity req2 = make_arity_t(2, 0, FALSE, FALSE, 0, lt_nil);
    /* Register regular functions */
    freg("ADD", add, req2);
    freg("MUL", mul, req2);
    freg("GT", gt, req2);
    freg("AND2", and_two, req2);
    freg("SUB", sub, req2);
    freg("DIV", div, req2);
    freg("OR2", or_two, req2);
    freg("CAR", lt_car, req1);
    freg("CDR", lt_cdr, req1);
    freg("FIXNUM-EQ", fixnum_eq, req2);
    freg("EQ", lt_eq, req2);
    freg("CONS", lt_cons, req2);
    freg("TYPE-OF", lt_type_of, req1);
    freg("LT/READ-A-CHAR", lt_read_a_char, req1);
    freg("LT/WRITE-A-CHAR", lt_write_a_char, req1);
#ifdef FS
#define sreg(name, prim, arity) reg(name, prim, arity, SPECIAL)

    sreg("IF", lt_if, 3);
    sreg("LAMBDA", lt_lambda, 2);
    sreg("QUOTE", lt_quote, 1);
#endif

    return env;
}

Environment init_dvars(Environment denv)
{
    Environment tmp = denv;

    standard_output = make_file_stream(stdout);
    tmp = extend_env_by_name("*STANDARD-OUTPUT*", standard_output, tmp);
    standard_input = make_file_stream(stdin);
    tmp = extend_env_by_name("*STANDARD-INPUT*", standard_input, tmp);
    standard_error = make_file_stream(stderr);
    tmp = extend_env_by_name("*STANDARD-ERROR*", standard_error, tmp);

    return tmp;
}

Environment init_variables(Environment lenv)
{
    Environment tmp = lenv;

    lt_nil = ensure_symbol_exists("NIL");
    tmp = extend_env_by_name("NIL", lt_nil, tmp);
    lt_t = ensure_symbol_exists("T");
    tmp = extend_env_by_name("T", lt_t, tmp);

    return tmp;
}

void init_special_operators(void)
{
    lt_block = ensure_symbol_exists("BLOCK");
    lt_catch = ensure_symbol_exists("CATCH");
    lt_defvar = ensure_symbol_exists("DEFVAR");
    lt_fset = ensure_symbol_exists("LT/FSET!");
    lt_function = ensure_symbol_exists("FUNCTION");
    lt_progn = ensure_symbol_exists("PROGN");
#ifndef FS
    lt_if = ensure_symbol_exists("IF");
    lt_lambda = ensure_symbol_exists("LAMBDA");
    lt_quote = ensure_symbol_exists("QUOTE");
#endif
    lt_return_from = ensure_symbol_exists("RETURN-FROM");
    lt_setq = ensure_symbol_exists("SETQ");
    lt_throw = ensure_symbol_exists("THROW");
}
