/*
 * init.c
 *
 *
 *
 * Copyright (C) 2012-11-04 liutos <mat.liutos@gmail.com>
 */
#include "environment.h"
#include "function.h"
#include "hash_table.h"
#include "package.h"
#include "stream.h"
#include "symbol.h"
#include "types.h"

/* Primitives implementation */
#include "arit.h"
#include "data.h"
#include "io.h"
#include "logic.h"
#include "spec.h"

#define freg(name, pkg, prim, arity) reg(name, pkg, prim, arity, REGULAR)
#define reg(name, pkg, prim, arity, type)                \
    env = reg_primitive(name, pkg, prim, arity, type, env)
#define sreg(name, pkg, prim, arity) reg(name, pkg, prim, arity, SPECIAL)

Environment init_cvars(Environment lenv)
{
    Environment tmp;

    tmp = lenv;
    lt_nil = gen_pkg_sym("NIL", pkg_cl);
    tmp = extend_env(lt_nil, lt_nil, tmp);
    lt_t = gen_pkg_sym("T", pkg_cl);
    tmp = extend_env(lt_t, lt_t, tmp);

    return tmp;
}

Environment init_dvars(Environment denv)
{
    Environment tmp;

    tmp = denv;
    tmp = extend_env_by_name("*PACKAGE*", pkg_cl, pkg_cl, tmp);
    standard_output = make_file_stream(stdout);
    tmp = extend_env_by_name("*STANDARD-OUTPUT*", pkg_cl, standard_output, tmp);
    standard_input = make_file_stream(stdin);
    tmp = extend_env_by_name("*STANDARD-INPUT*", pkg_cl, standard_input, tmp);
    standard_error = make_file_stream(stderr);
    tmp = extend_env_by_name("*STANDARD-ERROR*", pkg_cl, standard_error, tmp);

    return tmp;
}

void init_packages(void)
{
    packages = make_hash_table_aux(5, hash_string, string_cmp);
    pkg_cl = make_package("COMMON-LISP");
    pkg_lt = make_package("LIUTOS-LISP");
}

Environment reg_primitive(char *fn_name,
                          Package pkg,
                          primitive_t prim,
                          Arity arity,
                          FunctionType type,
                          Environment env)
{
    Function fn;

    fn = make_C_function(prim, arity, type);

    return extend_env(gen_pkg_sym(fn_name, pkg), fn, env);
}

Environment init_primitives(Environment env)
{
    Arity req1 = make_arity_t(1, 0, FALSE, FALSE, 0, lt_nil);
    Arity req1opt2 = make_arity_t(1, 2, FALSE, FALSE, 0, lt_nil);
    Arity req1opt4 = make_arity_t(1, 4, FALSE, FALSE, 0, lt_nil);
    Arity req1rest = make_arity_t(1, 0, TRUE, FALSE, 0, lt_nil);
    Arity req2 = make_arity_t(2, 0, FALSE, FALSE, 0, lt_nil);
    Arity req2opt1 = make_arity_t(2, 1, FALSE, FALSE, 0, lt_nil);
    Arity rest = make_arity_t(0, 0, TRUE, FALSE, 0, lt_nil);
    /* Register regular functions */
    freg("ADD", pkg_cl, add, req2);
    freg("MUL", pkg_cl, mul, req2);
    freg("GT", pkg_cl, gt, req2);
    freg("AND2", pkg_cl, and2, req2);
    freg("SUB", pkg_cl, sub, req2);
    freg("DIV", pkg_cl, div, req2);
    freg("OR2", pkg_cl, or2, req2);
    freg("FIXNUM-EQ", pkg_cl, fixnum_eq, req2);
    freg("EQ", pkg_cl, lt_eq, req2);
    freg("SPECIAL-OPERATOR-P", pkg_cl, lt_special_operator_p, req1);
    freg("TYPE-OF", pkg_cl, lt_type_of, req1);
    freg("WRITE-A-CHAR", pkg_cl, lt_write_a_char, req1);
    /* Cons operations */
    freg("CAR", pkg_cl, lt_car, req1);
    freg("CDR", pkg_cl, lt_cdr, req1);
    freg("CONS", pkg_cl, lt_cons, req2);
    freg("LIST", pkg_cl, lt_list, rest);
    freg("RPLACA", pkg_cl, lt_rplaca, req2);
    freg("REPACD", pkg_cl, lt_rplacd, req2);
    /* IO operations */
    freg("READ-CHAR", pkg_cl, lt_read_char, req1opt4);
    /* Package operations */
    freg("FIND-PACKAGE", pkg_cl, lt_find_package, req1);
    freg("PACKAGE-NAME", pkg_cl, lt_package_name, req1);
    /* String operations */
    freg("CHAR", pkg_cl, lt_char, req2);
    /* Register special operators */
    sreg("BLOCK", pkg_cl, lt_block, req1rest);
    sreg("CATCH", pkg_cl, lt_catch, req1rest);
    sreg("DEFVAR", pkg_cl, lt_defvar, req1opt2);
    sreg("FSET", pkg_cl, lt_fset, req2);
    sreg("FUNCTION", pkg_cl, lt_function, req1);
    sreg("GO", pkg_cl, lt_go, req1);
    sreg("IF", pkg_cl, lt_if, req2opt1);
    sreg("LAMBDA", pkg_cl, lt_lambda, req1rest);
    sreg("MK-MACRO", pkg_cl, lt_mk_macro, req1rest);
    sreg("PROGN", pkg_cl, lt_progn, rest);
    sreg("QUOTE", pkg_cl, lt_quote, req1);
    sreg("RETURN-FROM", pkg_cl, lt_return_from, req2);
    sreg("SETQ", pkg_cl, lt_setq, rest);
    sreg("TAGBODY", pkg_cl, lt_tagbody, rest);
    sreg("THROW", pkg_cl, lt_throw, req2);

    return env;
}
