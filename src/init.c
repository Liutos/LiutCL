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
#include "parse_sexp.h"
#include "stream.h"
#include "symbol.h"
#include "types.h"

/* Primitives implementation */
#include "arit.h"
#include "data.h"
#include "io.h"
#include "logic.h"
#include "spec.h"

#define cfreg(name, prim, arity) freg(name, pkg_cl, prim, arity)
#define csreg(name, prim, arity) sreg(name, pkg_cl, prim, arity)
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
    pkg_kw = make_package("KEYWORD");
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
    Arity req1opt1 = make_arity_t(1, 1, FALSE, FALSE, 0, lt_nil);
    Arity req1opt2 = make_arity_t(1, 2, FALSE, FALSE, 0, lt_nil);
    Arity req1opt4 = make_arity_t(1, 4, FALSE, FALSE, 0, lt_nil);
    Arity req1rest = make_arity_t(1, 0, TRUE, FALSE, 0, lt_nil);
    Arity req2 = make_arity_t(2, 0, FALSE, FALSE, 0, lt_nil);
    Arity req2opt1 = make_arity_t(2, 1, FALSE, FALSE, 0, lt_nil);
    Arity rest = make_arity_t(0, 0, TRUE, FALSE, 0, lt_nil);
    /* Arity structure with keywords appear only once. */
    /* For function MAKE-LIST. */
    Arity make_list_a = make_arity_kw(1, 0, FALSE, "INITIAL-ELEMENT", NULL);
    /* For function MAKE-STRING */
    Arity make_string_a = make_arity_kw(1, 0, FALSE, "INITIAL-ELEMENT", "ELEMENT-TYPE", NULL);

    /* Register regular functions */
    freg("ADD", pkg_cl, add, req2);
    freg("MUL", pkg_cl, mul, req2);
    freg("GT", pkg_cl, gt, req2);
    freg("AND2", pkg_cl, and2, req2);
    freg("SUB", pkg_cl, sub, req2);
    freg("DIV", pkg_cl, div, req2);
    freg("OR2", pkg_cl, or2, req2);
    freg("FIXNUM-EQ", pkg_cl, fixnum_eq, req2);
    cfreg("SPECIAL-OPERATOR-P", lt_special_operator_p, req1);
    cfreg("TYPE-OF", lt_type_of, req1);
    freg("WRITE-A-CHAR", pkg_cl, lt_write_a_char, req1);
    cfreg("VALUES", lt_values, rest);
    /* Comparison operations */
    cfreg("EQ", lt_eq, req2);
    freg("EQL", pkg_cl, lt_eql, req2);
    /* Cons operations */
    cfreg("CAR", lt_car, req1);
    cfreg("CDR", lt_cdr, req1);
    cfreg("CONS", lt_cons, req2);
    cfreg("CONSP", lt_consp, req1);
    cfreg("LIST", lt_list, rest);
    cfreg("MAKE-LIST", lt_make_list, make_list_a);
    cfreg("NTH", lt_nth, req2);
    cfreg("NTHCDR", lt_nthcdr, req2);
    cfreg("RPLACA", lt_rplaca, req2);
    cfreg("REPACD", lt_rplacd, req2);
    /* Function operations */
    cfreg("FUNCALL", lt_funcall, req1rest);
    cfreg("FUNCTIONP", lt_functionp, req1);
    /* IO operations */
    cfreg("READ-CHAR", lt_read_char, req1opt4);
    cfreg("READ-LINE", lt_read_line, req1opt4);
    /* Package operations */
    cfreg("FIND-PACKAGE", lt_find_package, req1);
    cfreg("PACKAGE-NAME", lt_package_name, req1);
    /* String operations */
    cfreg("CHAR", lt_char, req2);
    cfreg("MAKE-STRING", lt_make_string, make_string_a);
    cfreg("STRINGP", lt_stringp, req1);
    /* Symbol operations */
    cfreg("INTERN", lt_intern, req1opt1);
    cfreg("KEYWORDP", lt_keywordp, req1);
    cfreg("SET", lt_set, req2);
    cfreg("SYMBOL-NAME", lt_symbol_name, req1);
    cfreg("SYMBOL-PACKAGE", lt_symbol_package, req1);
    cfreg("SYMBOL-VALUE", lt_symbol_value, req1);
    cfreg("SYMBOLP", lt_symbolp, req1);
    /* Register special operators */
    csreg("BLOCK", lt_block, req1rest);
    csreg("CATCH", lt_catch, req1rest);
    csreg("DEFVAR", lt_defvar, req1opt2);
    sreg("FSET", pkg_lt, lt_fset, req2);
    csreg("FUNCTION", lt_function, req1);
    csreg("GO", lt_go, req1);
    csreg("IF", lt_if, req2opt1);
    csreg("LAMBDA", lt_lambda, req1rest);
    csreg("MK-MACRO", lt_mk_macro, req1rest);
    csreg("PROGN", lt_progn, rest);
    csreg("QUOTE", lt_quote, req1);
    csreg("RETURN-FROM", lt_return_from, req2);
    csreg("SETQ", lt_setq, rest);
    csreg("TAGBODY", lt_tagbody, rest);
    csreg("THROW", lt_throw, req2);

    return env;
}
