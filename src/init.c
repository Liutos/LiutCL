/*
 * init.c
 *
 * Initialization of packages, environment and primitives.
 *
 * Copyright (C) 2012-11-04 liutos <mat.liutos@gmail.com>
 */
#include "environment.h"
#include "function.h"
#include "hash_table.h"
#include "macro_def.h"
#include "package.h"
#include "parse_sexp.h"
#include "pdecls.h"
#include "stream.h"
#include "symbol.h"
#include "types.h"

#define INIT(fn) do {extern void init_##fn(Environment); init_##fn(env);} while(0)
#define RI(fn, exprs)

Environment init_cvars(Environment lenv)
{
    Environment tmp;

    tmp = lenv;
    lt_nil = S("NIL");
    tmp = extend_env(lt_nil, lt_nil, tmp);
    lt_t = S("T");
    tmp = extend_env(lt_t, lt_t, tmp);

    return tmp;
}

Environment init_dvars(Environment denv)
{
    Environment tmp;

    tmp = denv;
    tmp = extend_env_by_name("*PACKAGE*", pkg_cl, pkg_cl, tmp);
    standard_output = make_file_stream(stdout, WRITE);
    tmp = extend_env_by_name("*STANDARD-OUTPUT*", pkg_cl, standard_output, tmp);
    standard_input = make_file_stream(stdin, READ);
    tmp = extend_env_by_name("*STANDARD-INPUT*", pkg_cl, standard_input, tmp);
    standard_error = make_file_stream(stderr, WRITE);
    tmp = extend_env_by_name("*STANDARD-ERROR*", pkg_cl, standard_error, tmp);

    return tmp;
}

void init_packages(void)
{
    packages = make_hash_table_t(5, hash_string, string_cmp);
    pkg_cl = make_package("COMMON-LISP");
    pkg_kw = make_package("KEYWORD");
    pkg_lt = make_package("LIUTOS-LISP");
    package = pkg_cl;
}

void init_arity(void)
{
    req1 = make_arity(1, 0, FALSE, FALSE, 0, lt_nil);
    req1opt1 = make_arity(1, 1, FALSE, FALSE, 0, lt_nil);
    req1opt2 = make_arity(1, 2, FALSE, FALSE, 0, lt_nil);
    req1opt4 = make_arity(1, 4, FALSE, FALSE, 0, lt_nil);
    req1rest = make_arity(1, 0, TRUE, FALSE, 0, lt_nil);
    req2 = make_arity(2, 0, FALSE, FALSE, 0, lt_nil);
    req2opt1 = make_arity(2, 1, FALSE, FALSE, 0, lt_nil);
    rest = make_arity(0, 0, TRUE, FALSE, 0, lt_nil);
}

Environment init_primitives(Environment env)
{
    init_arity();

    INIT(arit);
    INIT(data);
    INIT(io);
    INIT(logic);
    INIT(spec);

    return env;
}

void init_init_exprs(void)
{
    init_exprs = make_hash_table_t(47, hash_ptr, ptr_cmp);
    PHEAD(lt_find_symbol);
    add_key_value(lt_find_symbol, parse_input("(*package*)"), init_exprs);
}
