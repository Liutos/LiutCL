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

arity_t req1;
arity_t req1opt1;
arity_t req1opt2;
arity_t req1opt4;
arity_t req1rest;
arity_t req2;
arity_t req2opt1;
arity_t req2rest;
arity_t rest;

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
#define mk_pos_arity(nreq, nopt) make_arity(nreq, nopt, FALSE, FALSE, 0, lt_nil)
#define mk_rs_arity(nreq) make_arity(nreq, 0, TRUE, FALSE, 0, lt_nil);

    req1 = mk_pos_arity(1, 0);
    req1opt1 = mk_pos_arity(1, 1);
    req1opt2 = mk_pos_arity(1, 2);
    req1opt4 = mk_pos_arity(1, 4);
    req1rest = mk_rs_arity(1);
    req2 = mk_pos_arity(2, 0);
    req2rest = mk_rs_arity(2);
    req2opt1 = mk_pos_arity(2, 1);
    rest =mk_rs_arity(0);
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
