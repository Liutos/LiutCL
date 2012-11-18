/*
 * function.c
 *
 * Creation and application of Lisp function and its relative stuff.
 *
 * Copyright (C) 2012-11-06 liutos <mat.liutos@gmail.com>
 */
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#include "atom.h"
#include "cons.h"
#include "environment.h"
#include "eval_sexp.h"
#include "hash_table.h"
#include "macro_def.h"
#include "print_sexp.h"
#include "stream.h"
#include "symbol.h"
#include "types.h"

Arity req1;
Arity req1opt1;
Arity req1opt2;
Arity req1opt4;
Arity req1rest;
Arity req2;
Arity req2opt1;
Arity rest;
Arity make_list_a;
Arity make_string_a;

hash_table_t init_exprs;

/* cons2frame所生成的Frame中的数组长度与`arity'中的req_count与opt_count的和一样长，
   并且可选参数的值如果没有实参对应，那么均为lt_nil。 */
Frame cons2frame(Cons args, Arity arity)
{
    size_t quantity;
    Frame frame;
    int i;

    quantity = arity->req_count + arity->opt_count;
    frame = malloc(sizeof(struct frame_t));
    frame->quantity = quantity;
    frame->rargs = malloc(quantity * sizeof(LispObject));
    for (i = 0; CONS_P(args) && i <quantity; i++) {
        frame->rargs[i] = CAR(args);
        args = CDR(args);
    }
    for (; i < quantity; i++)
        frame->rargs[i] = gunbound; /* 设置为NULL表示这些值没有绑定 */
    frame->rest_or_kws = args;

    return frame;
}

Function make_function_aux(void)
{
    return TO_FUNCTION(malloc(sizeof(struct function_t)));
}

Function make_C_function(primitive_t prim, Arity arity, FunctionType type)
{
    Function fn = make_function_aux();

    FUNCTION_CFLAG(fn) = TRUE;
    FTYPE(fn) = type;
    PRIMITIVE(fn) = prim;
    ARITY(fn) = arity;

    return fn;
}

Arity make_arity(req_cnt, opt_cnt, rest_flag, key_flag, key_cnt, keywords)
     int req_cnt;
     int opt_cnt;
     BOOL rest_flag;
     BOOL key_flag;
     int key_cnt;
     List keywords;
{
    arity_t arity;

    arity = malloc(sizeof(struct arity_t));
    arity->req_count = req_cnt;
    arity->opt_count = opt_cnt;
    arity->rest_flag = rest_flag;
    arity->key_flag = key_flag;
    arity->key_count = key_cnt;
    arity->keywords = keywords;

    return arity;
}

List make_keywords_aux(va_list ap)
{
    Cons cur, head, pre;
    char *name;

    name = va_arg(ap, char *);
    pre = head = make_cons(lt_nil, lt_nil);
    while (name != NULL) {
        cur = make_cons(gen_symbol(name, pkg_kw), lt_nil);
        _CDR(pre) = cur;
        pre = cur;
        name = va_arg(ap, char *);
    }
    va_end(ap);

    return CDR(head);
}

List make_keywords(int _, ...)
{
    va_list ap;

    va_start(ap, _);

    return make_keywords_aux(ap);
}

Arity make_arity_kw(int req_cnt, int opt_cnt, BOOL rest_flag, ...)
{
    List kws;
    va_list ap;

    va_start(ap, rest_flag);
    kws = make_keywords_aux(ap);

    return make_arity(req_cnt, opt_cnt, rest_flag, TRUE, cons_length(kws), kws);
}

Arity new_with_kws(Arity tmpl, ...)
{
    Arity new;
    va_list ap;
    List kws;

    new = malloc(sizeof(struct arity_t));
    memcpy(new, tmpl, sizeof(struct arity_t));
    va_start(ap, tmpl);
    kws = make_keywords_aux(ap);
    new->key_flag = TRUE;
    new->key_count = cons_length(kws);
    new->keywords = kws;

    return new;
}

Arity parse_arity(List parms)
{
    BOOL rest_flag;
    Symbol key, opt, rest;
    int key_count, opt_count, req_count;

    key = S("&key");
    opt = S("&optional");
    rest = S("&rest");
    key_count = opt_count = req_count = 0;
    /* Required parameters */
    while (CONS_P(parms)) {
        if (eq(key, CAR(parms)) || eq(opt, CAR(parms)) || eq(rest, CAR(parms)))
            break;
        req_count++;
        parms = CDR(parms);
    }
    /* Optional parameters */
    if (eq(opt, CAR(parms))) {
        parms = CDR(parms);
        while (CONS_P(parms)) {
            if (eq(key, CAR(parms)) || eq(rest, CAR(parms)))
                break;
            opt_count++;
            parms = CDR(parms);
        }
    }
    /* Keyword parameters */
    if (eq(key, CAR(parms))) {
        parms = CDR(parms);
        while (CONS_P(parms)) {
            if (eq(opt, CAR(parms))) {
                write_format(standard_error, "Lambda list marker &OPTIONAL not allowed here.\n");
                exit(1);
            }
            if (eq(rest, CAR(parms)))
                break;
            key_count++;
            parms = CDR(parms);
        }
    }
    /* Rest parameters */
    if (eq(rest, CAR(parms))) {
        parms = CDR(parms);
        if (CONS_P(parms))
            rest_flag = TRUE;
        parms = CDR(parms);
        if (eq(key, CAR(parms)) || eq(opt, CAR(parms))) {
            write_format(standard_error, "Lambda list marker $! not allowed here.\n");
            exit(1);
        } else if (parms != lt_nil) {
            write_format(standard_error, "Lambda list element $! is superfluous. Only one var...\n", CAR(parms));
            exit(1);
        }
    }

    return make_arity(req_count, opt_count, rest_flag, key_count != 0, key_count, lt_nil);
}

DEFMK(make_Lisp_function)
{
    Function fn;

    fn = make_function_aux();
    ARITY(fn) = parse_arity(parms);
    BLOCK_ENV(fn) = benv;
    FDEFINITION_ENV(fn) = fenv;
    FUNCTION_CFLAG(fn) = FALSE;
    FTYPE(fn) = REGULAR;
    GO_ENV(fn) = genv;
    EXPRESSION(fn) = expr;
    LEXICAL_ENV(fn) = lenv;
    PARAMETERS(fn) = parms;

    return fn;
}

DEFMK(make_Lisp_macro)
{
    Function fn;

    fn = CALL_MK(make_Lisp_function, parms, expr);
    FTYPE(fn) = MACRO;

    return fn;
}

DEFINVOKE(invoke_C_function, C_fn, Frame)
{
    return PRIMITIVE(C_fn)(args, lenv, denv, fenv, benv, genv);
}

List frame2cons(Frame frame)
{
    Cons cur, head, pre;

    pre = head = make_cons(lt_nil, lt_nil);
    for (int i = 0; i < frame->quantity; i++) {
        cur = make_cons(frame->rargs[i], lt_nil);
        _CDR(pre) = cur;
        pre = cur;
    }
    _CDR(pre) = frame->rest_or_kws;

    return CDR(head);
}

LispObject invoke_Lisp_function(Function Lisp_function, Cons args, Environment denv)
{
    BlockEnvironment benv;
    Environment fenv;
    Environment lenv;
    GoEnvironment genv;

    benv = BLOCK_ENV(Lisp_function);
    fenv = FDEFINITION_ENV(Lisp_function);
    lenv = make_new_env(PARAMETERS(Lisp_function),
                        args,
                        LEXICAL_ENV(Lisp_function));
    genv = GO_ENV(Lisp_function);
    
    return CALL_EVAL(eprogn, EXPRESSION(Lisp_function));
}

List fn_init_exprs(Function fn)
{
    List exprs;

    exprs = search_key(PRIMITIVE(fn), init_exprs);
    if (NULL == exprs)
        return lt_nil;
    else
        return exprs;
}

Frame fill_frame(Frame frame, Function fn, Environment lenv, Environment denv, Environment fenv, BlockEnvironment benv, GoEnvironment genv)
{
    Arity arity;
    int nreq, nopt;
    int i, j;
    List init_exprs;

    arity = ARITY(fn);
    nreq = arity->req_count;
    nopt = arity->opt_count;
    init_exprs = fn_init_exprs(fn);
    if (eq(lt_nil, init_exprs))
        return frame;
    if (nopt != 0) {
        i = nreq;
        for (j = 0; j < nopt; j++) {
            frame->rargs[i + j] = CALL_EVAL(eval_sexp, CAR(init_exprs));
            init_exprs = CDR(init_exprs);
        }
    }

    return frame;
}

DEFINVOKE(invoke_function, function, Cons)
{
    if (TRUE == FUNCTION_CFLAG(function)) {
        Frame frame;

        frame = cons2frame(args, ARITY(function));
        frame = fill_frame(frame, function, lenv, denv, fenv, benv, genv);

	return CALL_INVOKE(invoke_C_function, function, frame);
    } else if (REGULAR == FTYPE(function))
	return invoke_Lisp_function(function, args, denv);
    else {
        LispObject form;

        form = invoke_Lisp_function(function, args, denv);

        return CALL_EVAL(eval_sexp, form);
    }
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

    return extend_env(gen_symbol(fn_name, pkg), fn, env);
}
