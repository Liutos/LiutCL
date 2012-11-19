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
#include "parse_sexp.h"
#include "print_sexp.h"
#include "stream.h"
#include "symbol.h"
#include "types.h"

hash_table_t init_exprs;

arity_t make_arity
(int req_cnt,
 int opt_cnt,
 BOOL rest_flag,
 BOOL key_flag,
 int key_cnt,
 List keywords)
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

arity_t parse_arity(List parms)
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

/* Constructor */
Function make_C_function(primitive_t C_fn, arity_t arity, FunctionType type)
{
    function_t fn;

    fn = malloc(sizeof(struct function_t));
    fn->is_C_function = TRUE;
    fn->type = type;
    fn->u.fptr = C_fn;
    fn->arity = arity;

    return TO_FUNCTION(fn);
}

Function make_Lisp_function(List parms, List expr, Environment lenv, Environment denv, Environment fenv, BlockEnvironment benv, GoEnvironment genv)
{
    function_t fn;

    fn = malloc(sizeof(function_t));
    fn->arity = parse_arity(parms);
    fn->u.s.block_env = benv;
    fn->u.s.fdefinition_env = fenv;
    fn->is_C_function = FALSE;
    fn->type = REGULAR;
    fn->u.s.go_env = genv;
    fn->u.s.body = expr;
    fn->u.s.lexical_env = lenv;
    fn->u.s.parameters = parms;

    return TO_FUNCTION(fn);
}

/* Generates a a-list, keyword as key and LispObject as value, for storing the mapping between keyword parameter name and their values. */
List mk_kws(List keywords)
{
    Cons cur, head, pre;

    pre = head = make_cons(lt_nil, lt_nil);
    while (CONS_P(keywords)) {
        cur = make_cons(CAR(keywords), lt_nil);
        cur = make_cons(cur, lt_nil);
        /* _CDR(pre) = cur; */
        set_cdr(pre, cur);
        pre = cur;
        keywords = CDR(keywords);
    }

    return CDR(head);
}

Frame cons2frame(Cons args, arity_t arity)
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
    frame->rest = args;
    frame->kws = mk_kws(arity->keywords);
    {
        List tmp, kws;

        tmp = args;
        kws = frame->kws;
        while (CONS_P(kws)) {
            Cons kv;
            LispObject val;

            kv = CAR(kws);
            val = get_by_key(CAR(kv), tmp);
            set_cdr(kv, val);
            kws = CDR(kws);
        }
    }

    return frame;
}

void describe_frame(Frame frame)
{
    List kws;

    printf("The `kws' member in frame %p is\n", frame);
    kws = frame->kws;
    while (CONS_P(kws)) {
        Cons kv;

        kv = CAR(kws);
        write_format(standard_output, "%!\t\t", CAR(kv));
        if (!is_unbound(CDR(kv)))
            write_format(standard_output, "%!\n", CDR(kv));
        else
            write_format(standard_output, "unbound\n");
        kws = CDR(kws);
    }
}

List make_keywords_aux(va_list ap)
{
    Cons cur, head, pre;
    char *name;

    name = va_arg(ap, char *);
    pre = head = make_cons(lt_nil, lt_nil);
    while (name != NULL) {
        cur = make_cons(gen_symbol(name, pkg_kw), lt_nil);
        /* _CDR(pre) = cur; */
        set_cdr(pre, cur);
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

arity_t make_arity_kw(int req_cnt, int opt_cnt, BOOL rest_flag, ...)
{
    List kws;
    va_list ap;

    va_start(ap, rest_flag);
    kws = make_keywords_aux(ap);

    return make_arity(req_cnt, opt_cnt, rest_flag, TRUE, cons_length(kws), kws);
}

arity_t new_with_kws(arity_t tmpl, ...)
{
    arity_t new;
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
        /* _CDR(pre) = cur; */
        set_cdr(pre, cur);
        pre = cur;
    }
    /* _CDR(pre) = frame->rest; */
    set_cdr(pre, frame->rest);

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

Cons fn_init_exprs(Function fn)
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
    arity_t arity;
    int nreq, nopt, nkey;
    int i, j;
    List inits, opt_inits, key_inits;

    arity = ARITY(fn);
    nreq = arity->req_count;
    nopt = arity->opt_count;
    nkey = arity->key_count;
    inits = fn_init_exprs(fn);
    /* The result of `fn_init_exprs' is a Cons of optional and keyword initial values. */
    opt_inits = CAR(inits);
    key_inits = CDR(inits);
    /* Fill the optional parameters. */
    if (nopt != 0 && !eq(lt_nil, opt_inits)) {
        i = nreq;
        for (j = 0; j < nopt; j++) {
            frame->rargs[i + j] = CALL_EVAL(eval_sexp, CAR(opt_inits));
            opt_inits = CDR(opt_inits);
        }
    }
    /* Fill the keyword parameters. */
    if (nkey != 0 && !eq(lt_nil, key_inits)) {
        List tmp;

        tmp = frame->kws;
        while (CONS_P(tmp)) {
            Cons kv;

            kv = CAR(tmp);
            if (is_unbound(CDR(kv)))
                /* _CDR(kv) = CALL_EVAL(eval_sexp, CAR(key_inits)); */
                set_cdr(kv, CALL_EVAL(eval_sexp, CAR(key_inits)));
            tmp = CDR(tmp);
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
                          arity_t arity,
                          FunctionType type,
                          Environment fenv)
{
    Function fn;

    fn = make_C_function(prim, arity, type);

    return extend_env(gen_symbol(fn_name, pkg), fn, fenv);
}

void reg_inits(primitive_t fn, char *opts, char *keys)
{
    List opt_inits, key_inits;

    if (opts != NULL)
        opt_inits = parse_input(opts);
    else
        opt_inits = lt_nil;
    if (keys != NULL)
        key_inits = parse_input(keys);
    else
        key_inits = lt_nil;
    add_key_value(fn, make_cons(opt_inits, key_inits), init_exprs);
}
