/*
 * spec.c
 *
 *
 *
 * Copyright (C) 2012-11-05 liutos <mat.liutos@gmail.com>
 */
#include "../atom_proc.h"
#include "../cons.h"
#include "../eval_sexp.h"
#include "../types.h"

#include "pdecls.h"

#define CALL_EVAL(eval_fn, arg) eval_fn(arg, lenv, denv, benv, fenv)

PHEAD(lt_if)
{
    if (lt_nil != CALL_EVAL(eval_sexp, FIRST(args)))
        return CALL_EVAL(eval_sexp, SECOND(args));
    else
        return CALL_EVAL(eval_sexp, THIRD(args));
}

PHEAD(lt_lambda)
{
    return make_Lisp_function(CAR(args), CDR(args), lenv, denv, benv, fenv);
}

PHEAD(lt_quote)
{
    return FIRST(args);
}
