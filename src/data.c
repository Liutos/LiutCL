/*
 * data.c
 *
 *
 *
 * Copyright (C) 2012-11-07 liutos <mat.liutos@gmail.com>
 */
#include <stdlib.h>

#include "atom.h"
#include "cons.h"
#include "environment.h"
#include "eval_sexp.h"
#include "macro_def.h"
#include "object.h"
#include "package.h"
#include "pdecls.h"
#include "stream.h"
#include "symbol.h"

#define case_type(name) case name: RETURN(gen_pkg_sym(""#name"", pkg_cl)); break

/* Cons operations */
PHEAD(lt_car)
{
    RETURN(CAR(ARG1));
}

PHEAD(lt_cdr)
{
    RETURN(CAR(ARG1));
}

PHEAD(lt_cons)
{
    RETURN(make_cons(ARG1, ARG2));
}

PHEAD(lt_list)
{
    RETURN(ARGS);
}

PHEAD(lt_rplaca)
{
    Cons cons;
    LispObject object;

    cons = ARG1;
    object = ARG2;
    _CAR(cons) = object;
    RETURN(cons);
}

PHEAD(lt_rplacd)
{
    Cons cons;
    LispObject object;

    cons = ARG1;
    object = ARG2;
    _CDR(cons) = object;
    RETURN(cons);
}

/* Package operations */
PHEAD(lt_find_package)
{
    Package pkg;

    pkg = find_package(STRING_CONTENT(ARG1));
    if (pkg != NULL)
        RETURN(pkg);
    else
        RETURN(lt_nil);
}

PHEAD(lt_package_name)
{
    Package pkg;

    pkg = ARG1;
    RETURN(make_string(PACKAGE_NAME(pkg)));
}

/* String operations */
PHEAD(lt_char)
{
    String string;
    char *content;
    int index;

    string = ARG1;
    index = theFIXNUM(ARG2);
    content = STRING_CONTENT(string);
    RETURN(TO_CHAR(content[index]));
}

/* Other operations */
PHEAD(lt_eq)
{
    RETURN(ARG1 == ARG2 ? lt_t: lt_nil);
}

PHEAD(lt_special_operator_p)
{
    RETURN(SPECIAL_P(CALL_EVAL(eval_operator, ARG1))? lt_t: lt_nil);
}

PHEAD(lt_type_of)
{
    switch (type_of(ARG1)) {
        case_type(CONS);
        case_type(FIXNUM);
        case_type(FUNCTION);
        case_type(STREAM);
        case_type(STRING);
        case_type(SYMBOL);
    default :
        error_format("Unknown data type %d. How can you define that?\n",
                     type_of(ARG1));
        exit(1);
    }
}
