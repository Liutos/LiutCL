/*
 * data.c
 *
 * Accessors, comparison and other manipulations on data structures.
 *
 * Copyright (C) 2012-11-07 liutos <mat.liutos@gmail.com>
 */
#include <stdlib.h>

#include "atom.h"
#include "cons.h"
#include "environment.h"
#include "eval_sexp.h"
#include "function.h"
#include "macro_def.h"
#include "object.h"
#include "package.h"
#include "pdecls.h"
#include "stream.h"
#include "symbol.h"

#define case_type(name) case name: RETURN(gen_pkg_sym(""#name"", pkg_cl)); break

/* Cons/List operations */
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

PHEAD(lt_consp)
{
    RETURN(CONS_P(ARG1)? lt_t: lt_nil);
}

PHEAD(lt_list)
{
    RETURN(RK);
}

PHEAD(lt_make_list)
{
    Fixnum size;
    LispObject init;
    Symbol k1;

    size = ARG1;
    k1 = gen_keyword("INITIAL-ELEMENT");
    init = get_by_key(k1, RK);
    RETURN(make_list(theFIXNUM(size), init));
}

PHEAD(lt_nth)
{
    Fixnum n;
    List list;

    n = ARG1;
    list = ARG2;
    RETURN(nth_car(theFIXNUM(n), list));
}

PHEAD(lt_nthcdr)
{
    Fixnum n;
    List list;

    n = ARG1;
    list = ARG2;
    RETURN(nth_cdr(theFIXNUM(n), list));
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

/* Function operations */
PHEAD(lt_funcall)
{
    Function fn;

    fn = ARG1;
    /* args = CDR(ARGS); */
    args->rargs = args->rargs + 1;
    args->quantity--;
    RETURN(CALL_INVOKE(invoke_function, fn, args));
}

PHEAD(lt_functionp)
{
    RETURN(FUNCTION_P(ARG1)? lt_t: lt_nil);
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
    RETURN(make_char(content[index]));
}

PHEAD(lt_make_string)
{
    Character init;
    Fixnum size;
    LispObject type;
    String string;
    Symbol k1, k2;

    size = ARG1;
    k1 = gen_keyword("INITIAL-ELEMENT");
    k2 = gen_keyword("ELEMENT-TYPE");
    init = get_by_key(k1, RK);
    type = get_by_key(k2, RK);
    string = make_string("");
    for (int n = theFIXNUM(size); n != 0; n--)
        str_add_char(string, init);
    RETURN(string);
}

PHEAD(lt_stringp)
{
    LispObject obj;

    obj = ARG1;
    RETURN(STRING_P(obj)? lt_t: lt_nil);
}

/* Symbol operations */
PHEAD(lt_intern)
{
    Package pkg;
    String str;

    str = ARG1;
    pkg = ARG2;
    RETURN(gen_pkg_sym(STRING_CONTENT(str), pkg));
}

PHEAD(lt_keywordp)
{
    LispObject object;

    object = ARG1;
    RETURN((SYMBOL_P(object) && pkg_kw == SYMBOL_PACKAGE(object))? lt_t: lt_nil);
}

PHEAD(lt_set)
{
    LispObject val;
    Symbol sym;

    sym = ARG1;
    val = ARG2;
    SYMBOL_VALUE(sym) = val;
    RETURN(val);
}

PHEAD(lt_symbol_name)
{
    Symbol sym;

    sym = ARG1;
    RETURN(make_string(SYMBOL_NAME(sym)));
}

PHEAD(lt_symbol_package)
{
    Symbol sym;

    sym = ARG1;
    RETURN(SYMBOL_PACKAGE(sym));
}

PHEAD(lt_symbol_value)
{
    Symbol sym;

    sym = ARG1;
    RETURN(SYMBOL_VALUE(sym));
}

PHEAD(lt_symbolp)
{
    Symbol sym;

    sym = ARG1;
    RETURN(SYMBOL_P(sym)? lt_t: lt_nil);
}

/* Other operations */
PHEAD(lt_eq)
{
    RETURN(eq(ARG1, ARG2)? lt_t: lt_nil);
}

PHEAD(lt_eql)
{
    LispObject x, y;

    x = ARG1;
    y = ARG2;
    /* If implements the Fixnum and Character as tagged pointer, objects
       of these two types can compare by eq. */
    if (eq(x, y))
        RETURN(lt_t);
    if (NUMBER_P(x) && NUMBER_P(y) &&
        (type_of(x) == type_of(y)) &&
        numeric_eq(x, y))
        RETURN(lt_t);
    if (CHAR_P(x) && CHAR_P(y) && eq(x, y))
        RETURN(lt_t);
    RETURN(lt_nil);
}

PHEAD(lt_special_operator_p)
{
    RETURN(SPECIAL_P(CALL_EVAL(eval_operator, ARG1))? lt_t: lt_nil);
}

PHEAD(lt_type_of)
{
    switch (type_of(ARG1)) {
        case_type(CHARACTER);
        case_type(CONS);
        case_type(FIXNUM);
        case_type(FLOAT);
        case_type(FUNCTION);
        case_type(HASH_TABLE);
        case_type(PACKAGE);
        case_type(STREAM);
        case_type(STRING);
        case_type(SYMBOL);
        case_type(VECTOR);
    default :
        error_format("Unknown data type %d. How can you define that?\n",
                     type_of(ARG1));
        longjmp(toplevel, 1);
    }
}
