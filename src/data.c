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
#include "hash_table.h"
#include "macro_def.h"
#include "number.h"
#include "object.h"
#include "package.h"
#include "pdecls.h"
#include "print_sexp.h"
#include "stream.h"
#include "symbol.h"
#include "types.h"

#define case_type(name) case name: RETURN(S(""#name"")); break

/* Comparison operations */
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
    RETURN(lt_nil);
}


/* Cons/List operations */
PHEAD(lt_car)
{
    RETURN(car(ARG1));
}

PHEAD(lt_cdr)
{
    RETURN(car(ARG1));
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

PHEAD(lt_member)
{
    LispObject item;
    List list;
    Function key, test, test_not;

    item = ARG1;
    list = ARG2;
    key = KV("KEY");
    test = KV("TEST");
    test_not = KV("TEST-NOT");
    while (CONS_P(list)) {
        LispObject obj;

        obj = CAR(list);
        if (eq(item, obj))
            RETURN(list);
        list = CDR(list);
    }
    RETURN(lt_nil);
}

/* (MAKE-LIST size &key initial-element) */
PHEAD(lt_make_list)
{
    Fixnum size;
    LispObject init;

    size = ARG1;
    /* VOI(init, "INITIAL-ELEMENT", lt_nil); */
    init = ARG2;
    RETURN(make_list_aux(theFIXNUM(size), init));
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
    /* _CAR(cons) = object; */
    set_car(cons, object);
    RETURN(cons);
}

PHEAD(lt_rplacd)
{
    Cons cons;
    LispObject object;

    cons = ARG1;
    object = ARG2;
    /* _CDR(cons) = object; */
    set_cdr(cons, object);
    RETURN(cons);
}

/* Function operations */
PHEAD(lt_apply)
{
    Function fn;
    LispObject arg;
    List argv;

    fn = ARG1;
    arg = ARG2;
    argv = RK;
    {
        Cons head, pre;

        pre = head = make_cons(arg, lt_nil);
        while (CONS_P(argv)) {
            /* _CDR(pre) = make_cons(CAR(argv), lt_nil); */
            set_cdr(pre, make_cons(CAR(argv), lt_nil));
            pre = CDR(pre);
            argv = CDR(argv);
        }
        RETURN(CALL_INVOKE(invoke_function, fn, head));
    }
}

PHEAD(lt_funcall)
{
    Function fn;

    fn = ARG1;
    args->rargs = args->rargs + 1;
    args->quantity--;
    RETURN(CALL_INVOKE(invoke_function, fn, frame2cons(args)));
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

PHEAD(lt_in_package)
{
    String name;
    Package pkg;

    name = ARG1;
    pkg = find_package(STRING_CONTENT(theSTRING(name)));
    package = pkg;
    RETURN(pkg);
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

/* (MAKE-STRING size &key initial-element element-type) */
PHEAD(lt_make_string)
{
    Character init;
    Fixnum size;
    LispObject type;
    String string;

    size = ARG1;
    VOI(init, "INITIAL-ELEMENT", make_char('\0')); /* ---TODO: Do not set initial value of keyword parameters within the function body. */
    /* if (is_unbound(type = KV("ELEMENT-TYPE"))) */
    /*     type = S("CHARACTER"); */
    VOI(type, "ELEMENT-TYPE", S("CHARACTER"));
    string = make_string("");
    for (int n = theFIXNUM(size); n != 0; n--)
        str_add_char(string, init);
    RETURN(string);
}

PHEAD(lt_stringp)
{
    LispObject obj;

    obj = ARG1;
    RETURN(B(STRING_P(obj)));
}

/* Symbol operations */
PHEAD(lt_boundp)
{
    RETURN(B(is_unbound(SYMBOL_VALUE(ARG1))));
}

PHEAD(lt_find_symbol)
{
    String string;
    Package pkg;
    Symbol symbol;

    string = ARG1;
    pkg = ARG2;
    /* if (is_unbound(pkg = ARG2)) */
    /*     pkg = package; */
    symbol = get_symbol(STRING_CONTENT(theSTRING(string)),
                        PACKAGE_HASH_TABLE(pkg));
    if (NULL == symbol)
        symbol = lt_nil;
    RETURN(make_values(2, symbol, lt_nil)); /* ---TODO: The secondary return value shouldn't always be NIL. */
}

PHEAD(lt_intern)
{
    Package pkg;
    String str;

    str = ARG1;
    pkg = ARG2;
    RETURN(gen_symbol(STRING_CONTENT(str), pkg));
}

PHEAD(lt_keywordp)
{
    LispObject object;

    object = ARG1;
    RETURN(B(SYMBOL_P(object) && is_keyword(object)));
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
    RETURN(make_string(symbol_name(sym)));
}

PHEAD(lt_symbol_package)
{
    Symbol sym;

    sym = ARG1;
    RETURN(symbol_package(sym));
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
    RETURN(B(SYMBOL_P(sym)));
}

/* Other operations */
PHEAD(lt_atom)
{
    RETURN(B(ATOM_P(ARG1)));
}

PHEAD(lt_null)
{
    RETURN(B(lt_t == ARG1));
}

PHEAD(lt_special_operator_p)
{
    RETURN(B(SPECIAL_P(get_value(ARG1, fenv))));
}

PHEAD(lt_type_of)
{
    switch (type_of(ARG1)) {
        case_type(CHARACTER);
        case_type(CONS);
        case_type(FIXNUM);
        case_type(FUNCTION);
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

Values cons2values(Cons objs)
{
    size_t count;
    values_t vals;

    count = cons_length(objs);
    vals = malloc(sizeof(struct values_t));
    vals->count = count;
    vals->objs = malloc(count * sizeof(LispObject));
    for (int i = 0; i < count; i++) {
        vals->objs[i] = CAR(objs);
        objs = CDR(objs);
    }

    return TO_VALUES(vals);
}

/* This function can implemented by VALUES-LIST. */
PHEAD(lt_values)
{
    List objs;

    objs = RK;
    RETURN(cons2values(objs));
}

PHEAD(lt_values_list)
{
    List list;

    list = ARG1;
    RETURN(cons2values(list));
}

void init_data(Environment env)
{
    /* Comparison operations */
    cfreg("EQ", lt_eq, req2);
    cfreg("EQL", lt_eql, req2);
    /* Cons operations */
    cfreg("CAR", lt_car, req1);
    cfreg("CDR", lt_cdr, req1);
    cfreg("CONS", lt_cons, req2);
    cfreg("CONSP", lt_consp, req1);
    cfreg("LIST", lt_list, rest);
    cfreg("MAKE-LIST", lt_make_list,
          new_with_kws(req1, "INITIAL-ELEMENT", NULL));
    reg_inits(lt_make_list, NULL, "(nil)");
    cfreg("MEMBER", lt_member,
          new_with_kws(req2, "KEY", "TEST", "TEST-NOT", NULL));
    reg_inits(lt_member, NULL, "((function eql) nil nil)");
    cfreg("NTH", lt_nth, req2);
    cfreg("NTHCDR", lt_nthcdr, req2);
    cfreg("RPLACA", lt_rplaca, req2);
    cfreg("REPACD", lt_rplacd, req2);
    /* Function operations */
    cfreg("APPLY", lt_apply, req1rest);
    cfreg("FUNCALL", lt_funcall, req1rest);
    cfreg("FUNCTIONP", lt_functionp, req1);
    /* Package operations */
    cfreg("FIND-PACKAGE", lt_find_package, req1);
    cfreg("IN-PACKAGE", lt_in_package, req1);
    cfreg("PACKAGE-NAME", lt_package_name, req1);
    /* String operations */
    cfreg("CHAR", lt_char, req2);
    cfreg("MAKE-STRING", lt_make_string,
          new_with_kws(req1, "INITIAL-ELEMENT", "ELEMENT-TYPE", NULL)); /* ---TODO: The application of `reg_inits' for function `lt_make_string'. */
    cfreg("STRINGP", lt_stringp, req1);
    /* Symbol operations */
    cfreg("FIND-SYMBOL", lt_find_symbol, req1opt1);
    reg_inits(lt_find_symbol, "(*package*)", NULL);
    cfreg("INTERN", lt_intern, req1opt1);
    reg_inits(lt_intern, "(*package*)", NULL);
    cfreg("KEYWORDP", lt_keywordp, req1);
    cfreg("SET", lt_set, req2);
    cfreg("SYMBOL-NAME", lt_symbol_name, req1);
    cfreg("SYMBOL-PACKAGE", lt_symbol_package, req1);
    cfreg("SYMBOL-VALUE", lt_symbol_value, req1);
    cfreg("SYMBOLP", lt_symbolp, req1);
    /* Other operations */
    cfreg("ATOM", lt_atom, req1);
    cfreg("NULL", lt_null, req1);
    cfreg("SPECIAL-OPERATOR-P", lt_special_operator_p, req1);
    cfreg("TYPE-OF", lt_type_of, req1);
    cfreg("VALUES", lt_values, rest);
    cfreg("VALUES-LIST", lt_values_list, req1);
}
