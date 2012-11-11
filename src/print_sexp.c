/*
 * print_sexp.c
 *
 * 
 *
 * Copyright (C) 2012-10-03 liutos
 */
#include <stdio.h>
#include <stdlib.h>

#include "atom.h"
#include "cons.h"
#include "object.h"
#include "stream.h"
#include "symbol.h"
#include "types.h"

void print_object_notln(LispObject, Stream);
void print_object(LispObject, Stream);

void print_symbol(Symbol sym, Stream output)
{
    if (is_keyword(sym))
        write_format(output, ":%s", make_string(SYMBOL_NAME(sym)));
    else
        write_string(output, make_string(SYMBOL_NAME(sym)));
}

void print_atom(Atom atom, Stream output)
{
    switch (type_of(atom)) {
    case CHARACTER:
        write_format(output, "#\\%c", atom);
        break;
    case FIXNUM:
        write_fixnum(output, atom);
	break;
    case FLOAT:
        write_float(output, atom);
        break;
    case FUNCTION:
	if (TRUE == FUNCTION_CFLAG(atom))
            write_format(output, "#<FUNCTION C %p>", atom);
	else
            write_format(output, "#<FUNCTION Lisp %! %! %p>",
                         PARAMETERS(atom),
                         EXPRESSION(atom),
                         atom);
	break;
    case PACKAGE:
        write_format(output, "#<PACKAGE %!>", make_string(PACKAGE_NAME(atom)));
        break;
    case STREAM:
        write_format(output, "#<STREAM %p>", atom);
        break;
    case STRING:
        write_format(output, "\"%s\"", atom);
        break;
    case SYMBOL:
        print_symbol(atom, output);
	break;
    default :
        write_format(output, "Unknown type %d\n", TO_FIXNUM(type_of(atom)));
	exit(1);
    }
}

void print_cons_core(LispObject cons, Stream output)
{
    while (CONS_P(cons)) {
	if (CONS_P(CAR(cons)))
            print_object_notln(CAR(cons), output);
        else
            print_atom(CAR(cons), output);
	cons = CDR(cons);
	if (!CONS_P(cons)) {
	    if (cons != lt_nil) {
		write_string(output, make_string(" . "));
		print_atom(cons, output);
	    }
	} else
            write_char(output, TO_CHAR(' '));
    }
}

void print_cons(LispObject cons, Stream output)
{
    write_char(output, TO_CHAR('('));
    print_cons_core(cons, output);
    write_char(output, TO_CHAR(')'));
}

void print_object_notln(LispObject object, Stream output)
{
    if (CONS_P(object))
	print_cons(object, output);
    else
	print_atom(object, output);
}

void print_values(values_t vals, Stream output)
{
    for (int i = 0; i < vals->count; i++)
        print_object(vals->objs[i], output);
}

/* Print the string representation of `object' to `output'. */
void print_object(LispObject object, Stream output)
{
    if (NULL == object) {
        write_string(output, TO_STRING("; No value\n"));
        return;
    }
    if (VALUES_P(object))
        print_values(theVALUES(object), output);
    else
        print_object_notln(object, output);
    write_char(output, TO_CHAR('\n'));
}
