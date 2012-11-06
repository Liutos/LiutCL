/*
 * print_sexp.c
 *
 * 
 *
 * Copyright (C) 2012-10-03 liutos
 */
#include <stdio.h>
#include <stdlib.h>

#include "atom_proc.h"
#include "cons.h"
#include "object.h"
#include "stream.h"
#include "types.h"

void print_object_notln(LispObject, Stream);

void print_atom(Atom atom, Stream output)
{
    switch (TYPE(atom)) {
    case SYMBOL:
        write_string(output, make_string(SYMBOL_NAME(atom)));
	break;
    case FIXNUM:
        write_fixnum(output, atom);
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
    case STRING:
        write_format(output, "\"%s\"", atom);
        break;
    case CHARACTER:
        write_format(output, "#\\%c", atom);
        break;
    case STREAM:
        write_format(output, "#<STREAM %p>", atom);
        break;
    default :
        write_format(output, "Unknown type %d\n", TO_FIXNUM(TYPE(atom)));
	exit(1);
    }
}

void print_cons_core(LispObject cons, Stream output)
{
    while (!is_tail(cons)) {
	if (CONS_P(CAR(cons)))
            print_object_notln(CAR(cons), output);
        else
            print_atom(CAR(cons), output);
	cons = CDR(cons);
	if (is_atom_object(cons)) {
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

/* Print the representation of `object' to OUTPUT. */
void print_object(LispObject object, Stream output)
{
    if (NULL == object) {
        write_string(output, make_string("; No value\n"));
        return;
    }
    print_object_notln(object, output);
    write_char(output, TO_CHAR('\n'));
}
