/*
 * print_sexp.c
 *
 * Prints an inner structures of a S-expression to screen. Depends on the low-
 * level operations implemented in stream module.
 *
 * Copyright (C) 2012-10-03 liutos
 */
#include "types.h"
#include "atom_proc.h"
#include "stream.h"
#include "object.h"

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

void print_sexp_notln(LispObject, Stream);
void print_cons(LispObject, Stream);

void print_atom(Atom atom, Stream output)
{
    switch (TYPE(atom)) {
    case SYMBOL:
        /* print_symbol(theSYMBOL(atom), stream); */
        printf("%s", theSYMBOL(atom)->symbol_name);
	break;
    case INTEGER:
        write_file_stream_integer(output, atom);
	break;
    case FUNCTION:
	if (TRUE == FUNC_FLAG(atom))
	    printf("#<FUNCTION C %p>", atom);
	else {
	    printf("#<FUNCTION I ");
	    print_cons(PARAMETERS(atom), output);
	    putchar(' ');
	    print_sexp_notln(EXPRESSION(atom), output);
	    putchar(' ');
	    printf("%p>", atom);
	}
	break;
    case STRING:
        printf("\"%s\"", STRING(atom));
        break;
    case CHARACTER:
        write_stream_char(standard_output, atom);
        break;
    case STREAM:
        printf("#<STREAM %p>", atom);
        break;
    default :
	fprintf(stderr, "Unknown type '%d'\n", TYPE(atom));
	exit(1);
    }
}

void print_cons_core(LispObject cons, Stream output)
{
    while (!is_tail(cons)) {
        /* The variable `cons' here would never be the lt_void
           so it's safe to use the macro CAR instead of SCAR. */
	if (CONS_P(CAR(cons)))
            print_cons(CAR(cons), output);
        else
            print_atom(CAR(cons), output);
	cons = CDR(cons);
	if (is_atom_object(cons)) {
	    if (cons != lt_nil) {
		printf(" . ");
		print_atom(cons, output);
	    }
	} else
            putchar(' ');
    }
}

void print_cons(LispObject cons, Stream output)
{
    putchar('(');
    print_cons_core(cons, output);
    putchar(')');
}

void print_sexp_notln(LispObject object, Stream output)
{
    if (CONS_P(object))
	print_cons(object, output);
    else
	print_atom(object, output);
}

/* Print the representation of OBJECT to OUTPUT. */
void print_sexp(LispObject object, Stream output)
{
    if (NULL == object) {
        printf("; No value\n");
        return;
    }
    print_sexp_notln(object, output);
    putchar('\n');
}
