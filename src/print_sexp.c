/*
 * print_sexp.c
 *
 * Prints an inner structures of a S-expression to screen.
 *
 * Copyright (C) 2012-10-03 liutos
 */
#include "types.h"
#include "atom_proc.h"
#include <stdio.h>
#include <stdlib.h>

extern void print_cons(LispObject);
extern Boolean lt_true, lt_false;

void print_sexp(LispObject);
void print_cons(LispObject);

void print_atom(Atom atom)
{
    switch (TYPE(atom)) {
    case SYMBOL:
	printf("%s", atom->symbol_name);
	break;
    case INTEGER:
	printf("%d", INTEGER(atom));
	break;
    case FUNCTION:
	if (TRUE == FUNC_FLAG(atom))
	    printf("#<FUNCTION C %p>", atom);
	else {
	    printf("#<FUNCTION I ");
	    print_cons(PARAMETERS(atom));
	    putchar(' ');
	    if (CONS == TYPE(EXPRESSION(atom)))
		print_cons(EXPRESSION(atom));
	    else
		print_atom(EXPRESSION(atom));
	    putchar(' ');
	    printf("%p>", atom);
	}
	break;
    default :
	fprintf(stderr, "Unknown type '%d'\n", TYPE(atom));
	exit(1);
    }
}

void print_cons_core(LispObject cons)
{
    while (!is_tail(cons)) {
	if (CAR(cons)->type != CONS)
	    print_atom(CAR(cons));
	else
	    print_cons(CAR(cons));
	cons = CDR(cons);
	if (!is_tail(cons)) {
	    if (is_atom_object(cons)) {
		printf(" . ");
		print_atom(cons);
	    }
	    else
		putchar(' ');
	}
    }
}

void print_cons(LispObject cons)
{
    putchar('(');
    print_cons_core(cons);
    putchar(')');
}

void print_sexp(LispObject object)
{
    if (CONS == object->type)
	print_cons(object);
    else
	print_atom(object);
    putchar('\n');
}
