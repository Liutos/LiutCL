#include "types.h"

#include <stdio.h>
#include <ctype.h>

void print_atom(struct LispObject *atom_object)
{
    char *name;
    int i;
    struct LookupEntry *first_node;

    switch (atom_object->atom_type) {
    case LOOKUP_TABLE:
	first_node = atom_object->head_node->next;
	while (first_node != NULL) {
	    printf("name: %s \tobject: %p\tvalue: %p\tfunction: %p\n",
		   first_node->symbol_name,
		   first_node->symbol_object,
		   first_node->value,
		   first_node->function);
	    first_node = first_node->next;
	}
	printf("END");
	break;
    case SYMBOL:
	i = 0;
	name = atom_object->name;
	while (name[i] != '\0')
	    putchar(toupper(name[i++]));
	break;
    default :
	;
    }
}

void print_cons_core(struct LispObject *cons_object)
{
    while (cons_object != NULL) {
	if (ATOM == CAR(cons_object)->type)
	    print_atom(CAR(cons_object));
	else {
	    putchar('(');
	    print_cons_core(CAR(cons_object));
	    putchar(')');
	}
	if (CDR(cons_object) != NULL) {
	    if (ATOM == CDR(cons_object)->type) {
		printf(" . ");
		print_atom(CDR(cons_object));
		cons_object = NULL;
	    } else {
		putchar(' ');
		cons_object = CDR(cons_object);
	    }
	} else
	    cons_object = NULL;
    }
}

void print_cons(struct LispObject *cons_object)
{
    putchar('(');
    print_cons_core(cons_object);
    putchar(')');
}

void print_object(struct LispObject *object)
{
    if (ATOM == object->type)
	print_atom(object);
    else
	print_cons(object);
    putchar('\n');
}
