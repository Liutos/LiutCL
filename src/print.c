#include "types.h"

#include <stdio.h>
#include <ctype.h>

void print_atom(struct LispObject *atom_object)
{
    void print_cons(struct LispObject *);
    char *name;
    int i;
    struct LookupEntry *first_node;

    switch (atom_object->atom_type) {
    case FUNCTION:
	if (COMPILE == EXPR_TYPE(atom_object))
	    printf("#<Function compiled %p>", FUNC_CODE(atom_object));
	else {
	    printf("#<Function interpreted %p ", FUNC_EXPR(atom_object));
	    print_cons(FUNC_EXPR(atom_object));
	    putchar('>');
	}
	break;
    case INTEGER:
	printf("%d", INTEGER(atom_object));
	break;
    case LOOKUP_TABLE:
	first_node = atom_object->head_node->next;
	printf("NAME\tOBJECT  \tVALUE   \t\n");
	while (first_node != NULL) {
	    printf("%s\t%p\t%p   \t\n",
		   first_node->symbol_name,
		   first_node->symbol_object,
		   first_node->value);
	    first_node = first_node->next;
	}
	printf("END");
	break;
    case STRING:
	printf("%s", atom_object->string);
	break;
    case SYMBOL:
	i = 0;
	name = atom_object->name;
	while (name[i] != '\0')
	    putchar(toupper(name[i++])); /* Convert every character to its upper case */
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
    if (object != NULL) {
	if (ATOM == object->type)
	    print_atom(object);
	else
	    print_cons(object);
	putchar('\n');
    } else
	printf("Encounter a null pointer\n");
}
