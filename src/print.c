#include "primitives.h"
#include "types.h"

#include <stdio.h>
#include <ctype.h>

extern struct LispObject lt_null;

void print_env(ENVIRONMENT *env, BOOLEAN is_recursive)
{
    void print_atom(struct LispObject *);
    struct LookupEntry *first_node;

    first_node = HEAD_NODE(env)->next;
    printf("Environment Name: %s\nNAME\t\tOBJECT\t\tVALUE\n",
	   env->env_name);
    while (first_node != NULL) {
	printf("%-16s%-16p",
	       first_node->symbol_name,
	       first_node->symbol_object);
	if (ENTRY_VALUE(first_node) != NULL) {
	    if (ATOM == ENTRY_VALUE(first_node)->type) {
		print_atom(ENTRY_VALUE(first_node));
		putchar('\n');
	    } else
		printf("%p\n", ENTRY_VALUE(first_node));
	} else
	    printf("%p\n", ENTRY_VALUE(first_node));
	first_node = first_node->next;
    }
    if (TRUE == is_recursive)
	if (env->next_env != NULL)
	    print_env(env->next_env, TRUE);
}

void print_atom(struct LispObject *atom_object)
{
    void print_cons(struct LispObject *);
    char *name;
    int i;
    /* struct LookupEntry *first_node; */

    switch (atom_object->atom_type) {
    case FUNCTION:
	if (COMPILE == EXEC_TYPE(atom_object))
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
	print_env(atom_object, FALSE);
	break;
    case STRING:
	printf("\"%s\"", atom_object->string);
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
    while (is_null(cons_object) == FALSE) {
	if (ATOM == CAR(cons_object)->type)
	    print_atom(CAR(cons_object));
	else {
	    putchar('(');
	    print_cons_core(CAR(cons_object));
	    putchar(')');
	}
	if (CDR(cons_object) != NIL) {
	    if (ATOM == CDR(cons_object)->type) {
		printf(" . ");
		print_atom(CDR(cons_object));
		cons_object = NIL;
	    } else {
		putchar(' ');
		cons_object = CDR(cons_object);
	    }
	} else
	    cons_object = NIL;
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
