#include "primitives.h"
#include "print.h"
#include "types.h"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

extern struct LispObject lt_null;

BOOLEAN is_atom_expression(char *expression)
{
    if (expression[0] != '(')
	return TRUE;
    else
	return FALSE;
}

BOOLEAN is_string_token(const char *token)
{
    return '"' == token[0] && '"' == token[strlen(token) - 1];
}

BOOLEAN is_integer(const char *token)
{
    int i = 0;
    BOOLEAN result = TRUE;

    if (('+' == token[0] || '-' == token[0]) && token[1] != '\0')
	i++;
    for (; token[i] != '\0'; i++)
	if (isdigit(token[i]) == 0) {
	    result = FALSE;
	    break;
	}

    return result;
}

enum ATOM_TYPE type_of(char *token)
{
    if (is_string_token(token)) return STRING;
    if (is_integer(token)) return INTEGER;

    return SYMBOL;
}

struct LookupEntry *lookup_symbol(ENVIRONMENT *env, char *name)
{
    struct LookupEntry *first_node;

    while (env != NULL) {
	first_node = HEAD_NODE(env)->next;
	while (first_node != NULL) {
	    if (strcasecmp(name, first_node->symbol_name) == 0)
		return first_node;
	    first_node = first_node->next;
	}
	env = env->next_env;
    }

    return NULL;
}

struct LispObject *lookup_symbol_value(ENVIRONMENT *env, char *symbol_name)
{
    struct LookupEntry *result;

    result = lookup_symbol(env, symbol_name);

    return result != NULL ? ENTRY_VALUE(result) : NULL;
}

void add_new_symbol(ENVIRONMENT *env, char *symbol_name, struct LispObject *symbol_object) /* When calling this function, the caller must ensure that the symbol own the name of argument symbol_name is a new one.  */
{
    struct LookupEntry *head_node, *entry;

    head_node = env->head_node;
    entry = malloc(sizeof(struct LookupEntry));
    entry->symbol_name = symbol_name;
    entry->symbol_object = symbol_object;
    entry->value = NULL;
    entry->next = head_node->next;
    head_node->next = entry;
}

char *get_next_token(char *expression)
{
    char *token;
    int i;

    i = 0;
    switch (expression[0]) {
    case '"':
	i = 1;
	do {
	    if ('"' == expression[i])
		if ('\\' == expression[i - 1])
		    i++;
		else
		    break;
	    else
		i++;
	} while (expression[i] != '\0');
	i++;
	break;
    default :
	while (expression[i] != ' ' && expression[i] != '(' &&
	       expression[i] != ')' && expression[i] != '\0' &&
	       expression[i] != '\n')
	    i++;
    }
    token = strndup(expression, i);

    return token;
}

struct LispObject *make_atom_core(char *expression, ENVIRONMENT *env)
{
    struct LispObject *atom;
    struct LookupEntry *entry;

    switch (type_of(expression)) {
    case INTEGER:
	atom = malloc(sizeof(struct LispObject));
	atom->type = ATOM;
	atom->atom_type = INTEGER;
	INTEGER(atom) = atoi(expression);
	break;
    case STRING:
	atom = malloc(sizeof(struct LispObject));
	atom->type = ATOM;
	atom->atom_type = STRING;
	STRING(atom) = strndup(expression + 1, strlen(expression) - 2);
	break;
    case SYMBOL:
	entry = lookup_symbol(env, expression);
	if (entry != NULL)
	    return entry->symbol_object;
	else {
	    atom = malloc(sizeof(struct LispObject));
	    atom->type = ATOM;
	    atom->atom_type = SYMBOL;
	    atom->name = expression;
	    add_new_symbol(env, expression, atom);
	}
	break;
    default :
	;
    }

    return atom;
}

struct LispObject *make_atom(char *expression, ENVIRONMENT *env)
{
    char *token = get_next_token(expression);

    return make_atom_core(token, env);
}

struct LispObject *make_cons_core(char *expression, int *offset, ENVIRONMENT *env)
{
    char *token;
    int i, sub_offset, step;
    struct LispObject *atom, *cur, *head, *pre, *dot;

    i = 0;
    head = malloc(sizeof(struct LispObject));
    head->type = CONS;
    head->atom_type = DO_NOT_MIND;
    head->car = head->cdr = NIL;
    pre = head;
    while (expression[i] != '\0') {
	switch (expression[i]) {
	case '(':
	    cur = malloc(sizeof(struct LispObject));
	    cur->type = CONS;
	    CAR(cur) = make_cons_core(expression + i + 1, &sub_offset, env);
	    CDR(cur) = NIL;
	    step = sub_offset;
	    break;
	case ')':
	    *offset = i + 2;
	    return CDR(head);
	    break;
	case ' ':
	case '\n':
	case '\t':
	    step = 1;
	    break;
	case '.':
	    if (' ' == expression[i + 1]) {
		dot = make_cons_core(expression + i + 1, &sub_offset, env);
		if (is_null(CDR(dot)))
		    CDR(pre) = CAR(dot);
		else
		    CDR(pre) = dot;
		*offset = sub_offset + i + 1;
		return CDR(head);
	    }
	    break;
	default :
	    token = get_next_token(expression + i);
	    atom = make_atom(token, env);
	    cur = malloc(sizeof(struct LispObject));
	    cur->type = CONS;
	    cur->atom_type = DO_NOT_MIND;
	    CAR(cur) = atom;
	    CDR(cur) = NIL;
	    step = strlen(token);
	}
	if (expression[i] != ' ' && expression[i] != ')' &&
	    expression[i] != '\n' && expression[i] != '\t') {
	    CDR(pre) = cur;
	    pre = cur;
	}
	i += step;
    }

    return CDR(head);
}

struct LispObject *make_cons(char *expression, ENVIRONMENT *env)
{
    int offset;

    return make_cons_core(expression + 1, &offset, env);
}

struct LispObject *make_object(char *raw_expression, ENVIRONMENT *env)
{
    struct LispObject *value;

    if (is_atom_expression(raw_expression)) {
	value = make_atom(raw_expression, env);
	return value;
    }
    else
	return make_cons(raw_expression, env);
}
