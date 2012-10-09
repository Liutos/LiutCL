/*
 * read-sexp.c
 *
 * Parses the string and generates the correspoding inner structures of
 * S-expressions.
 *
 * Copyright (C) 2012-10-03 liutos
 */
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include "types.h"
#include "cons.h"
#include "atom_proc.h"
#include "object.h"

#define NIL lt_void

extern Boolean lt_true, lt_false;

char *get_next_token(char *string, int *offset)
{
    int i;

    i = 0;
    while (string[i] != ' ' && string[i] != '(' &&
	   string[i] != ')' && string[i] != '\0')
	i++;
    *offset = i;

    return strndup(string, i);
}

BOOL is_integer_token(char *token)
{
    BOOL flag;
    int i;

    flag = TRUE;
    for (i = 0; token[i] != '\0'; i++) {
	if (isdigit(token[i]) == 0) {
	    flag = FALSE;
	    break;
	}
    }

    return flag;
}

LispType token_type(char *token)
{
    if (is_integer_token(token))
	return INTEGER;

    return SYMBOL;
}

Atom parse_atom(char *token)
{
    Atom atom;
    LispType type;

    type = token_type(token);
    switch (type) {
    case INTEGER:
	atom = new_object();
	atom->type = type;
	atom->integer = atoi(token);
	break;
    case SYMBOL:
	atom = ensure_symbol_exists(token);
	break;
    default :
	fprintf(stderr, "Don't know how to make atom for token '%s'.\n", token);
	exit(0);
    }

    return atom;
}

Cons parse_cons_core(char *string, int *offset)
{
    Cons head, cur, pre;
    int step, i;
    char *token;

    pre = head = new_object();
    for (i = 0; string[i] != '\0'; i += step) {
	switch (string[i]) {
	case '(':
	    cur = make_cons_cell(parse_cons_core(string + i + 1, &step), NIL);
	    break;
	case ' ':
	    step = 1;
	    continue;
	case ')':
	    *offset = i + 2;
	    pre = CDR(head);
	    free(head);

	    return pre;
	default :
	    token = get_next_token(string + i, &step);
	    cur = make_cons_cell(parse_atom(token), NIL);
	}
	CDR(pre) = cur;
	pre = cur;
    }
    pre = CDR(head);
    free(head);

    return pre;
}

Cons parse_cons(char *string)
{
    int tmp;

    return parse_cons_core(string + 1, &tmp);
}

LispObject parse_sexp(char *string)
{
    if ('(' == string[0])
	return parse_cons(string);
    else
	return parse_atom(string);
}
