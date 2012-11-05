/*
 * read-sexp.c
 *
 * 
 *
 * Copyright (C) 2012-10-03 liutos
 */
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "atom_proc.h"
#include "cons.h"
#include "object.h"
#include "symbol_table.h"
#include "types.h"
#include "stream.h"

char *get_next_token(char *string, int *offset)
{
    int i;

    if ('"' == *string) {
        i = 1;
        while (string[i] != '\0' && string[i] != '"')
            i++;
        if ('\0' == string[i]) {
            write_format(standard_error, "Incomplete string\n");
            exit(1);
        }
        *offset = ++i;
    } else {
        i = 0;
        while (string[i] != ' ' && string[i] != '(' &&
               string[i] != ')' && string[i] != '\0' &&
               string[i] != '\n')
            i++;
        *offset = i;
    }

    return strndup(string, i);
}

BOOL is_integer_token(char *token)
{
    BOOL flag = TRUE;
    int i;
    for (i = 0; token[i] != '\0'; i++)
	if (isdigit(token[i]) == 0)
            return FALSE;

    return flag;
}

BOOL is_string_token(char *token)
{
    return '"' == *token && '"' == token[strlen(token) - 1];
}

LispType token_type(char *token)
{
    if (is_integer_token(token))
	return FIXNUM;
    if (is_string_token(token))
        return STRING;

    return SYMBOL;
}

char *toupper_string(char *origin)
{
    char *target = strdup(origin);
    int i;
    for (i = 0; target[i] != '\0'; ++i)
        target[i] = toupper(target[i]);

    return target;
}

Atom parse_atom(char *token)
{
    LispType type = token_type(token);
    switch (type) {
    case FIXNUM:
        return make_fixnum(atoi(token));
    case STRING:
        return make_string(strndup(token + 1, strlen(token) - 2));
    case SYMBOL:
	return ensure_symbol_exists(toupper_string(token));
    default :
        write_format(standard_error, "Don't know how to parse token %!.\n", token);
	exit(0);
    }
}

Cons parse_cons_aux(char *string, int *offset)
{
    Cons head, cur, pre;
    int step, i;
    char *token;

    pre = head = make_cons(lt_nil, lt_nil);
    for (i = 0; string[i] != '\0'; i += step) {
	switch (string[i]) {
	case '(':
	    cur = make_cons(parse_cons_aux(string + i + 1, &step), lt_nil);
	    break;
	case ' ':
        case '\n':
	    step = 1;
	    continue;
	case ')':
	    *offset = i + 2;
	    pre = CDR(head);
            free_cons(head);

	    return pre;
	default :
	    token = get_next_token(string + i, &step);
	    cur = make_cons(parse_atom(token), lt_nil);
	}
	_CDR(pre) = cur;
	pre = cur;
    }
    pre = CDR(head);
    free_cons(head);

    return pre;
}

Cons parse_cons(char *input)
{
    int tmp;

    return parse_cons_aux(input + 1, &tmp);
}

LispObject parse_sexp(char *input)
{
    int trash;

    while ('\0' != *input && isblank(*input))
        input++;
    if ('\0' == *input)
        return NULL;
    if ('(' == input[0])
	return parse_cons(input);
    else
        return parse_atom(get_next_token(input, &trash));
}
