/*
 * read-sexp.c
 *
 * Parses text input and generates the inner structures.
 *
 * Copyright (C) 2012-10-03 liutos
 */
#include <ctype.h>
#include <regex.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#include "atom.h"
#include "cons.h"
#include "object.h"
#include "package.h"
#include "symbol.h"
#include "stream.h"
#include "types.h"

LispObject parse_sexp(char *, Package);

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

BOOL is_fixnum_token(char *token)
{
    if ('-' == *token)
        token++;
    for (int i = 0; token[i] != '\0'; i++)
	if (isdigit(token[i]) == 0)
            return FALSE;

    return TRUE;
}

BOOL is_float_token(char *token)
{
    if ('-' == *token)
        token++;
    for (; *token != '\0' && *token != '.'; token++)
	if (isdigit(*token) == 0)
            return FALSE;
    if ('.' == *token)
        return is_fixnum_token(token + 1);
    else
        return TRUE;
}

BOOL is_string_token(char *token)
{
    regex_t regex;

    if (regcomp(&regex, "^\"([^\"\\]+|\\.)*\"$", REG_EXTENDED)) {
        error_format("Could not compile regex\n");
        exit(1);
    }

    return 0 == regexec(&regex, token, 0, NULL, 0)? TRUE: FALSE;
}

LispType token_type(char *token)
{
    if (is_fixnum_token(token))
	return FIXNUM;
    if (is_float_token(token))
        return FLOAT;
    if (is_string_token(token))
        return STRING;

    return SYMBOL;
}

char *toupper_string(char *origin)
{
    char *target;
    int i;

    target = strdup(origin);
    for (i = 0; target[i] != '\0'; ++i)
        target[i] = toupper(target[i]);

    return target;
}

BOOL is_keyword_token(char *token)
{
    return ':' == *token;
}

BOOL is_qualified(char *token, char **pkg_name, char **sym_name)
{
    int i;

    for (i = 0; token[i] != '\0' && token[i] != ':'; i++);
    /* Character colon `:' doesn't exist. */
    if ('\0' == token[i])
        return FALSE;
    /* Find colon `:'. */
    *pkg_name = strndup(token, i);
    if (':' == token[i + 1])
        i++;
    *sym_name = strdup(token + i + 1);

    return TRUE;
}

Symbol parse_symbol(char *token, Package pkg)
{
    char *pkg_name, *sym_name;

    token = toupper_string(token);
    if (is_keyword_token(token))
        return gen_pkg_sym(token + 1, pkg_kw);
    if (is_qualified(token, &pkg_name, &sym_name)) {
        Package pkg;

        pkg = find_package(pkg_name);

        return gen_pkg_sym(sym_name, pkg);
    } else
        return gen_pkg_sym(token, pkg);
}

Atom parse_atom(char *token, Package pkg)
{
    LispType type;

    type = token_type(token);
    switch (type) {
    case FIXNUM:
        return make_fixnum(atoi(token));
    case FLOAT:
        return make_float(atof(token));
    case STRING:
        return make_string(strndup(token + 1, strlen(token) - 2));
    case SYMBOL:
        return parse_symbol(token, pkg);
    default :
        write_format(standard_error, "Don't know how to parse token %!.\n", token);
	exit(0);
    }
}

Cons parse_cons_aux(char *string, int *offset, Package pkg)
{
    Cons cur, head, pre;
    char *token;
    int step;

    pre = head = make_cons(lt_nil, lt_nil);
    for (int i = 0; string[i] != '\0'; i += step) {
	switch (string[i]) {
	case '(':
	    cur = make_cons(parse_cons_aux(string + i + 1, &step, pkg), lt_nil);
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
        case '\'': {
            Symbol quote;
            LispObject obj;

            quote = gen_pkg_sym("QUOTE", pkg_cl);
            /* The following is a cons. */
            if ('(' == string[i + 1]) {
                obj = parse_cons_aux(string + i + 1, &step, pkg);
                cur = make_cons(make_cons(quote, obj), lt_nil);
            } else {
                /* The following is an atom. */
                token = get_next_token(string + i + 1, &step);
                obj = parse_atom(token, pkg);
                cur = make_cons(make_cons(quote, make_cons(obj, lt_nil)), lt_nil);
            }
            step++;
            break;
        }
	default :
	    token = get_next_token(string + i, &step);
	    cur = make_cons(parse_atom(token, pkg), lt_nil);
	}
	_CDR(pre) = cur;
	pre = cur;
    }
    pre = CDR(head);
    free_cons(head);

    return pre;
}

Cons parse_cons(char *input, Package pkg)
{
    int tmp;

    return parse_cons_aux(input + 1, &tmp, pkg);
}

LispObject parse_sexp(char *input, Package pkg)
{
    int trash;

    while ('\0' != *input && isblank(*input))
        input++;
    if ('\0' == *input)
        return NULL;
    if ('(' == input[0])
	return parse_cons(input, pkg);
    else if ('\'' == input[0]) {
        Symbol quote;

        quote = gen_pkg_sym("QUOTE", pkg_cl);

        return make_cons(quote, make_cons(parse_sexp(input + 1, pkg), lt_nil));
    } else
        return parse_atom(get_next_token(input, &trash), pkg);
}
