/*
 * read-sexp.c
 *
 * Parses a text input and generates the inner structures.
 *
 * Copyright (C) 2012-10-03 liutos
 */
#include <ctype.h>
#include <gmp.h>
#include <regex.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#include "atom.h"
#include "cons.h"
#include "number.h"
#include "object.h"
#include "package.h"
#include "stream.h"
#include "symbol.h"
#include "types.h"

LispObject parse_input(char *);
LispObject parse_sexp(char *, int *);

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

/* Tokens classifier */
BOOL is_match_regex(char *src, char *pattern)
{
    regex_t regex;

    if (regcomp(&regex, pattern, REG_EXTENDED)) {
        fprintf(stderr, "Couldn't compile regex %s.\n", pattern);
        exit(1);
    }

    return 0 == regexec(&regex, src, 0, NULL, 0);
}

BOOL is_fixnum_token(char *token)
{
    int sum;

    sum = 0;
    if ('-' == *token) {
        for (int i = 0; token[i] != '\0'; i++) {
            if (sum > MAX_FIXNUM)
                return FALSE;
            sum = sum * 10 + token[i] - '0';
        }
    } else {
        for (int i = 0; token[i] != '\0'; i++) {
            if (sum < MIN_FIXNUM)
                return FALSE;
            sum = sum * 10 - (token[i] - '0');
        }
    }

    return TRUE;
}

/* Tell whether the `token' means an integer and needs other functions to tell
   more precise subtype of Integer. */
BOOL is_integer_token(char *token)
{
    return is_match_regex(token, "^-?[0-9]+\\.?$");
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

BOOL is_ratio_token(char *token)
{
    return is_match_regex(token, "^-?[0-9]+/[0-9]+$");
}

BOOL is_string_token(char *token)
{
    return is_match_regex(token, "^\"([^\"\\]+|\\.)*\"$");
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

/* Generators */
Bignum parse_bignum(char *token)
{
    mpz_t x;

    mpz_init(x);
    mpz_set_str(x, token, 10);

    return make_bignum(x);
}

Integer parse_integer(char *token)
{
    Integer n;

    if (is_fixnum_token(token))
        n = make_fixnum(atoi(token));
    else
        n = parse_bignum(token);
    free(token);

    return n;
}

Ratio parse_ratio(char *token)
{
    int i;
    ratio_t ratio;

    for (i = 0; token[i] != '\0'; i++) {
        if ('/' == token[i]) {
            token[i] = '\0';
            break;
        }
    }
    ratio = malloc(sizeof(ratio_t));
    ratio->numerator = parse_input(token);
    ratio->denominator = parse_input(token + i + 1);
    free(token);

    return make_ratio(ratio);
}

String parse_string(char *token)
{
    String str;

    str = make_string(strndup(token + 1, strlen(token) - 2));
    free(token);

    return str;
}

Symbol parse_symbol(char *token, Package pkg)
{
    char *pkg_name, *sym_name;

    token = toupper_string(token);
    if (is_keyword_token(token))
        return gen_keyword(token + 1);
    if (is_qualified(token, &pkg_name, &sym_name)) {
        Package pkg;

        pkg = find_package(pkg_name);

        return gen_symbol(sym_name, pkg);
    } else
        return gen_symbol(token, pkg);
}

Atom parse_atom_aux(char *token)
{
    if (is_integer_token(token))
        return parse_integer(token);
    if (is_ratio_token(token))
        return parse_ratio(token);
    if (is_string_token(token))
        return parse_string(token);

    return parse_symbol(token, package);
}

Atom parse_atom(char *input, int *offset)
{
    char *token;

    token = get_next_token(input, offset);
    /* `token' is a brand-new string and the callee use it for assignment. */

    return parse_atom_aux(token);
}

Cons parse_cons(char *string, int *offset)
{
    Cons cur, head, pre;
    int step;

    pre = head = make_cons(lt_nil, lt_nil);
    for (int i = 0; string[i] != '\0'; i += step) {
	switch (string[i]) {
	case '(':
	    cur = make_cons(parse_cons(string + i + 1, &step), lt_nil);
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
            /* Symbol quote; */
            LispObject obj;

            /* quote = S("QUOTE"); */
            obj = parse_sexp(string + i + 1, &step);
            /* cur = make_cons(make_cons(S("QUOTE"), make_cons(obj, lt_nil)), lt_nil); */
            cur = make_cons(make_list(S("QUOTE"), obj), lt_nil);
            step++;
            break;
        }
	default :
	    cur = make_cons(parse_atom(string + i, &step), lt_nil);
	}
        set_cdr(pre, cur);
	pre = cur;
    }
    pre = CDR(head);
    free_cons(head);

    return pre;
}

/* The `input' will be free. Don't assign it directly. */
LispObject parse_sexp(char *input, int *offset)
{
    if ('\0' == *input)
        return NULL;
    while ('\0' != *input && isblank(*input))
        input++;
    if ('\'' == *input) {
        LispObject obj;
        /* Symbol quote; */

        /* quote = S("QUOTE"); */
        obj = parse_input(input + 1);

        /* return make_cons(S("QUOTE"), make_cons(obj, lt_nil)); */
        return make_list(S("QUOTE"), obj);
    }
    if ('(' == input[0]) {
        Cons cons;

        cons = parse_cons(input + 1, offset);
        (*offset)++;

	return cons;
    } else
        return parse_atom(input, offset);
}

/* The `input' will be free in the future. */
LispObject parse_input(char *input)
{
    int trash;

    return parse_sexp(input, &trash);
}
