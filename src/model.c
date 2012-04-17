#include "types.h"

#include <ctype.h>
#include <regex.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/types.h>

BOOLEAN is_atom_expression(char *expression)
{
    if (expression[0] != '(')
	return TRUE;
    else
	return FALSE;
}

BOOLEAN is_string_token(char *token)
{
    return '"' == token[0] && '"' == token[strlen(token) - 1];
}

BOOLEAN is_integer(char *token)
{
    /* const char *pattern = "[1-9][0-9]*"; */
    /* int cflags = REG_EXTENDED; */
    /* regex_t preg; */
    /* regmatch_t pmatch[1]; */

    /* regcomp(&preg, pattern, cflags); */

    /* return regexec(&preg, token, 1, pmatch, REG_NOTBOL) == 0 ? TRUE : FALSE; */
    int i;
    BOOLEAN result = TRUE;

    for (i = 0; token[i] != '\0'; i++)
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

struct LookupEntry *lookup_symbol(ENVIRONMENT *env, char *symbol_name)
{
    struct LookupEntry *first_node;

    first_node = env->head_node->next;
    while (first_node != NULL) {
	if (strcasecmp(symbol_name, first_node->symbol_name) == 0)
	    return first_node;
	first_node = first_node->next;
    }

    return NULL;
}

/* struct LispObject *lookup_symbol_fn(ENVIRONMENT *env, char *symbol_name) */
/* { */
/*     struct LookupEntry *result; */

/*     result = lookup_symbol(env, symbol_name); */

/*     return result != NULL ? ENTRY_VALUE(result) : NULL; */
/* } */

struct LispObject *lookup_symbol_value(ENVIRONMENT *env, char *symbol_name)
{
    struct LookupEntry *result;

    result = lookup_symbol(env, symbol_name);

    return result != NULL ? ENTRY_VALUE(result) : NULL;
}

void add_new_symbol(ENVIRONMENT *env, char *symbol_name, struct LispObject *symbol_object) /* When calling this function, the caller must ensure that  */
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
	i++;
	while (expression[i] != '"' && expression[i] != '\0')
	    i++;
	i++;
	break;
    default :
	while (expression[i] != ' ' &&
	       expression[i] != '(' &&
	       expression[i] != ')' &&
	       expression[i] != '\0' &&
	       expression[i] != '\n')
	    i++;
    }

    token = malloc(sizeof(i + 1) * sizeof(char));
    strncpy(token, expression, i);
    token[i] = '\0';

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
	STRING(atom) = expression;
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
	    /* add_env_symbol(env, expression, atom, NULL, NULL); */
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
    return make_atom_core(get_next_token(expression), env);
}

char *get_cons_content(char *expression)
{
    char *content;
    int balance, i;

    balance = 0;
    i = -1;
    do {
	i++;
	if ('(' == expression[i]) balance++;
	if (')' == expression[i]) balance--;
    } while (balance != 0 && expression[i] != '\0');

    content = malloc(i * sizeof(char));
    strncpy(content, expression + 1, i - 1);
    content[i - 1] = '\0';

    return content;
}

struct LispObject *make_cons_core(char *expression, ENVIRONMENT *env)
{
    char *content, *token;
    int i, step;
    struct LispObject *atom, *cur, *head, *pre;

    i = 0;
    head = malloc(sizeof(struct LispObject));
    pre = head;
    while (expression[i] != '\0') {
	switch (expression[i]) {
	case '(':
	    content = get_cons_content(expression + i);
	    cur = malloc(sizeof(struct LispObject));
	    cur->type = CONS;
	    cur->car = make_cons_core(content, env);
	    cur->cdr = NULL;
	    step = strlen(content) + 2;
	    break;
	case ')':
	    step = 0;
	    break;
	case ' ':
	case '\n':
	case '\t':
	    step = 1;
	    break;
	default :
	    token = get_next_token(expression + i);
	    atom = make_atom(token, env);
	    cur = malloc(sizeof(struct LispObject));
	    cur->type = CONS;
	    cur->car = atom;
	    cur->cdr = NULL;
	    step = strlen(token);
	}
	if (expression[i] != ' ' &&
	    expression[i] != ')' &&
	    expression[i] != '\n' &&
	    expression[i] != '\t') {
	    pre->cdr = cur;
	    pre = cur;
	}
	i += step;
    }

    return head->cdr;
}

struct LispObject *make_cons(char *expression, ENVIRONMENT *env)
{
    return make_cons_core(get_cons_content(expression), env);
}

struct LispObject *make_object(char *raw_expression, ENVIRONMENT *env)
{
    if (is_atom_expression(raw_expression))
	return make_atom(raw_expression, env);
    else
	return make_cons(raw_expression, env);
}
