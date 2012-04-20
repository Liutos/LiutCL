#include "eval.h"
#include "model.h"
#include "print.h"
#include "types.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

struct LispObject lt_t;
struct LispObject lt_nil;
struct LispObject lt_null;

PHEAD(lt_binary_add)
{
    struct LispObject *arg1, *arg2, *result;

    arg1 = CAR(arg_list);
    arg2 = CADR(arg_list);
    assert(INTEGER == arg1->atom_type &&
	   INTEGER == arg2->atom_type);
    result = malloc(sizeof(struct LispObject));
    result->type = ATOM;
    result->atom_type = INTEGER;
    result->integer = INTEGER(arg1) + INTEGER(arg2);

    return result;
}

PHEAD(lt_binary_mul)
{
    struct LispObject *arg1, *arg2, *result;

    arg1 = CAR(arg_list);
    arg2 = CADR(arg_list);
    assert(INTEGER == arg1->atom_type &&
	   INTEGER == arg2->atom_type);
    result = malloc(sizeof(struct LispObject));
    result->type = ATOM;
    result->atom_type = INTEGER;
    INTEGER(result) = INTEGER(arg1) * INTEGER(arg2);

    return result;
}

PHEAD(lt_binary_sub)
{
    struct LispObject *arg1, *arg2, *result;

    arg1 = CAR(arg_list);
    arg2 = CADR(arg_list);
    assert(INTEGER == arg1->atom_type &&
	   INTEGER == arg2->atom_type);
    result = malloc(sizeof(struct LispObject));
    result->type = ATOM;
    result->atom_type = INTEGER;
    INTEGER(result) = INTEGER(arg1) - INTEGER(arg2);

    return result;
}

PHEAD(lt_car)
{
    return CAR(CAR(arg_list));	/* Argument arg_list is a proper list so the first argument is stored in the car of it. It's better of using the macros defined in the file types.h for extracting the argument in the argument list. */
}

PHEAD(lt_cdr)
{
    return CDR(CAR(arg_list));
}

struct LispObject *cons_two_objects(struct LispObject *car_ele,
				    struct LispObject *cdr_ele)
{
    struct LispObject *cons;

    cons = malloc(sizeof(struct LispObject));
    cons->type =CONS;
    cons->car = car_ele;
    cons->cdr = cdr_ele;

    return cons;
}

struct LispObject *list_two_objects(struct LispObject *obj1,
				    struct LispObject *obj2)
{
    struct LispObject *cons1, *cons2;

    cons2 = malloc(sizeof(struct LispObject));
    cons2->type = CONS;
    cons2->atom_type = DO_NOT_MIND;
    CAR(cons2) = obj2;
    CDR(cons2) = NIL;

    cons1 = malloc(sizeof(struct LispObject));
    cons1->type = CONS;
    cons1->atom_type = DO_NOT_MIND;
    CAR(cons1) = obj1;
    CDR(cons1) = cons2;

    return cons1;
}

PHEAD(lt_cons)
{
    struct LispObject *arg1, *arg2;

    arg1 = CAR(arg_list);
    arg2 = CAR(CDR(arg_list));

    return cons_two_objects(arg1, arg2);
}

PHEAD(lt_eq)
{
    struct LispObject *arg1, *arg2;

    arg1 = CAR(arg_list);
    arg2 = CAR(CDR(arg_list));

    return arg1 == arg2 ? &lt_t : &lt_nil;
}

char *gensym(void)
{
    char *symbol;
    static int counter = 0;

    symbol = calloc(10, sizeof(char));
    sprintf(symbol, "L%d", counter);
    counter++;

    return symbol;
}

PHEAD(lt_gensym)
{
    return make_atom(gensym(), env);
}

BOOLEAN is_null(struct LispObject *object)
{
    return object == NIL;
}

struct LispObject *append_cons(struct LispObject *cons1, struct LispObject *cons2)
{
    struct LispObject *tmp;

    assert(cons1 != NULL);
    tmp = cons1;
    while (is_null(tmp) == FALSE &&
	   is_null(CDR(tmp)) == FALSE)
	tmp = CDR(tmp);
    CDR(tmp) = cons2;

    return cons1;
}

PHEAD(lt_if)
{
    struct LispObject *test, *then_part, *else_part;

    test = CAR(arg_list);
    then_part = CADR(arg_list);
    else_part = CADDR(arg_list);

    if (&lt_t == eval_expression(test, env))
	return eval_expression(then_part, env);
    else
	return eval_expression(else_part, env);
}

PHEAD(lt_numeric_eq)
{
    struct LispObject *arg1, *arg2;

    arg1 = CAR(arg_list);
    arg2 = CADR(arg_list);
    assert(INTEGER == arg1->atom_type &&
	   INTEGER == arg2->atom_type);

    return INTEGER(arg1) == INTEGER(arg2) ? &lt_t : &lt_nil;
}

PHEAD(lt_numeric_gt)
{
    struct LispObject *arg1, *arg2;

    arg1 = CAR(arg_list);
    arg2 = CADR(arg_list);
    assert(INTEGER == arg1->atom_type &&
	   INTEGER == arg2->atom_type);

    return INTEGER(arg1) > INTEGER(arg2) ? &lt_t : &lt_nil;
}

PHEAD(lt_numeric_lt)
{
    struct LispObject *arg1, *arg2;

    arg1 = CAR(arg_list);
    arg2 = CADR(arg_list);
    assert(INTEGER == arg1->atom_type &&
	   INTEGER == arg2->atom_type);

    return INTEGER(arg1) < INTEGER(arg2) ? &lt_t : &lt_nil;
}

PHEAD(lt_quit)
{
    exit(0);
}

PHEAD(lt_quote)
{
    return CAR(arg_list);
}

void set_symbol_value(ENVIRONMENT *env, char *symbol_name,
		      struct LispObject *value)
{
    struct LookupEntry *first_node;

    first_node = env->head_node->next;
    while (first_node != NULL) {
	if (strcasecmp(symbol_name, first_node->symbol_name) == 0) {
	    first_node->value = value;
	    break;
	}
	first_node = first_node->next;
    }
}

PHEAD(lt_set)
{
    struct LispObject *symbol_object, *value;

    symbol_object = CAR(arg_list);
    value = CADR(arg_list);
    set_symbol_value(env, SYMBOL_NAME(symbol_object), value);

    return value;
}

int cons_length(struct LispObject *cons)
{
    int length = 0;

    while (cons != NIL) {
	length++;
	cons = CDR(cons);
    }

    return length;
}

void add_new_symbol_tail(ENVIRONMENT *env, char *symbol_name, struct LispObject *symbol_object)
{
    struct LookupEntry *head_node, *entry;

    head_node = HEAD_NODE(env);
    while (head_node != NULL && head_node->next != NULL)
	head_node = head_node->next;
    entry = malloc(sizeof(struct LookupEntry));
    entry->symbol_name = symbol_name;
    entry->symbol_object = symbol_object;
    entry->value = NULL;
    entry->next = NULL;
    head_node->next = entry;
}

ENVIRONMENT *make_closure_env(struct LispObject *argv)
{
    ENVIRONMENT *closure_env;

    closure_env = malloc(sizeof(ENVIRONMENT));
    closure_env->type = ATOM;
    closure_env->atom_type = LOOKUP_TABLE;
    closure_env->env_name = "closure";
    closure_env->head_node = malloc(sizeof(struct LookupEntry));
    while (argv != NIL) {
	add_new_symbol_tail(closure_env, SYMBOL_NAME(CAR(argv)), NULL); /* The order of the creation of symbol in environment can not be changed */
	argv = CDR(argv);
    }

    return closure_env;
}

ENVIRONMENT *concatenate_env(ENVIRONMENT *env1, ENVIRONMENT *env2)
{
    env1->next_env = env2;

    return env1;
}

struct LispObject *with_symbol_cons(char *symbol,
				    struct LispObject *expression,
				    ENVIRONMENT *env)
{
    return cons_two_objects(make_atom(symbol, env), expression);
}

PHEAD(lt_lambda)
{
    struct LispObject *argv, *body, *closure;

    argv = CAR(arg_list);
    body = CDR(arg_list);
    closure = malloc(sizeof(struct LispObject));
    closure->type = ATOM;
    closure->atom_type = FUNCTION;
    EXEC_TYPE(closure) = INTERPRET; /* Run as interpreted */
    FUNC_TYPE(closure) = REGULAR;   /* Evaluate the arguments */
    FUNC_EXPR(closure) = with_symbol_cons("progn", body, env);
    closure->arg_num = cons_length(argv);
    closure->func_env = concatenate_env(make_closure_env(argv), env); /* Lexical environment */

    return closure;
}

PHEAD(lt_progn)
{
    struct LispObject *body;

    body = arg_list;
    while (body != NIL && CDR(body) != NIL) {
	eval_expression(CAR(body), env);
	body = CDR(body);
    }
    if (body != NIL)
	return eval_expression(CAR(body), env);
    else
	return NIL;
}

PHEAD(lt_eval)
{
    struct LispObject *expr;

    expr = CAR(arg_list);

    return eval_expression(expr, env);
}

PHEAD(lt_macro)
{
    struct LispObject *argv, *body, *macro;

    argv = CAR(arg_list);
    body = CDR(arg_list);
    macro = malloc(sizeof(struct LispObject));
    macro->type = ATOM;
    macro->atom_type = FUNCTION;
    EXEC_TYPE(macro) = INTERPRET; /* Run as interpreted */
    FUNC_TYPE(macro) = MACRO;	/* Don't evaluate the arguments */
    FUNC_EXPR(macro) = with_symbol_cons("progn", body, env);
    macro->arg_num = cons_length(argv);
    macro->func_env = concatenate_env(make_closure_env(argv), env);

    return macro;
}

PHEAD(lt_print)
{
    struct LispObject *object;

    object = CAR(arg_list);
    print_object(object);

    return &lt_nil;
}

PHEAD(lt_type_of)
{
    struct LispObject *object;

    object = CAR(arg_list);
    if (ATOM == object->type) {
	switch (object->atom_type) {
	case FUNCTION: return make_atom("function", env);
	case INTEGER: return make_atom("integer", env);
	case STRING: return make_atom("string", env);
	case SYMBOL: return make_atom("symbol", env);
	default : return NULL;
	}
    } else
	return make_atom("cons", env);
}

struct LispObject *bq_eval_expr(struct LispObject *list, ENVIRONMENT *env)
{
    struct LispObject *tmp, *arg, *value;

    tmp = list;
    while (is_null(tmp) == FALSE) {
	arg = CAR(tmp);
	if (ATOM == arg->type) {
	    if (SYMBOL == arg->atom_type &&
		strcmp("comma", SYMBOL_NAME(arg)) == 0) {
		return eval_expression(CADR(tmp), env);
	    }
	} else {
	    if (ATOM == CAR(arg)->type && SYMBOL == CAR(arg)->atom_type &&
		strcmp("comma", SYMBOL_NAME(CAR(arg))) == 0) {
		CAR(tmp) = eval_expression(CADR(arg), env);
	    } else {
		value = bq_eval_expr(CADR(arg), env);
		if (ATOM == value->type)
		    CAR(CDR(arg)) = value;
		else
		    CDR(arg) = value;
	    }
	}
	tmp = CDR(tmp);
    }

    return list;
}

PHEAD(lt_backquote)
{
    struct LispObject *body;

    body = CAR(arg_list);
    if (ATOM == body->type)
	return body;
    else
	return bq_eval_expr(body, env);
}

PHEAD(lt_closure_env)
{
    struct LispObject *clz;

    clz = CAR(arg_list);
    assert(ATOM == clz->type &&
	   FUNCTION == clz->atom_type &&
	   INTERPRET == clz->expr_type);

    return clz->func_env;
}

PHEAD(lt_dump_env)		/* The wrapper function for printting the information when needed */
{
    print_object(env);

    return NULL;
}

void add_lookup_entry(ENVIRONMENT *env, struct LookupEntry *entry)
{
    struct LookupEntry *head_node;

    head_node = env->head_node;
    entry->next = head_node->next;
    head_node->next = entry;
}

void register_primitive(ENVIRONMENT *env, char *symbol_name, PRIMITIVE fn,
			enum FUNC_TYPE func_type, int arg_num,
			enum ARGC_TYPE argc_type)
{
    struct LispObject *symbol_object, *function;
    struct LookupEntry *entry, *head_node;

    head_node = env->head_node;
    /* The symbol_object slot initialization */
    symbol_object = malloc(sizeof(struct LispObject));
    symbol_object->type = ATOM;
    symbol_object->atom_type = SYMBOL;
    symbol_object->name = symbol_name;
    /* The function slot initialization */
    function = malloc(sizeof(struct LispObject));
    function->type = ATOM;
    function->atom_type = FUNCTION;
    function->expr_type = COMPILE;
    FUNC_CODE(function) = fn;
    FUNC_TYPE(function) = func_type;
    FUNC_ARGC(function) = arg_num;
    ARGC_TYPE(function) = argc_type;
    /* The entry contains the symbol_object and the function above */
    entry = malloc(sizeof(struct LookupEntry));
    entry->symbol_name = symbol_name;
    entry->symbol_object = symbol_object;
    ENTRY_VALUE(entry) = function;
    entry->next = NULL;
    add_lookup_entry(env, entry); /* Abstract the operators above as a function alone is better when changing the inner structure of the argument env */
}

void init_primitives(ENVIRONMENT *env)
{
    lt_t.type = ATOM;
    lt_t.atom_type = SYMBOL;
    lt_t.name = "t";
    add_new_symbol(env, "t", &lt_t);
    set_symbol_value(env, "t", &lt_t);

    lt_nil.type = ATOM;
    lt_nil.atom_type = SYMBOL;
    lt_nil.name = "nil";
    add_new_symbol(env, "nil", &lt_nil);
    set_symbol_value(env, "nil", &lt_nil);

    lt_null.type = CONS;
    lt_null.atom_type = DO_NOT_MIND;
    lt_null.car = lt_null.cdr = NIL;

    register_primitive(env, "car", lt_car, REGULAR, 1, FIXED);
    register_primitive(env, "cdr", lt_cdr, REGULAR, 1, FIXED);
    register_primitive(env, "cons", lt_cons, REGULAR, 2, FIXED);
    register_primitive(env, "eq", lt_eq, REGULAR, 2, FIXED);
    register_primitive(env, "eval", lt_eval, REGULAR, 1, FIXED);
    register_primitive(env, "gensym", lt_gensym, REGULAR, 0, FIXED);
    register_primitive(env, "if", lt_if, SPECIAL, 3, FIXED);
    register_primitive(env, "lambda", lt_lambda, SPECIAL, 2, VARIABLE);
    register_primitive(env, "print", lt_print, REGULAR, 1, FIXED);
    register_primitive(env, "progn", lt_progn, SPECIAL, 1, VARIABLE);
    register_primitive(env, "quit", lt_quit, REGULAR, 0, FIXED);
    register_primitive(env, "quote", lt_quote, SPECIAL, 1, FIXED);
    register_primitive(env, "set", lt_set, REGULAR, 2, FIXED);
    register_primitive(env, "type-of", lt_type_of, REGULAR, 1, FIXED);
    register_primitive(env, "+", lt_binary_add, REGULAR, 2, FIXED);
    register_primitive(env, "-", lt_binary_sub, REGULAR, 2, FIXED);
    register_primitive(env, "*", lt_binary_mul, REGULAR, 2, FIXED);
    register_primitive(env, "=", lt_numeric_eq, REGULAR, 2, FIXED);
    register_primitive(env, ">", lt_numeric_gt, REGULAR, 2, FIXED);
    register_primitive(env, "<", lt_numeric_lt, REGULAR, 2, FIXED);
    register_primitive(env, "lt-backquote", lt_backquote, SPECIAL, 1, FIXED);
    register_primitive(env, "lt-clz-env", lt_closure_env, REGULAR, 1, FIXED);
    register_primitive(env, "lt-dump-env", lt_dump_env, SPECIAL, 0, FIXED);
    register_primitive(env, "lt-macro", lt_macro, SPECIAL, 2, VARIABLE);
}
