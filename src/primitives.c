#include "model.h"
#include "types.h"

#include <stdlib.h>
#include <strings.h>

struct LispObject lt_t;
struct LispObject lt_nil;

PHEAD(lt_quit)
{
    exit(0);
}

PHEAD(lt_car)
{
    return CAR(CAR(arg_list));	/* Argument arg_list is a proper list so the first argument is stored in the car of it */
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

void add_lookup_entry(ENVIRONMENT *env, struct LookupEntry *entry)
{
    struct LookupEntry *head_node;

    head_node = env->head_node;
    entry->next = head_node->next;
    head_node->next = entry;
}

void register_primitive(ENVIRONMENT *env, char *symbol_name, PRIMITIVE fn)
{
    struct LispObject *symbol_object, *function;
    struct LookupEntry *entry, *head_node;

    head_node = env->head_node;

    symbol_object = malloc(sizeof(struct LispObject));
    symbol_object->type = ATOM;
    symbol_object->atom_type = SYMBOL;
    symbol_object->name = symbol_name;

    function = malloc(sizeof(struct LispObject));
    function->type = ATOM;
    function->atom_type = FUNCTION;
    function->func_expr = fn;

    entry = malloc(sizeof(struct LookupEntry));
    entry->symbol_name = symbol_name;
    entry->symbol_object = symbol_object;
    entry->function = function;
    entry->next = NULL;
    /* entry->next = head_node->next; */
    /* head_node->next = entry; */
    add_lookup_entry(env, entry); /* Abstract the operators above as a function alone is better when changing the inner structure of the argument env */
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
    }
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

    register_primitive(env, "quit", lt_quit);
    register_primitive(env, "car", lt_car);
    register_primitive(env, "cdr", lt_cdr);
    register_primitive(env, "cons", lt_cons);
    register_primitive(env, "eq", lt_eq);
}
