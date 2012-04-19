#include "model.h"
#include "primitives.h"
#include "print.h"
#include "types.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

struct LispObject *eval_expression(struct LispObject *, ENVIRONMENT *);

struct LispObject *eval_atom(struct LispObject *atom_object, ENVIRONMENT *env)
{
    assert(ATOM == atom_object->type);
    if (SYMBOL != atom_object->atom_type)
	return atom_object;
    else {
	return lookup_symbol_value(env, atom_object->name);
    }
}

ENVIRONMENT *set_closure_env(ENVIRONMENT *closure_env, struct LispObject *arg_list)
{
    struct LispObject *arg_tmp;
    struct LookupEntry *env_tmp;

    env_tmp = HEAD_NODE(closure_env)->next;
    arg_tmp = arg_list;
    while (env_tmp != NULL) {
	ENTRY_VALUE(env_tmp) = CAR(arg_tmp);
	env_tmp = env_tmp->next;
	arg_tmp = CDR(arg_tmp);
    }

    return closure_env;
}

struct LispObject *eval_args(struct LispObject *arg_list, ENVIRONMENT *env)
{
    struct LispObject *head, *curr, *prev;

    head = malloc(sizeof(struct LispObject));
    prev = head;
    while (is_null(arg_list) == FALSE) {
	curr = malloc(sizeof(struct LispObject));
	curr->type = CONS;
	curr->atom_type = DO_NOT_MIND;
	CDR(curr) = NIL;
	CAR(curr) = eval_expression(CAR(arg_list), env);
	CDR(prev) = curr;
	prev = curr;
	arg_list = CDR(arg_list);
    }

    return CDR(head);
}

struct LispObject *eval_cons(struct LispObject *cons, ENVIRONMENT *env)
{
    struct LispObject *arg_list, *op, *operator, *expand_value;

    if (NULL == cons) return NULL;
    if (is_null(cons)) return NIL;
    operator = CAR(cons);
    assert(SYMBOL == operator->atom_type);
    arg_list = CDR(cons);
    op = eval_expression(operator, env);
    assert(op != NULL);
    if (REGULAR == FUNC_TYPE(op)) {
	arg_list = eval_args(arg_list, env); /* You could not modify the original argument list */
    }
    if (MACRO == FUNC_TYPE(op)) {
	op->func_env = set_closure_env(op->func_env, arg_list);
	expand_value = eval_expression(FUNC_EXPR(op), op->func_env); /* The value of macro expanding */
	return eval_expression(expand_value, env);
    } else if (COMPILE == EXEC_TYPE(op)) {
	return (*FUNC_CODE(op))(env, arg_list);
    } else {
	op->func_env = set_closure_env(op->func_env, arg_list);
	/* print_env(op->func_env, FALSE); */
	/* print_object(FUNC_EXPR(op)); */
	return eval_expression(FUNC_EXPR(op), op->func_env);
    }
}

struct LispObject *eval_expression(struct LispObject *expression, ENVIRONMENT *env)
{
    if (ATOM == expression->type)
	return eval_atom(expression, env);
    else
	return eval_cons(expression, env);
}
