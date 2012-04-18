#include "model.h"
#include "print.h"
#include "types.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

struct LispObject *eval_atom(struct LispObject *atom_object,
			     ENVIRONMENT *env)
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
	env_tmp->value = CAR(arg_tmp);
	env_tmp = env_tmp->next;
	arg_tmp = CDR(arg_tmp);
    }

    return closure_env;
}

struct LispObject *eval_cons(struct LispObject *cons,
				   ENVIRONMENT *env)
{
    struct LispObject *eval_expression(struct LispObject *, ENVIRONMENT *);
    struct LispObject *arg_list, *op, *operator, *tmp;

    if (NULL == cons) return NULL;
    operator = CAR(cons);
    assert(SYMBOL == operator->atom_type);
    arg_list = CDR(cons);
    /* op = lookup_symbol_value(env, operator->name); */
    op = eval_expression(operator, env);
    if (NULL == op) {
	printf("There is not a corresponding function with symbol %s\n", operator->name);
	return NULL;
    }
    if (REGULAR == FUNC_TYPE(op)) {
	tmp = arg_list;
	while (tmp != NULL) {
	    CAR(tmp) = eval_expression(CAR(tmp), env);
	    tmp = CDR(tmp);
	}
    }
    if (MACRO == FUNC_TYPE(op))
	return eval_expression((*FUNC_CODE(op))(env, arg_list), env);
    else if (COMPILE == EXPR_TYPE(op))
	return (*FUNC_CODE(op))(env, arg_list);
    else {
	op->func_env = set_closure_env(op->func_env, arg_list);
	return eval_expression(FUNC_EXPR(op), op->func_env);
    }
}

struct LispObject *eval_expression(struct LispObject *expression,
				   ENVIRONMENT *env)
{
    if (ATOM == expression->type)
	return eval_atom(expression, env);
    else
	return eval_cons(expression, env);
}
