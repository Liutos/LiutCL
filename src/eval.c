#include "model.h"
#include "print.h"
#include "types.h"

#include <assert.h>
#include <stdio.h>

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

    env_tmp = closure_env->head_node->next;
    arg_tmp = arg_list;
    while (env_tmp != NULL) {
	env_tmp->value = CAR(arg_list);
	env_tmp = env_tmp->next;
	arg_tmp = CDR(arg_list);
    }

    return closure_env;
}

struct LispObject *eval_cons(struct LispObject *cons,
				   ENVIRONMENT *env)
{
    struct LispObject *eval_expression(struct LispObject *, ENVIRONMENT *);
    struct LispObject *arg_list, *op, *operator, *tmp;

    operator = CAR(cons);
    assert(SYMBOL == operator->atom_type);
    arg_list = CDR(cons);
    op = lookup_symbol_fn(env, operator->name);
    if (REGULAR == FUNC_TYPE(op)) {
	tmp = arg_list;
	while (tmp != NULL) {
	    /* CAR(tmp) = (ATOM == CAR(tmp)->type ? */
	    /* 		eval_atom(CAR(tmp), env) : */
	    /* 		eval_cons(CAR(tmp), env)); /\* Avoiding the declaration of function eval_expression() below. *\/ */
	    CAR(tmp) = eval_expression(CAR(tmp), env);
	    tmp = CDR(tmp);
	}
    }
    if (MACRO == FUNC_TYPE(op))
	return eval_expression((*FUNC_CODE(op))(env, arg_list), env);
    else if (COMPILE == EXPR_TYPE(op))
	return (*FUNC_CODE(op))(env, arg_list);
    else {
	print_object(FUNC_EXPR(op));
	return eval_expression(FUNC_EXPR(op),
			       set_closure_env(op->func_env, arg_list)); /* If something wrong, it must be occured here! */
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
