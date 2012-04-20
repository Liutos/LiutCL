#include "model.h"
#include "primitives.h"
#include "print.h"
#include "types.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

struct LispObject *eval_expression(struct LispObject *, ENVIRONMENT *);
struct LispObject *eval_cons(struct LispObject *, ENVIRONMENT *);

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
    while (env_tmp != NULL && is_null(arg_tmp) == FALSE) {
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

int check_arg_num(struct LispObject *arg_list, int arg_num,
		  enum ARGC_TYPE argc_type)
{
    int counter;

    counter = 0;
    while (is_null(arg_list) == FALSE) {
	counter++;
	if (counter > arg_num) {
	    switch (argc_type) {
	    case FIXED: return counter - arg_num;
		break;
	    case VARIABLE:
		if (0 == arg_num)
		    return counter - arg_num;
		else
		    return 0;
		break;
	    }
	}
	arg_list = CDR(arg_list);
    }
    if (counter < arg_num)
	return counter - arg_num;
    else
	return 0;
}

struct LispObject *make_gensyms(int num, ENVIRONMENT *env)
{
    int i;
    struct LispObject *head, *curr, *prev, *sym;

    head = malloc(sizeof(struct LispObject));
    prev = head;
    for (i = 0; i < num; i++) {
	curr = malloc(sizeof(struct LispObject));
	curr->type = CONS;
	curr->atom_type = DO_NOT_MIND;
	sym = make_atom(gensym(), env);
	CAR(curr) = sym;
	CDR(curr) = NIL;
	CDR(prev) = curr;
	prev = curr;
    }

    return CDR(head);
}

struct LispObject *partial_apply(struct LispObject *expr,
				 int arg_num, ENVIRONMENT *env)
{
    struct LispObject *argv, *clz_expr;

    argv = make_gensyms(arg_num, env);
    /* print_object(argv); */
    clz_expr = with_symbol_cons("lambda",
				list_two_objects(argv,
						 append_cons(expr, argv)),
				env);
    /* print_object(clz_expr); */

    return eval_cons(clz_expr, env);
}

struct LispObject *eval_cons(struct LispObject *cons, ENVIRONMENT *env)
{
    int result;
    struct LispObject *arg_list, *op, *operator, *expand_value;

    if (NULL == cons) return NULL;
    if (is_null(cons)) return NIL;
    operator = CAR(cons);
    assert(SYMBOL == operator->atom_type);
    arg_list = CDR(cons);
    op = eval_expression(operator, env);
    assert(op != NULL);
    /* check_arg_num(arg_list, FUNC_ARGC(op), ARGC_TYPE(op)); */
    if (REGULAR == FUNC_TYPE(op)) {
	result = check_arg_num(arg_list, FUNC_ARGC(op), ARGC_TYPE(op));
	if (result > 0) {
	    printf("Too many arguments when calling function ");
	    print_object(op);
	    exit(1);
	}
	arg_list = eval_args(arg_list, env); /* When the function is a regular function, evaluates its arguments first. */
	if (result < 0) {
	    printf("Too few arguments. Partial applying function ");
	    print_object(op);

	    return partial_apply(cons, 0 - result, env);
	}
    }
    if (MACRO == FUNC_TYPE(op)) {
	op->func_env = set_closure_env(op->func_env, arg_list);
	expand_value = eval_expression(FUNC_EXPR(op), op->func_env); /* The value of macro expanding */
	return eval_expression(expand_value, env);
    } else if (COMPILE == EXEC_TYPE(op)) { /* If it is a primitive function defined by the interpreter... */
	return (*FUNC_CODE(op))(env, arg_list);
    } else {
	op->func_env = set_closure_env(op->func_env, arg_list);

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
