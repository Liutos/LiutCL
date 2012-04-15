#include "model.h"
#include "types.h"

struct LispObject *eval_expression(struct LispObject *expression,
				   ENVIRONMENT *env)
{
    struct LispObject *arg_list, *op, *operator;

    operator = CAR(expression);
    arg_list = CDR(expression);
    op = lookup_symbol_fn(env, operator->name);

    return (*op->func_expr)(env, arg_list);
}
