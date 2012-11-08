#ifndef EDECLS_H
#define EDECLS_H

#define CALL_EVAL(eval_fn, arg) eval_fn(arg, lenv, denv, benv, fenv)
#define DEFEVAL(name, hd)                       \
    LispObject name(LispObject hd,              \
                    Environment lenv,           \
                    Environment denv,           \
                    BlockEnvironment benv,      \
                    Environment fenv)

#endif
