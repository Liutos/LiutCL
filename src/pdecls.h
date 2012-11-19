#ifndef PDECLS_H
#define PDECLS_H

#include "cons.h"
#include "macro_def.h"

#define ARGS (args->rargs)
#define ARG(n) ARGS[(n) - 1]
#define ARG1 (ARGS[0])
#define ARG2 (ARGS[1])
#define ARG3 (ARGS[2])
#define RK (args->rest)
#define KWS (args->kws)

#define KV(name) get_by_keyword(name, KWS)
/* Function header line of primitive. */
#define PHEAD(fn_name)                          \
    LispObject fn_name(Frame args,               \
                       Environment lenv,        \
                       Environment denv,        \
                       Environment fenv,        \
                       BlockEnvironment benv,   \
                       GoEnvironment genv)
#define RETURN(x) return (x)
#define VOI(var, name, init)                    \
    do {                                        \
        if (is_unbound(var = KV(name)))         \
            var = init;                         \
    } while (0)

#endif
