#ifndef PDECLS_H
#define PDECLS_H

#include "macro_def.h"

#define ARGS (args->rargs)
#define ARG1 (ARGS[0])
#define ARG2 (ARGS[1])
#define ARG3 (ARGS[2])
#define RK (args->rest_or_kws)

/* Function header line of primitive. */
#define PHEAD(fn_name)                          \
    LispObject fn_name(Frame args,               \
                       Environment lenv,        \
                       Environment denv,        \
                       Environment fenv,        \
                       BlockEnvironment benv,   \
                       GoEnvironment genv)
#define RETURN(x) return (x)

#endif
