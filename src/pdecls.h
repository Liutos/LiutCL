#ifndef PDECLS_H
#define PDECLS_H

#include "macro_def.h"

#define ARGS args
#define ARG1 FIRST(args)
#define ARG2 SECOND(args)
#define ARG3 THIRD(args)

#define PHEAD(fn_name)                          \
    LispObject fn_name(Cons args,               \
                       Environment lenv,        \
                       Environment denv,        \
                       Environment fenv,        \
                       BlockEnvironment benv,   \
                       GoEnvironment genv)
#define RETURN(x) return (x)

#endif
