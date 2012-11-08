#ifndef PDECLS_H
#define PDECLS_H

#include "edecls.h"

#define ARGS args
#define ARG1 FIRST(args)
#define ARG2 SECOND(args)
#define ARG3 THIRD(args)

#define ACCESS_PARM2(o1, ac1, o2, ac2)          \
    do {                                        \
        o1 = ac1(ARG1);                         \
        o2 = ac2(ARG2);                         \
    } while (0)
#define PARM2(o1, o2)                           \
    do {                                        \
        o1 = ARG1;                              \
        o2 = ARG2;                              \
    } while(0)
#define PHEAD(fn_name)                          \
    LispObject fn_name(Cons args,               \
                       Environment lenv,        \
                       Environment denv,        \
                       BlockEnvironment benv,   \
                       Environment fenv)
#define RETURN(x) return (x)

#endif
