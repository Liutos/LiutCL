#ifndef PDECLS_H
#define PDECLS_H

#define PARM2(o1, o2)                           \
    do {                                        \
        o1 = FIRST(args);                       \
        o2 = SECOND(args);                      \
    } while(0)
#define ACCESS_PARM2(o1, ac1, o2, ac2)          \
    do {                                        \
        o1 = ac1(FIRST(args));                  \
        o2 = ac2(SECOND(args));                 \
    } while (0)

#ifndef FS

#define PHEAD(fn_name) LispObject fn_name(Cons args)

#else

#define PHEAD(fn_name) LispObject fn_name(Cons args, Environment lenv, Environment denv, BlockEnvironment benv, Environment fenv)

#endif

#endif
