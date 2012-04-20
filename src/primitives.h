#ifndef PRIMITIVES_H
#define PRIMITIVES_H

#include "types.h"

extern void init_primitives(ENVIRONMENT *);
extern BOOLEAN is_null(struct LispObject *);
extern struct LispObject *with_symbol_cons(const char*, struct LispObject *, ENVIRONMENT *);
extern char *gensym(void);
extern struct LispObject *append_cons(struct LispObject *, struct LispObject *);
extern struct LispObject *cons_two_objects(struct LispObject *, struct LispObject *);
extern struct LispObject *list_two_objects(struct LispObject *, struct LispObject *);

#endif
