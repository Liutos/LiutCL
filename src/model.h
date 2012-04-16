#ifndef MODEL_H
#define MODEL_H

#include "types.h"

extern struct LispObject *make_object(char *, ENVIRONMENT *);
extern struct LookupEntry *lookup_symbol(ENVIRONMENT *, char *);
extern struct LispObject *lookup_symbol_fn(ENVIRONMENT *, char *);
extern void add_new_symbol(ENVIRONMENT *, char *, struct LispObject *);
extern struct LispObject *lookup_symbol_value(ENVIRONMENT *, char *);
extern struct LispObject *make_atom(char *, ENVIRONMENT *);

#endif
