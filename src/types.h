#ifndef TYPES_H
#define TYPES_H

enum TYPE {
    ATOM,
    CONS,
};

enum ATOM_TYPE {
    FUNCTION,
    LOOKUP_TABLE,
    SYMBOL,
};

struct LookupEntry {
    char *symbol_name;
    struct LispObject *symbol_object;
    struct LispObject *value;
    struct LispObject *function;
    struct LookupEntry *next;
};

typedef struct LispObject ENVIRONMENT;

struct LispObject {
    enum TYPE type;
    enum ATOM_TYPE atom_type;
    union {
	struct {
	    struct LispObject *car;
	    struct LispObject *cdr;
	};			/* CONS */
	struct LispObject *(*func_expr)(ENVIRONMENT *,
					struct LispObject *); /* FUNCTION */
	struct LookupEntry *head_node;			/* LOOKUP_TABLE */
	char *name;		/* SYMBOL */
    };
};

typedef int BOOLEAN;
#define TRUE 1
#define FALSE 0

#define CAR(cons_object) ((cons_object)->car)
#define CDR(cons_object) ((cons_object)->cdr)
#define PHEAD(func_name) struct LispObject *func_name(ENVIRONMENT *env, struct LispObject *arg_list)

typedef struct LispObject *(*PRIMITIVE)(ENVIRONMENT *, struct LispObject *);

#endif
