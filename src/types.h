#ifndef TYPES_H
#define TYPES_H

enum TYPE {
    ATOM,
    CONS,
};

enum ATOM_TYPE {
    FUNCTION,
    INTEGER,
    LOOKUP_TABLE,
    STRING,
    SYMBOL,
};

enum FUNC_TYPE {
    REGULAR,
    SPECIAL,
    MACRO,
};

enum EXPR_TYPE {
    COMPILE,
    INTERPRET,
};

struct LookupEntry {
    char *symbol_name;
    struct LispObject *symbol_object;
    struct LispObject *value;
    /* struct LispObject *function; /\* The variable and function will own the same namespace so this slot is obsolete *\/ */
    struct LookupEntry *next;
};

#define ENTRY_VALUE(entry) ((entry)->value)

typedef struct LispObject ENVIRONMENT;
typedef struct LispObject *(*PRIMITIVE)(ENVIRONMENT *, struct LispObject *);

struct LispObject {
    enum TYPE type;
    enum ATOM_TYPE atom_type;
    union {
	struct {
	    struct LispObject *car;
	    struct LispObject *cdr;
	};			/* CONS */
	int integer;		/* INTEGER */
	struct {
	    enum EXPR_TYPE expr_type;
	    enum FUNC_TYPE func_type;
	    int arg_num;	       /* The number of arguments of a function and now it's useless. */
	    PRIMITIVE func_code;
	    struct LispObject *func_expr;
	    ENVIRONMENT *func_env;
	};      /* FUNCTION */
	struct {
	    struct LookupEntry *head_node;			/* LOOKUP_TABLE */
	    ENVIRONMENT *next_env;				/* Pointer to the outer environment */
	};
	char *string;
	char *name;		/* SYMBOL */
    };
};

typedef int BOOLEAN;
#define TRUE 1
#define FALSE 0

#define CAR(cons_object) ((cons_object)->car)
#define CDR(cons_object) ((cons_object)->cdr)
#define CADR(cons_object) ((cons_object)->cdr->car)
#define CADDR(cons_object) ((cons_object)->cdr->cdr->car)
/* The macros above is designed for extracting the function argument when calling */

#define FUNC_CODE(function_object) ((function_object)->func_code)
#define FUNC_EXPR(function_object) ((function_object)->func_expr)
#define FUNC_TYPE(function_object) ((function_object)->func_type)
#define FUNC_ARGC(function_object) ((function_object)->arg_num)
#define EXPR_TYPE(function_object) ((function_object)->expr_type)

#define INTEGER(atom_object) ((atom_object)->integer)
#define STRING(atom_object) ((atom_object)->string)

#define SYMBOL_NAME(symbol_object) ((symbol_object)->name)

#define PHEAD(func_name) struct LispObject *func_name(ENVIRONMENT *env, struct LispObject *arg_list)

#endif
