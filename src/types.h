#ifndef TYPES_H
#define TYPES_H

typedef enum {
    CONS,                       /* Everything except CONS is of type ATOM. */
    SYMBOL,
    INTEGER,
    FUNCTION,
    STRING,
} LispType;

typedef struct LispObject *(*primitive_t)(struct LispObject *);

typedef int BOOL;
#define TRUE 1
#define FALSE 0

typedef struct function_t {
    BOOL is_c_func;
    union {
	primitive_t prim;
	struct {
	    struct LispObject *parms;
	    struct LispObject *expr;
	    struct Environment *env;
	    struct Environment *denv;
	};
    };
} *function_t;                  /* The inner representation of the
                                   function type in Lisp level. */
typedef struct LispObject {
    LispType type;
    union {
	char *symbol_name;
	int integer;
	function_t fun;
        char *string;
	struct {
	    struct LispObject *car;
	    struct LispObject *cdr;
	};
    };
} *LispObject;

/* Alias. For readbility. */
typedef LispObject Cons;
typedef LispObject Atom;
typedef LispObject Symbol;
typedef LispObject Function;
typedef LispObject Boolean;

#define CAR(x) ((x)->car)
#define CDR(x) ((x)->cdr)
#define SCAR(x) safe_car(x)     /* A safe version of macro CAR */
#define SCDR(x) safe_cdr(x)     /* A safe version of macro CDR */
#define TYPE(x) ((x)->type)
#define INTEGER(x) ((x)->integer)
#define FUNCTION(x) ((x)->fun)
#define PRIMITIVE(x) (FUNCTION(x)->prim)
#define EXPRESSION(x) ((x)->fun->expr)
#define PARAMETERS(x) ((x)->fun->parms)
#define LOCAL_ENV(x) ((x)->fun->env)
#define FUNC_FLAG(x) ((x)->fun->is_c_func)
#define FUNC_DENV(x) (FUNCTION(x)->denv)

#define PHEAD(func_name) LispObject func_name(Cons args)

#define FIRST(x) CAR(x)
#define SECOND(x) CAR(SCDR(x))
#define CDDR(x) SCDR(SCDR(x))
#define THIRD(x) SCAR(CDDR(x))
#define FOURTH(x) SCAR(SCDR(CDDR(x)))

extern BOOL is_debug_mode;

#endif
