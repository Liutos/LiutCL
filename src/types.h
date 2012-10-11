#ifndef TYPES_H
#define TYPES_H

typedef enum {
    CONS,
    SYMBOL,
    INTEGER,
    FUNCTION,
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
} *function_t;
typedef struct LispObject {
    LispType type;
    union {
	char *symbol_name;
	int integer;
	/* struct { */
	/*     BOOL is_c_func;	/\* Use the 'prim' member variable in the nested */
	/* 			   union if this is true. *\/ */
	/*     union { */
	/* 	primitive_t prim; */
	/* 	struct { */
	/* 	    struct LispObject *parms; */
	/* 	    struct LispObject *expr; */
	/* 	    struct Environment *env; */
	/* 	}; */
	/*     }; */
	/* }; */
	function_t fun;
	struct {
	    struct LispObject *car;
	    struct LispObject *cdr;
	};
    };
} *LispObject;

typedef LispObject Cons;
typedef LispObject Atom;
typedef LispObject Symbol;
typedef LispObject Function;
typedef LispObject Boolean;

#define CAR(x) ((x)->car)
#define CDR(x) ((x)->cdr)
#define SCDR(x) safe_cdr(x)
#define TYPE(x) ((x)->type)
#define INTEGER(x) ((x)->integer)
#define FUNCTION(x) ((x)->fun)
#define PRIMITIVE(x) (FUNCTION(x)->prim)
#define EXPRESSION(x) ((x)->fun->expr)
#define PARAMETERS(x) ((x)->fun->parms)
#define LOCAL_ENV(x) ((x)->fun->env)
#define FUNC_FLAG(x) ((x)->fun->is_c_func)
#define FUNC_DENV(x) (FUNCTION(x)->denv)

typedef struct StrSymMap {
    char *symbol_name;
    Symbol symbol;
    struct StrSymMap *next;
} *SymbolTable;

typedef struct SymValMap {
    Symbol symbol;
    LispObject value;
    struct SymValMap *next;
} *SymValMap;			/* Kepps the mapping between symbol and
				   value. */
typedef struct Environment {
    SymValMap map;
    struct Environment *next_env;
} *Environment;			/* Keeps a series of mapping described above. */

#define PHEAD(func_name) LispObject func_name(Cons args)

#define FIRST(x) CAR(x)
#define SECOND(x) CAR(CDR(x))
#define CDDR(x) CDR(CDR(x))
#define THIRD(x) CAR(CDDR(x))
#define FOURTH(x) CAR(CDR(CDDR(x)))

#endif
