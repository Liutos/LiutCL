#ifndef TYPES_H
#define TYPES_H

typedef enum {
    CONS,
    SYMBOL,
    INTEGER,
    FUNCTION,
    BOOLEAN,
} LispType;

typedef struct LispObject *(*primitive_t)(struct LispObject *);

typedef int BOOL;
#define TRUE 1
#define FALSE 0

typedef struct LispObject {
    LispType type;
    union {
	char *symbol_name;
	int integer;
	struct {
	    BOOL is_c_func;	/* Use the 'prim' member variable in the nested
				   union if this is true. */
	    union {
		primitive_t prim;
		struct {
		    struct LispObject *parms;
		    struct LispObject *expr;
		    struct SymValMap *env;
		};
	    };
	};
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
#define TYPE(x) ((x)->type)
#define INTEGER(x) ((x)->integer)
#define PRIMITIVE(x) ((x)->prim)
#define EXPRESSION(x) ((x)->expr)
#define PARAMETERS(x) ((x)->parms)
#define LOCAL_ENV(x) ((x)->env)
#define FUNC_FLAG(x) ((x)->is_c_func)

typedef struct StrSymMap {
    char *symbol_name;
    Symbol symbol;
    struct StrSymMap *next;
} *SymbolTable;

typedef struct SymValMap {
    Symbol symbol;
    LispObject value;
    struct SymValMap *next;
} *Environment;

#define PHEAD(func_name) LispObject func_name(Cons args)

#define FIRST(x) CAR(x)
#define SECOND(x) CAR(CDR(x))
#define CDDR(x) CDR(CDR(x))
#define THIRD(x) CAR(CDDR(x))
#define FOURTH(x) CAR(CDR(CDDR(x)))

#endif
