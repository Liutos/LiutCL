#ifndef TYPES_H
#define TYPES_H

#include <stdio.h>

typedef enum {
    CONS,                       /* Everything except CONS is of type ATOM. */
    SYMBOL,
    INTEGER,
    FUNCTION,
    STRING,
    STREAM,
    CHARACTER,
    VECTOR,
} LispType;

struct LispObject;
typedef struct LispObject *LispObject;
/* Alias. For readbility. */
typedef LispObject Cons;
typedef LispObject Atom;
typedef LispObject Symbol;
typedef LispObject Boolean;
typedef LispObject Function;
typedef LispObject Stream;
typedef LispObject Character;
typedef LispObject Vector;
typedef LispObject Integer;
typedef LispObject String;      /* 只是为了快速试验一下向文件流的写入而设的 */
typedef LispObject (*primitive_t)(LispObject);

typedef int BOOL;
#define TRUE 1
#define FALSE 0

/* Cons definition */
typedef struct cons_t {
    LispObject car;
    LispObject cdr;
} *cons_t;

/* Function definition. */
typedef struct function_t {
    BOOL is_c_func;
    union {
	primitive_t prim;
	struct {
	    LispObject parms;
	    LispObject expr;
	    struct Environment *env; /* Lexical Bindings */
	    struct Environment *denv; /* Dynamic Bindings */
            struct block_environment *block_env; /* Block Environment */
            struct Environment *fenv;            /* Function definitions. */
	};
    };
} *function_t;                  /* The inner representation of the
                                   function type in Lisp level. */
/* Symbol definition */
typedef struct symbol_t {
    char *symbol_name;
    LispObject value_cell;
    LispObject function_cell;
} *symbol_t;

/* Stream definition */
typedef enum {
    FILE_STREAM,
    CHARACTER_STREAM,
    BYTE_STREAM,
} STREAM_TYPE;

typedef struct stream_t {
    STREAM_TYPE type;
    union {
        FILE *file;
        char *string;
    } u;
} *stream_t;

/* Vector definition */
struct vector_t;
typedef struct vector_t *vector_t;
struct vector_t {
    unsigned int length;
    LispObject *content;        /* A C array. */
};

/* Lisp Object definition. Maybe this could be called as `tagged union' */
struct LispObject {
    LispType type;
    union {
	function_t fun;
        char *string;
        struct stream_t *stream;
        struct vector_t *vector;
    } u;
};

/* Object */
#define TYPE(x) (enum_type_of(x))
#define TYPE_TAG(x) ((int)(x) & 7) /* Used for tagged pointer. */
#define UNTAG(x) ((int)(x) & ~7)

#define POINTER_TAG 0
#define INTEGER_TAG 1
#define CONS_TAG 2
#define SYMBOL_TAG 3
#define CHARACTER_TAG 4

#define POINTER_P(x) (TYPE_TAG(x) == POINTER_TAG)
#define INTEGER_P(x) (TYPE_TAG(x) == INTEGER_TAG)
#define CONS_P(x) (TYPE_TAG(x) == CONS_TAG)
#define SYMBOL_P(x) (TYPE_TAG(x) == SYMBOL_TAG)
#define CHARACTER_P(x) (TYPE_TAG(x) == CHARACTER_TAG)
/* Cons Object */
#define MAKE_CONS(x) ((LispObject)((int)(x) | CONS_TAG))
#define GET_CONS(x) ((cons_t)UNTAG(x))

#define CAR(x) (GET_CONS(x)->car)
#define CDR(x) (GET_CONS(x)->cdr)
#define SCAR(x) safe_car(x)     /* A safe version of macro CAR */
#define SCDR(x) safe_cdr(x)     /* A safe version of macro CDR */
#define FIRST(x) CAR(x)
#define SECOND(x) CAR(SCDR(x))
#define CDDR(x) SCDR(SCDR(x))
#define THIRD(x) SCAR(CDDR(x))
#define FOURTH(x) SCAR(SCDR(CDDR(x)))

/* Integer Object */
#define INTEGER(x) ((int)(x) >> 3)
#define MAKE_INTEGER(x) ((LispObject)(((int)(x) << 3) | INTEGER_TAG))

/* Function Object */
#define FUNCTION(x) ((x)->u.fun)
#define LOCAL_ENV(x) (FUNCTION(x)->env)
#define PRIMITIVE(x) (FUNCTION(x)->prim)
#define FUNC_DENV(x) (FUNCTION(x)->denv)
#define FUNC_FLAG(x) (FUNCTION(x)->is_c_func)
#define BLOCK_ENV(x) (FUNCTION(x)->block_env)
#define EXPRESSION(x) (FUNCTION(x)->expr)
#define PARAMETERS(x) (FUNCTION(x)->parms)
#define FENV(x) (FUNCTION(x)->fenv)

/* Symbol Object */
#define MAKE_SYMBOL(x) ((LispObject)((int)(x) | SYMBOL_TAG))
#define theSYMBOL(x) ((symbol_t)UNTAG(x))
#define SYMBOL_NAME(x) (theSYMBOL(x)->symbol_name)

/* String Object */
#define STRING(x) ((x)->u.string)

/* Character Object */
#define MAKE_CHARACTER(x) ((LispObject)(((int)(x) << 3) | CHARACTER_TAG))
#define CHARACTER(x) ((int)(x) >> 3)

/* Stream Object */
#define theSTREAM(x) ((x)->u.stream)
#define FILE_STREAM(x) ((x)->u.file)

/* Vector Object */
#define theVector(x) ((x)->u.vector)

#define PHEAD(func_name) LispObject func_name(Cons args)

#endif
