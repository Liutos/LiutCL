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

/* Function definition. */
typedef struct function_t {
    BOOL is_c_func;
    union {
	primitive_t prim;
	struct {
	    struct LispObject *parms;
	    struct LispObject *expr;
	    struct Environment *env; /* Lexical Bindings */
	    struct Environment *denv; /* Dynamic Bindings */
            struct block_environment *block_env; /* Block Environment */
            struct Environment *fenv;            /* Function definitions. */
	};
    };
} *function_t;                  /* The inner representation of the
                                   function type in Lisp level. */
/* Symbol definition */
struct symbol_t;
typedef struct symbol_t *symbol_t;
struct symbol_t {
    char *symbol_name;
    LispObject value_cell;
    LispObject function_cell;
};

/* Stream definition */
typedef enum {
    FILE_STREAM,
    CHARACTER_STREAM,
    BYTE_STREAM,
} STREAM_TYPE;

struct stream_t;
typedef struct stream_t *stream_t;
typedef stream_t file_stream;
typedef stream_t char_stream;
typedef stream_t byte_stream;
struct stream_t {
    STREAM_TYPE type;
    union {
        FILE *file;
        char *string;
    } u;
};

/* Vector definition */
struct vector_t;
typedef struct vector_t *vector_t;
struct vector_t {
    unsigned int length;
    LispObject *content;        /* A C array. */
};

/* Lisp Object definition */
struct LispObject {
    LispType type;
    union {
	struct symbol_t *symbol;
	int integer;
	function_t fun;
        char *string;
        struct stream_t *stream;
	struct {
	    LispObject car;
	    LispObject cdr;
	} s;
        char c;                 /* The character */
        struct vector_t *vector;
    } u;
};

/* Object */
#define TYPE(x) ((x)->type)
/* Cons Object */
#define CAR(x) ((x)->u.s.car)
#define CDR(x) ((x)->u.s.cdr)
#define SCAR(x) safe_car(x)     /* A safe version of macro CAR */
#define SCDR(x) safe_cdr(x)     /* A safe version of macro CDR */
#define FIRST(x) CAR(x)
#define SECOND(x) CAR(SCDR(x))
#define CDDR(x) SCDR(SCDR(x))
#define THIRD(x) SCAR(CDDR(x))
#define FOURTH(x) SCAR(SCDR(CDDR(x)))
/* Integer Object */
#define INTEGER(x) ((x)->u.integer)
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
#define theSYMBOL(x) ((x)->u.symbol)
#define SYMBOL_NAME(x) (theSYMBOL(x)->symbol_name)
/* String Object */
#define STRING(x) ((x)->u.string)
/* Character Object */
#define CHARACTER(x) ((x)->u.c)
/* Stream Object */
#define theSTREAM(x) ((x)->u.stream)
#define FILE_STREAM(x) ((x)->u.file)
/* Vector Object */
#define theVector(x) ((x)->u.vector)

#define PHEAD(func_name) LispObject func_name(Cons args)

extern BOOL is_debug_mode;      /* Not used yet */

#endif
