#ifndef TYPES_H
#define TYPES_H

#include <gmp.h>
#include <setjmp.h>
#include <stdio.h>

#include "decls.h"

enum bool_t { FALSE, TRUE};

enum error_t {
    END_OF_FILE,
    MISSING_BLOCK_NAME,
    MISSING_CATCH_TAG,
    MISSING_GO_TAG,
    TOO_FEW_ARGUMENTS,
    TOO_MANY_ARGUMENTS,
    TYPE_ERROR,
    UNBOUND_VARIABLE,
    UNDEFINED_FUNCTION,
};

struct block_environment_t {
    Symbol name;
    jmp_buf context;
    BlockEnvironment prev;
};

struct environment_t {
    hash_table_t map;
    Environment next;
};

/* Place for storing arguments. */
typedef struct frame_t {
    size_t quantity;            /* The length of `rargs'. */
    LispObject *rargs;         /* Required and optional arguments in array form. */
    List kws;           /* Keyword arguments in a-list form, order the same as making the corresponding lambda list. */
    List rest;           /* Rest arguments. */
} *frame_t;

struct go_environment_t {
    List tags;
    jmp_buf context;
    GoEnvironment prev;
};

/* Cons definition */
typedef struct cons_t {
    LispObject car;
    LispObject cdr;
} *cons_t;

/* Function definition */
typedef enum {
    MACRO,
    REGULAR,
    SPECIAL,
} FunctionType;

/* typedef int Arity; */
struct arity_t {
    int req_count;
    int opt_count;
    BOOL rest_flag;
    BOOL key_flag;              /* This seems duplicated. */
    int key_count;
    List keywords;
};

typedef struct function_t {
    BOOL is_C_function;
    FunctionType type;
    arity_t arity;
    union {
	primitive_t fptr;
        primitive_t specform;
	struct {
	    Cons parameters;
	    LispObject body;
	    Environment lexical_env;
            Environment fdefinition_env;
            BlockEnvironment block_env;
            GoEnvironment go_env;
	} s;
    } u;
} *function_t;

/* Hash table definition */
typedef struct table_entry_t *table_entry_t;
struct table_entry_t {
    void *key;
    void *value;
    table_entry_t next;
};

typedef struct hash_table {
    table_entry_t *elements;
    Fixnum (*hash_fn)(LispObject *, unsigned int);
    Boolean (*compare_fn)(LispObject *, LispObject *);
    unsigned int size;
    unsigned int count;
} *hash_table;

/* For inner use. */
struct hash_table_t {
    table_entry_t *elements;
    unsigned int (*hash_fn)(void *, unsigned int);
    BOOL (*compare_fn)(void *, void *);
    unsigned int size;
    unsigned int count;         /* The quantity of valid objects. */
};

/* Package definition */
typedef struct package_t {
    char *name;
    hash_table_t table;
} *package_t;

/* Ratio definition */
typedef struct ratio_t {
    Integer numerator;
    Integer denominator;
} *ratio_t;

/* Stream definition */
typedef enum {
    FILE_STREAM,
    CHARACTER_STREAM,
    BYTE_STREAM,
} STREAM_TYPE;

typedef enum {
    READ,
    WRITE,
    RW,
} MODE;

typedef struct stream_t {
    STREAM_TYPE type;
    MODE mode;
    union {
        FILE *file;
        struct {
            char *string;
            int position;
        } s;
    } u;
} *stream_t;

/* String definition */
typedef struct string_t {
    char *content;
    unsigned int length;        /* The number of contained characters. */
    unsigned int size;          /* Total space size */
} *string_t;

/* Symbol definition */
typedef struct symbol_t {
    char *name;
    LispObject package;
    List property_list;
    LispObject value;
    LispObject function;
} *symbol_t;

/* Multiple values cell definition */
typedef struct values_t {
    LispObject *objs;         /* Objects in array form. */
    size_t count;
} *values_t;

/* Vector definition */
typedef struct vector_t {
    unsigned int length;
    LispObject *content;
} *vector_t;

/* Lisp object definition */
typedef enum {
    /* Tagged pointer */
    CHARACTER = 1,
    CONS,
    FIXNUM,
    FUNCTION,
    STRING,
    SYMBOL,
    VALUES,
    /* Tagged union */
    BIGNUM,
    PACKAGE,
    RATIO,
    STREAM,
    VECTOR,
} LispType;

struct lisp_object_t {
    LispType type;
    union {
        mpz_t bi;
        package_t package;
        ratio_t ratio;
        stream_t stream;
        vector_t vector;
    } u;
};

/* Type */
#define TAGOF(x) ((int)(x) & 7)
#define UNTAG(x) ((int)(x) & ~7)

#define POINTER_TAG 0
#define CHARACTER_TAG 1
#define CONS_TAG 2
#define FIXNUM_TAG 3
#define FUNCTION_TAG 4
#define STRING_TAG 5
#define SYMBOL_TAG 6
#define VALUES_TAG 7

#define POINTER_P(x) (TAGOF(x) == POINTER_TAG)
#define thePOINTER(x) ((LispObject)UNTAG(x))

/* Bignum */
/* theBIGNUM: Bignum -> mpz_t */
#define theBIGNUM(x) ((x)->u.bi)
#define BIGNUM_P(x) (POINTER_P(x) && BIGNUM == (x)->type)

/* Character */
/* TO_CHAR: char -> Character */
#define TO_CHAR(x) ((LispObject)(((int)(x) << 3) | CHARACTER_TAG))
/* theCHAR: Character -> char */
#define theCHAR(x) ((int)(x) >> 3)
#define CHAR_P(x) (TAGOF(x) == CHARACTER_TAG)

/* Cons */
/* TO_CONS: cons_t -> Cons */
#define TO_CONS(x) ((LispObject)((int)(x) | CONS_TAG))
#define theCONS(x) ((cons_t)UNTAG(x))
#define CONS_P(x) (TAGOF(x) == CONS_TAG)

/* #define _CAR(x) (theCONS(x)->car) */
/* #define _CDR(x) (theCONS(x)->cdr) */
#define CAR(x) (lt_nil == (x) ? lt_nil: theCONS(x)->car)
#define CDR(x) (lt_nil == (x) ? lt_nil: theCONS(x)->cdr)
#define CDDR(x) CDR(CDR(x))
/* #define FIRST(x) CAR(x) */
/* #define SECOND(x) CAR(CDR(x)) */
/* #define THIRD(x) CAR(CDDR(x)) */

/* Fixnum */
/* TO_FIXNUM: int -> Fixnum */
#define TO_FIXNUM(x) ((LispObject)(((int)(x) << 3) | FIXNUM_TAG))
#define theFIXNUM(x) ((int)(x) >> 3)
#define FIXNUM_P(x) (TAGOF(x) == FIXNUM_TAG)

#define MAX_FIXNUM 536870911
#define MIN_FIXNUM -536870912

/* Function */
/* TO_FUNCTION: function_t -> Function */
#define TO_FUNCTION(x) ((LispObject)((int)(x) | FUNCTION_TAG))
#define theFUNCTION(x) ((function_t)UNTAG(x))
#define FUNCTION_P(x) (TAGOF(x) == FUNCTION_TAG)

#define ARITY(x) (theFUNCTION(x)->arity)
#define BLOCK_ENV(x) (theFUNCTION(x)->u.s.block_env)
#define EXPRESSION(x) (theFUNCTION(x)->u.s.body)
#define FDEFINITION_ENV(x) (theFUNCTION(x)->u.s.fdefinition_env)
#define FTYPE(x) (theFUNCTION(x)->type)
#define FUNCTION_CFLAG(x) (theFUNCTION(x)->is_C_function)
#define GO_ENV(x) (theFUNCTION(x)->u.s.go_env)
#define LEXICAL_ENV(x) (theFUNCTION(x)->u.s.lexical_env)
#define PARAMETERS(x) (theFUNCTION(x)->u.s.parameters)
#define PRIMITIVE(x) (theFUNCTION(x)->u.fptr)

#define SPECIAL_P(x) (SPECIAL == FTYPE(x))

/* Hash table */
/* #define theHASH_TABLE(x) ((x)->u.hash_table) */
/* #define HASH_TABLE_P(x) (POINTER_P(x) && HASH_TABLE == (x)->type) */

/* #define HASH_CMP(x) (theHASH_TABLE(x)->compare_fn) */
/* #define TABLE_COUNT(x) (theHASH_TABLE(x)->count) */
/* #define HASH_FN(x) (theHASH_TABLE(x)->hash_fn) */
/* #define TABLE_ELEMENTS(x) (theHASH_TABLE(x)->elements) */
/* #define TABLE_SIZE(x) (theHASH_TABLE(x)->size) */

/* Package */
/* thePACKAGE: Package -> package_t */
#define thePACKAGE(x) ((x)->u.package)
#define PACKAGE_P(x) (POINTER_P(x) && PACKAGE == (x)->type)

#define PACKAGE_HASH_TABLE(x) (thePACKAGE(x)->table)
#define PACKAGE_NAME(x) (thePACKAGE(x)->name)

/* Rational */
#define theRATIO(x) ((x)->u.ratio)

#define DENOMINATOR(x) (theRATIO(x)->denominator)
#define NUMERATOR(x) (theRATIO(x)->numerator)

/* Stream */
#define theSTREAM(x) ((x)->u.stream)
#define STREAM_P(x) (POINTER_P(x) && STREAM == (x)->type)

#define STREAM_FILE(x) (theSTREAM(x)->u.file)

/* String */
/* TO_STRING: string_t -> String */
#define TO_STRING(x) ((LispObject)((int)(x) | STRING_TAG))
/* theSTRING: String -> string_t */
#define theSTRING(x) ((string_t)UNTAG(x))
#define STRING_P(x) (TAGOF(x) == STRING_TAG)

#define STRING_CONTENT(x) (theSTRING(x)->content)
#define STRING_LENGTH(x) (theSTRING(x)->length)
#define STRING_SIZE(x) (theSTRING(x)->size)

/* Symbol */
/* TO_SYMBOL: symbol_t -> Symbol */
#define TO_SYMBOL(x) ((LispObject)((int)(x) | SYMBOL_TAG))
#define theSYMBOL(x) ((symbol_t)UNTAG(x))
#define SYMBOL_P(x) (TAGOF(x) == SYMBOL_TAG)

#define SYMBOL_FUNCTION(x) (theSYMBOL(x)->function)
#define SYMBOL_NAME(x) (theSYMBOL(x)->name)
#define SYMBOL_PACKAGE(x) (theSYMBOL(x)->package)
#define SYMBOL_VALUE(x) (theSYMBOL(x)->value)

/* Multiple values cell */
/* TO_VALUES: values_t -> Values */
#define TO_VALUES(x) ((LispObject)((int)(x) | VALUES_TAG))
/* theVALUES: Values -> values_t */
#define theVALUES(x) ((values_t)UNTAG(x))
#define VALUES_P(x) (TAGOF(x) == VALUES_TAG)

#define _VALUES(x) (theVALUES(x)->objs)
#define NO_VALUES_P(x) (theVALUES(x)->count == 0)
#define PRIMARY_VALUE(x) (_VALUES(x)[0])
#define SINGLE_VALUES_P(x) (theVALUES(x)->count == 1)

/* Vector */
#define theVECTOR(x) ((x)->u.vector)
#define VECTOR_P(x) (POINTER_P(x) && VECTOR == (x)->type)

#define ATOM_P(x) (!CONS_P(x))
#define INTEGER_P(x) (BIGNUM_P(x) && FIXNUM_P(x))
#define NUMBER_P(x) (INTEGER_P(x))
#define TAIL_P(x) (!CONS_P(x))
#define eq(x, y) (x == y)

#define B(x) ((x)? lt_t: lt_nil)

#endif
