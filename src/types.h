#ifndef TYPES_H
#define TYPES_H

#include <stdio.h>

#include "decls.h"

enum bool_t { FALSE, TRUE};

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
typedef struct arity_t {
    int req_count;
    int opt_count;
    BOOL rest_flag;
    BOOL key_flag;
    int key_count;
    List keywords;
} *arity_t, *Arity;

typedef struct function_t {
    BOOL is_C_function;
    FunctionType type;
    Arity arity;
    union {
	primitive_t fptr;
	struct {
	    Cons parameters;
	    LispObject body;
	    Environment lexical_env;
            BlockEnvironment block_env;
            Environment fdefinition_env;
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

typedef struct hash_table_t {
    table_entry_t *elements;
    unsigned int (*hash_fn)(void *, unsigned int);
    BOOL (*compare_fn)(void *, void *);
    unsigned int size;
    unsigned int count;
} *hash_table_t;

/* Package definition */
typedef struct package_t {
    char *name;
    hash_table_t table;
} *package_t;

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
        struct {
            char *string;
            int position;
        } s;
    } u;
} *stream_t;

/* String definition */
typedef struct string_t {
    char *content;
    unsigned int length;
} *string_t;

/* Symbol definition */
typedef struct symbol_t {
    char *name;
    LispObject value;
    LispObject function;
    LispObject package;
} *symbol_t;

/* Multiple values cell definition */
typedef struct values_t {
    LispObject *values;
    int count;
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
    FLOAT,
    HASH_TABLE,
    PACKAGE,
    STREAM,
    VECTOR,
} LispType;

struct lisp_object_t {
    LispType type;
    union {
        float f;
        hash_table_t hash_table;
        package_t package;
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

/* Character */
/* TO_CHAR: char -> Character */
#define TO_CHAR(x) ((LispObject)(((int)(x) << 3) | CHARACTER_TAG))
#define theCHAR(x) ((int)(x) >> 3)
#define CHAR_P(x) (TAGOF(x) == CHARACTER_TAG)

/* Cons */
/* TO_CONS: cons_t -> Cons */
#define TO_CONS(x) ((LispObject)((int)(x) | CONS_TAG))
#define theCONS(x) ((cons_t)UNTAG(x))
#define CONS_P(x) (TAGOF(x) == CONS_TAG)

#define _CAR(x) (theCONS(x)->car)
#define _CDR(x) (theCONS(x)->cdr)
#define CAR(x) (lt_nil == (x) ? lt_nil: theCONS(x)->car)
#define CDR(x) (lt_nil == (x) ? lt_nil: theCONS(x)->cdr)
#define CDDR(x) CDR(CDR(x))
#define FIRST(x) CAR(x)
#define SECOND(x) CAR(CDR(x))
#define THIRD(x) CAR(CDDR(x))

/* Fixnum */
/* TO_FIXNUM: int -> Fixnum */
#define TO_FIXNUM(x) ((LispObject)(((int)(x) << 3) | FIXNUM_TAG))
#define theFIXNUM(x) ((int)(x) >> 3)
#define FIXNUM_P(x) (TAGOF(x) == FIXNUM_TAG)

/* Single floating point number */
#define theFLOAT(x) ((x)->u.f)

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
#define LEXICAL_ENV(x) (theFUNCTION(x)->u.s.lexical_env)
#define PARAMETERS(x) (theFUNCTION(x)->u.s.parameters)
#define PRIMITIVE(x) (theFUNCTION(x)->u.fptr)

#define SPECIAL_P(x) (SPECIAL == FTYPE(x))

/* Hash table */
#define theHASH_TABLE(x) ((x)->u.hash_table)
#define HASH_TABLE_P(x) (POINTER_P(x) && HASH_TABLE == (x)->type)

#define HASH_CMP(x) (theHASH_TABLE(x)->compare_fn)
#define TABLE_COUNT(x) (theHASH_TABLE(x)->count)
#define HASH_FN(x) (theHASH_TABLE(x)->hash_fn)
#define TABLE_ELEMENTS(x) (theHASH_TABLE(x)->elements)
#define TABLE_SIZE(x) (theHASH_TABLE(x)->size)

/* Package */
/* thePACKAGE: Package -> package_t */
#define thePACKAGE(x) ((x)->u.package)
#define PACKAGE_P(x) (POINTER_P(x) && PACKAGE == (x)->type)

#define PACKAGE_HASH_TABLE(x) (thePACKAGE(x)->table)
#define PACKAGE_NAME(x) (thePACKAGE(x)->name)

/* Stream */
#define theSTREAM(x) ((x)->u.stream)
#define STREAM_P(x) (POINTER_P(x) && STREAM == (x)->type)

#define STREAM_FILE(x) (theSTREAM(x)->u.file)

/* String */
/* TO_STRING: string_t -> String */
#define TO_STRING(x) ((LispObject)((int)(x) | STRING_TAG))
#define theSTRING(x) ((string_t)UNTAG(x))
#define STRING_P(x) (TAGOF(x) == STRING_TAG)

#define STRING_CONTENT(x) (theSTRING(x)->content)
#define STRING_LENGTH(x) (theSTRING(x)->length)

/* Symbol */
/* TO_SYMBOL: symbol_t -> Symbol */
#define TO_SYMBOL(x) ((LispObject)((int)(x) | SYMBOL_TAG))
#define theSYMBOL(x) ((symbol_t)UNTAG(x))
#define SYMBOL_P(x) (TAGOF(x) == SYMBOL_TAG)

#define SYMBOL_NAME(x) (theSYMBOL(x)->name)
#define SYMBOL_PACKAGE(x) (theSYMBOL(x)->package)
#define SYMBOL_VALUE(x) (theSYMBOL(x)->value)

/* Multiple values cell */
#define TO_VALUES(x) ((LispObject)((int)(x) | VALUES_TAG))
#define theVALUES(x) ((values_t)UNTAG(x))
#define VALUES_P(x) (TAGOF(x) == VALUES_TAG)

#define _VALUES(x) (theVALUES(x)->values)
#define PRIMARY_VALUE(x) (_VALUES(x)[0])

/* Vector */
#define theVECTOR(x) ((x)->u.vector)
#define VECTOR_P(x) (POINTER_P(x) && VECTOR == (x)->type)

#define ATOM_P(x) (!CONS_P(x))
#define TAIL_P(x) (!CONS_P(x))
#define eq(x, y) (x == y)

#endif
