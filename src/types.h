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
typedef struct function_t {
    BOOL is_C_function;
    int arity;
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
    LispObject key;
    LispObject value;
    table_entry_t next;
};

typedef struct hash_table_t {
    table_entry_t *elements;
    unsigned int (*hash_fn)(LispObject, unsigned int);
    BOOL (*compare_fn)(LispObject, LispObject);
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

/* Vector definition */
typedef struct vector_t {
    unsigned int length;
    LispObject *content;
} *vector_t;

/* Lisp object definition */
typedef enum {
    CHARACTER,
    CONS,
    FUNCTION,
    FIXNUM,
    HASH_TABLE,
    PACKAGE,
    STREAM,
    STRING,
    SYMBOL,
    VECTOR,
} LispType;

struct lisp_object_t {
    LispType type;
    union {
        hash_table_t hash_table;
        package_t package;
        stream_t stream;
        vector_t vector;
    } u;
};

/* Type */
#define TYPE(x) (enum_type_of(x))
#define TAGOF(x) ((int)(x) & 7)
#define UNTAG(x) ((int)(x) & ~7)

#define POINTER_TAG 0
#define FIXNUM_TAG 1
#define CONS_TAG 2
#define SYMBOL_TAG 3
#define CHARACTER_TAG 4
#define FUNCTION_TAG 5
#define STRING_TAG 6

#define POINTER_P(x) (TAGOF(x) == POINTER_TAG)
#define thePOINTER(x) ((LispObject)UNTAG(x))

/* Character */
#define TO_CHAR(x) ((LispObject)(((int)(x) << 3) | CHARACTER_TAG))
#define theCHAR(x) ((int)(x) >> 3)
#define CHAR_P(x) (TAGOF(x) == CHARACTER_TAG)

/* Cons */
#define TO_CONS(x) ((LispObject)((int)(x) | CONS_TAG))
#define theCONS(x) ((cons_t)UNTAG(x))
#define CONS_P(x) (TAGOF(x) == CONS_TAG)

#define _CAR(x) (theCONS(x)->car)
#define _CDR(x) (theCONS(x)->cdr)
#define CAR(x) safe_car(x)
#define CDR(x) safe_cdr(x)
#define CDDR(x) CDR(CDR(x))
#define FIRST(x) CAR(x)
#define SECOND(x) CAR(CDR(x))
#define THIRD(x) CAR(CDDR(x))
#define FOURTH(x) CAR(CDR(CDDR(x)))

/* Fixnum */
#define TO_FIXNUM(x) ((LispObject)(((int)(x) << 3) | FIXNUM_TAG))
#define theFIXNUM(x) ((int)(x) >> 3)
#define FIXNUM_P(x) (TAGOF(x) == FIXNUM_TAG)

/* Function */
#define TO_FUNCTION(x) ((LispObject)((int)(x) | FUNCTION_TAG))
#define theFUNCTION(x) ((function_t)UNTAG(x))
#define FUNCTION_P(x) (TAGOF(x) == FUNCTION_TAG)

#define ARITY(x) (theFUNCTION(x)->arity)
#define BLOCK_ENV(x) (theFUNCTION(x)->u.s.block_env)
#define EXPRESSION(x) (theFUNCTION(x)->u.s.body)
#define FDEFINITION_ENV(x) (theFUNCTION(x)->u.s.fdefinition_env)
#define FUNCTION_CFLAG(x) (theFUNCTION(x)->is_C_function)
#define LEXICAL_ENV(x) (theFUNCTION(x)->u.s.lexical_env)
#define PARAMETERS(x) (theFUNCTION(x)->u.s.parameters)
#define PRIMITIVE(x) (theFUNCTION(x)->u.fptr)

/* Hash table */
#define TO_HASH_TABLE(x) ((HashTable)(x))
#define theHASH_TABLE(x) ((x)->u.hash_table)
#define HASH_TABLE_P(x) (POINTER_P(x) && HASH_TABLE == (x)->type)

#define HASH_CMP(x) (theHASH_TABLE(x)->compare_fn)
#define TABLE_COUNT(x) (theHASH_TABLE(x)->count)
#define HASH_FN(x) (theHASH_TABLE(x)->hash_fn)
#define TABLE_ELEMENTS(x) (theHASH_TABLE(x)->elements)
#define TABLE_SIZE(x) (theHASH_TABLE(x)->size)

/* Package */
#define TO_PACKAGE(x) ((Package)(x))
#define thePACKAGE(x) ((x)->u.package)
#define PACKAGE_P(x) (POINTER_P(x) && PACKAGE == (x)->type)

/* Stream */
#define theSTREAM(x) ((x)->u.stream)
#define STREAM_P(x) (POINTER_P(x) && STREAM == (x)->type)

#define STREAM_FILE(x) (theSTREAM(x)->u.file)

/* String */
#define TO_STRING(x) ((LispObject)((int)(x) | STRING_TAG))
#define theSTRING(x) ((string_t)UNTAG(x))
#define STRING_P(x) (TAGOF(x) == STRING_TAG)

#define STRING_CONTENT(x) (theSTRING(x)->content)
#define STRING_LENGTH(x) (theSTRING(x)->length)

/* Symbol */
#define TO_SYMBOL(x) ((LispObject)((int)(x) | SYMBOL_TAG))
#define theSYMBOL(x) ((symbol_t)UNTAG(x))
#define SYMBOL_P(x) (TAGOF(x) == SYMBOL_TAG)

#define SYMBOL_NAME(x) (theSYMBOL(x)->name)
#define SYMBOL_PACKAGE(x) (theSYMBOL(x)->package)

/* Vector */
#define theVector(x) ((x)->u.vector)
#define VECTOR_P(x) (POINTER_P(x) && VECTOR == (x)->type)

#endif
