/*
 * atom_proc.c
 *
 * 
 *
 * Copyright (C) 2012-10-05 liutos
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cons.h"
#include "env_types.h"
#include "object.h"
#include "symbol_table.h"
#include "types.h"

Symbol lt_nil;
Symbol lt_t;

Symbol lt_block;
Symbol lt_catch;
Symbol lt_defvar;
Symbol lt_fset;
Symbol lt_function;
Symbol lt_if;
Symbol lt_lambda;
Symbol lt_progn;
Symbol lt_quote;
Symbol lt_return_from;
Symbol lt_set;
Symbol lt_throw;

inline Character make_char(char C_character)
{ return TO_CHAR(C_character); }

inline Fixnum make_fixnum(int C_integer)
{ return TO_FIXNUM(C_integer); }

inline Function make_function_t(void)
{ return TO_FUNCTION(malloc(sizeof(struct function_t))); }

Function make_C_function(primitive_t prim, int arity)
{
    Function fn = make_function_t();

    FUNCTION_CFLAG(fn) = TRUE;
    PRIMITIVE(fn) = prim;
    ARITY(fn) = arity;

    return fn;
}

Function make_Lisp_function(Cons parms, LispObject expr, Environment lenv, Environment denv, BlockEnvironment benv, Environment fenv)
{
    Function fn = make_function_t();

    ARITY(fn) = cons_length(parms);
    BLOCK_ENV(fn) = benv;
    FDEFINITION_ENV(fn) = fenv;
    FUNCTION_CFLAG(fn) = FALSE;
    EXPRESSION(fn) = expr;
    LEXICAL_ENV(fn) = lenv;
    PARAMETERS(fn) = parms;

    return fn;
}

String make_string(char *C_string)
{
    string_t object = malloc(sizeof(struct string_t));
    object->content = strdup(C_string);
    object->length = strlen(C_string);

    return TO_STRING(object);
}

inline BOOL is_true_obj(LispObject obj)
{ return lt_nil != obj; }

inline BOOL is_atom_object(LispObject object)
{ return !CONS_P(object); }

inline BOOL is_tail(LispObject object)
{ return lt_nil == object || is_atom_object(object); }

inline BOOL is_symbol(LispObject object)
{ return SYMBOL_P(object); }
