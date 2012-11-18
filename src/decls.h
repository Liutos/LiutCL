#ifndef DECLS_H
#define DECLS_H

typedef enum bool_t BOOL;
typedef struct arity_t *arity_t, *Arity;
typedef struct block_environment_t *BlockEnvironment;
typedef struct environment_t *Environment;
typedef struct frame_t *Frame;
typedef struct go_environment_t *GoEnvironment;
typedef struct hash_table_t *hash_table_t;
typedef struct lisp_object_t *LispObject;

typedef LispObject Atom;
typedef LispObject Cons;
typedef LispObject List;        /* Type `List' means proper list. */
typedef LispObject Values;      /* Multiple values return */

typedef Atom Boolean;
typedef Atom Character;
/* Number hiearachy */
typedef Atom Number;
typedef Number Real;
typedef Real Float;
typedef Real Rational;
typedef Float DoubleFloat;
typedef Float LongFloat;
typedef Float ShortFloat;
typedef Float SingleFloat;
typedef Rational Integer;
typedef Rational Ratio;
typedef Integer Bignum;
typedef Integer Fixnum;

typedef Atom Function;
typedef Atom HashTable;
typedef Atom Package;
typedef Atom Stream;
typedef Atom String;
typedef Atom Symbol;
typedef Atom Vector;
typedef LispObject (*primitive_t)(Frame, Environment, Environment, Environment, BlockEnvironment, GoEnvironment);
/* symbol.c */
extern Symbol lt_nil;           /* NIL */
extern Symbol lt_t;             /* T */
/* liutcl.c */
extern jmp_buf toplevel;
/* package.c */
extern Package pkg_cl;
extern Package pkg_kw;
extern Package pkg_lt;
extern Package package;
extern hash_table_t packages;
/* function.c */
extern Arity req1;
extern Arity req1opt1;
extern Arity req1opt2;
extern Arity req1opt4;
extern Arity req1rest;
extern Arity req2;
extern Arity req2opt1;
extern Arity rest;
extern Arity make_list_a;
extern Arity make_string_a;
/* stream.c */
extern Stream standard_error;
extern Stream standard_input;
extern Stream standard_output;
/* eval_sexp.c */
extern LispObject return_value;
/* atom.c */
extern LispObject gunbound;     /* 表示未绑定，用is_unbound函数进行判断。 */

#endif
