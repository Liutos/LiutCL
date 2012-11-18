/*
 * number.h
 *
 *
 *
 * Copyright (C) 2012-11-12 liutos <mat.liutos@gmail.com>
 */
#ifndef NUMBER_H
#define NUMBER_H

#include <gmp.h>

#include "types.h"

#define AHEAD(fn_name) Number fn_name(Number, Number)

extern AHEAD(bignum_add);
extern AHEAD(bignum_div);
extern AHEAD(bignum_mul);
extern AHEAD(bignum_sub);
extern AHEAD(fixnum_add);
extern AHEAD(fixnum_div);
extern AHEAD(fixnum_mul);
extern AHEAD(fixnum_sub);
extern BOOL numeric_eq(Number, Number);
extern Bignum fixnum2bignum(Fixnum);
extern Bignum make_bignum(mpz_t);
extern Fixnum make_fixnum(int);
extern Ratio make_ratio(ratio_t);

#endif
