/*
 * number.c
 *
 * Creation, convertion and arithmetic operations on all kinds of number.
 * 
 * Constructors
 * Convertors
 * Comparators
 * Arithmetic operations
 * Utils
 *
 * Copyright (C) 2012-11-12 liutos <mat.liutos@gmail.com>
 */
#include <gmp.h>
#include <malloc.h>

#include "object.h"
#include "types.h"

#define DEFARIT(fn_name, type) type fn_name(type n, type m)

int gcd(int, int);

/* Constructors */
Bignum make_bignum(mpz_t integer)
{
    Bignum bn;

    bn = make_object();
    bn->type = BIGNUM;
    *theBIGNUM(bn) = *integer;

    return bn;
}

/* DoubleFloat make_double_float(double C_double) */
/* { */
/*     DoubleFloat d; */

/*     d = make_object(); */
/*     d->type = DOUBLE_FLOAT; */
/*     theDOUBLE_FLOAT(d) = C_double; */

/*     return d; */
/* } */

Fixnum make_fixnum(int C_integer)
{
    return TO_FIXNUM(C_integer);
}

Ratio make_ratio(ratio_t ratio)
{
    Ratio r;

    r = make_object();
    r->type = RATIO;
    theRATIO(r) = ratio;

    return r;
}

/* SingleFloat make_single_float(float C_float) */
/* { */
/*     SingleFloat f; */

/*     f = make_object(); */
/*     f->type = SINGLE_FLOAT; */
/*     theSINGLE_FLOAT(f) = C_float; */

/*     return f; */
/* } */

/* Convertors */
Bignum fixnum2bignum(Fixnum number)
{
    mpz_t n;

    mpz_init(n);
    mpz_set_si(n, theFIXNUM(number));

    return make_bignum(n);
}

/* DoubleFloat fixnum2double_float(Fixnum number) */
/* { */
/*     return make_double_float(theFIXNUM(number)); */
/* } */

/* SingleFloat fixnum2single_float(Fixnum number) */
/* { */
/*     return make_single_float(theFIXNUM(number)); */
/* } */

/* Comparators */
BOOL fixnum_eq(Fixnum n, Fixnum m)
{
    return theFIXNUM(n) > theFIXNUM(m);
}

BOOL bignum_eq(Bignum n, Bignum m)
{
    return mpz_cmp(theBIGNUM(n), theBIGNUM(m)) == 0;
}

BOOL integer_eq(Integer n, Integer m)
{
    if (FIXNUM_P(n) && FIXNUM_P(m))
        return fixnum_eq(n, m);
    if (BIGNUM_P(n) && BIGNUM_P(m))
        return bignum_eq(n, m);
    if (FIXNUM_P(n))
        return bignum_eq(fixnum2bignum(n), m);
    else
        return bignum_eq(n, fixnum2bignum(m));
}

BOOL numeric_eq(Number n, Number m)
{
    if (INTEGER_P(n) && INTEGER_P(m))
        return integer_eq(n, m);

    return FALSE;
}

/* Arithmetic operations */
/* Addition */
DEFARIT(bignum_add, Bignum)
{
    mpz_t result;

    mpz_init(result);
    mpz_add(result, theBIGNUM(n), theBIGNUM(m));

    return make_bignum(result);
}

DEFARIT(fixnum_add, Fixnum)
{
    return make_fixnum(theFIXNUM(n) + theFIXNUM(m));
}

/* Division */
Rational bignum_div(Bignum _n, Bignum _m)
{
    mpz_t n, m, tmp;

    *n = *theBIGNUM(_n);
    *m = *theBIGNUM(_m);
    mpz_init(tmp);
    mpz_mod(tmp, n, m);
    /* Divided evenly */
    if (mpz_sgn(tmp) == 0) {
        mpz_cdiv_q(tmp, n, m);

        return make_bignum(tmp);
    } else {
        mpz_t q;
        ratio_t r;

        mpz_gcd(tmp, n, m);
        r = malloc(sizeof(struct ratio_t));
        mpz_cdiv_q(q, n, tmp);
        r->numerator = make_bignum(q);
        mpz_cdiv_q(q, m, tmp);
        r->denominator = make_bignum(q);

        return make_ratio(r);
    }
}

Fixnum fixnum_div(Fixnum n, Fixnum m)
{
    return make_fixnum(theFIXNUM(n) / theFIXNUM(m));
}
/* Rational fixnum_div(Fixnum _n, Fixnum _m) */
/* { */
/*     int n, m; */

/*     n = theFIXNUM(_n); */
/*     m = theFIXNUM(_m); */
/*     if (0 == n % m) */
/*         return make_fixnum(theFIXNUM(n) / theFIXNUM(m)); */
/*     else { */
/*         int d; */
/*         ratio_t r; */

/*         r = malloc(sizeof(struct ratio_t)); */
/*         d = gcd(n, m); */
/*         r->numerator = make_fixnum(n / d); */
/*         r->denominator = make_fixnum(m / d); */

/*         return make_ratio(r); */
/*     } */
/* } */

/* Multiplication */
DEFARIT(bignum_mul, Bignum)
{
    mpz_t result;

    mpz_init(result);
    mpz_mul(result, theBIGNUM(n), theBIGNUM(m));

    return make_bignum(result);
}

DEFARIT(fixnum_mul, Fixnum)
{
    return make_fixnum(theFIXNUM(n) * theFIXNUM(m));
}

/* Subtraction */
DEFARIT(bignum_sub, Bignum)
{
    mpz_t result;

    mpz_init(result);
    mpz_sub(result, theBIGNUM(n), theBIGNUM(m));

    return make_bignum(result);
}

DEFARIT(fixnum_sub, Fixnum)
{
    return make_fixnum(theFIXNUM(n) - theFIXNUM(m));
}

/* Utils */
/* GCD */
int gcd(int n, int m)
{
    if (0 == n)
        return m;
    else
        return gcd(m, n % m);
}

Fixnum fixnum_gcd(Fixnum _n, Fixnum _m)
{
    int n, m;

    n = theFIXNUM(_n);
    m = theFIXNUM(_m);

    return make_fixnum(gcd(n, m));
}
