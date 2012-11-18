/*
 * arit.c
 *
 * Arithmetic implementation on several kinds of number.
 *
 * Copyright (C) 2012-11-04 liutos <mat.liutos@gmail.com>
 */
#include "atom.h"
#include "number.h"
#include "pdecls.h"
#include "types.h"

/* Arithmetic operations */
/* Addition */
Integer integer_add(Integer n, Integer m)
{
    if (FIXNUM_P(n) && FIXNUM_P(m))
        return fixnum_add(n, m);
    if (BIGNUM_P(n) && BIGNUM_P(m))
        return bignum_add(n, m);
    if (FIXNUM_P(n))
        return bignum_add(fixnum2bignum(n), m);
    else
        return bignum_add(n, fixnum2bignum(m));
}

PHEAD(lt_add)
{
    /* int n1, n2; */

    /* n1 = theFIXNUM(ARG1); */
    /* n2 = theFIXNUM(ARG2); */
    /* RETURN(TO_FIXNUM(n1 + n2)); */
    RETURN(integer_add(ARG1, ARG2));
}

/* Subtraction */
Integer integer_sub(Integer n, Integer m)
{
    if (FIXNUM_P(n) && FIXNUM_P(m))
        return fixnum_sub(n, m);
    if (BIGNUM_P(n) && BIGNUM_P(m))
        return bignum_sub(n, m);
    if (FIXNUM_P(n))
        return bignum_sub(fixnum2bignum(n), m);
    else
        return bignum_sub(n, fixnum2bignum(m));
}

PHEAD(lt_sub)
{
    /* int n1, n2; */

    /* n1 = theFIXNUM(ARG1); */
    /* n2 = theFIXNUM(ARG2); */
    /* RETURN(TO_FIXNUM(n1 - n2)); */
    RETURN(integer_sub(ARG1, ARG2));
}

/* Multiplication */
Integer integer_mul(Integer n, Integer m)
{
    if (FIXNUM_P(n) && FIXNUM_P(m))
        return fixnum_mul(n, m);
    if (BIGNUM_P(n) && BIGNUM_P(m))
        return bignum_mul(n, m);
    if (FIXNUM_P(n))
        return bignum_mul(fixnum2bignum(n), m);
    else
        return bignum_mul(n, fixnum2bignum(m));
}

PHEAD(lt_mul)
{
    /* int n1, n2; */

    /* n1 = theFIXNUM(ARG1); */
    /* n2 = theFIXNUM(ARG2); */
    /* RETURN(TO_FIXNUM(n1 * n2)); */
    RETURN(integer_mul(ARG1, ARG2));
}

/* Division */
Integer integer_div(Integer n, Integer m)
{
    if (FIXNUM_P(n) && FIXNUM_P(m))
        return fixnum_div(n, m);
    if (BIGNUM_P(n) && BIGNUM_P(m))
        return bignum_div(n, m);
    if (FIXNUM_P(n))
        return bignum_div(fixnum2bignum(n), m);
    else
        return bignum_div(n, fixnum2bignum(m));
}

PHEAD(lt_div)
{
    /* int n1, n2; */

    /* n1 = theFIXNUM(ARG1); */
    /* n2 = theFIXNUM(ARG2); */
    /* RETURN(TO_FIXNUM(n1 / n2)); */
    RETURN(integer_div(ARG1, ARG2));
}

/* Comparison operations */
PHEAD(lt_numeric_eq)
{
    /* RETURN(integer_eq(ARG1, ARG2)? lt_t: lt_nil); */
    RETURN(numeric_eq(ARG1, ARG2)? lt_t: lt_nil);
}

BOOL bignum_gt(Bignum n, Bignum m)
{
    return mpz_cmp(theBIGNUM(n), theBIGNUM(m)) > 0;
}

BOOL fixnum_gt(Fixnum n, Fixnum m)
{
    return theFIXNUM(n) > theFIXNUM(m);
}

BOOL integer_gt(Integer n, Integer m)
{
    if (FIXNUM_P(n) && FIXNUM_P(m))
        return fixnum_gt(n, m);
    if (BIGNUM_P(n) && BIGNUM_P(m))
        return bignum_gt(n, m);
    if (FIXNUM_P(n))
        return bignum_gt(fixnum2bignum(n), m);
    else
        return bignum_gt(n, fixnum2bignum(m));
}

PHEAD(lt_gt)
{
    /* int n1, n2; */

    /* n1 = theFIXNUM(ARG1); */
    /* n2 = theFIXNUM(ARG2); */
    /* RETURN(n1 > n2 ? lt_t: lt_nil); */
    RETURN(integer_gt(ARG1, ARG2)? lt_t: lt_nil);
}

void init_arit(Environment env)
{
    freg("+", pkg_cl, lt_add, req2);
    freg("*", pkg_cl, lt_mul, req2);
    freg(">", pkg_cl, lt_gt, req2);
    freg("-", pkg_cl, lt_sub, req2);
    freg("/", pkg_cl, lt_div, req2);
    freg("=", pkg_cl, lt_numeric_eq, req2);
}
